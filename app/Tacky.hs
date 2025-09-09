module Tacky
(
  tack
, TackyProg(..)
, TopLevel(..)
, Instruction(..)
, UnaryOp(..)
, Value(..)
) where

import qualified Parse as P
import TypeCheck

import Control.Monad.RWS.Strict
import Data.Maybe (catMaybes, isJust)
import qualified Data.Map as M

data TackyState = TackyState {
  varCt :: Int
, labelCt :: Int
, symbols :: SymbolMap
}

type SymbolMap = M.Map String (P.Type, IdentAttr)
type TackyMonad = RWS SymbolMap [Instruction] TackyState

newtype TackyProg = TackyProg [TopLevel]
    deriving (Show)
data TopLevel = FuncDef String Bool [String] [Instruction]
              | StaticVar String Bool Integer
    deriving (Show)
data Instruction = Return Value
                 | Unary UnaryOp Value Value
                 | Binary P.BinaryOp Value Value Value
                 | Copy Value Value
                 | Jump String
                 | JZero Value String
                 | JNZero Value String
                 | Label String
                 | FunctionCall String [Value] Value
    deriving (Show)
data Value = Constant Integer | Var String
    deriving (Show)
data UnaryOp = Complement | Negate | Not
    deriving (Show)

tack :: (P.Program, M.Map String (P.Type, IdentAttr)) -> Either String (TackyProg, M.Map String (P.Type, IdentAttr))
tack (prog, syms) = Right (prog', syms)
    where prog' = combo (TackyProg (convertSyms (M.assocs syms))) $ fst $ evalRWS (scan prog) M.empty (TackyState 0 0 syms)

combo :: TackyProg -> TackyProg -> TackyProg
combo (TackyProg a) (TackyProg b) = TackyProg (b ++ a)

convertSyms :: [(String, (P.Type, IdentAttr))] -> [TopLevel]
convertSyms ((name, (_, StaticAttr (Initial i) global)):syms) = StaticVar name global i : convertSyms syms
convertSyms ((name, (_, StaticAttr Tentative global)):syms) = StaticVar name global 0 : convertSyms syms
convertSyms (_:syms) = convertSyms syms
convertSyms [] = []

scan :: P.Program -> TackyMonad TackyProg
scan (P.Program f) = TackyProg <$> mapM funcDef (filter fil f)
    where fil (P.FuncDecl _ _ _ _ x) = isJust x
          fil _ = False

funcDef :: P.Declaration -> TackyMonad TopLevel
funcDef (P.FuncDecl name params _ _ block) = do
    g <- attrGlobal name
    FuncDef name g params <$> case block of 
        Just (P.Block items) -> 
            snd <$> listen (mapM blockItem (items ++ [P.S (P.Return (P.Constant (P.ConstInt 0)))]))
        Nothing -> return []        
funcDef _ = return $ StaticVar [] False 0

attrGlobal :: String -> TackyMonad Bool
attrGlobal name = do
    val <- asks (M.lookup name)
    case val of
        Just (_, StaticAttr _ a) -> return a
        Just (_, FunAttr _ a) -> return a
        _ -> return False

blockItem :: P.BlockItem -> TackyMonad ()
blockItem (P.S s) = statement s
blockItem (P.D (P.VarDecl _ (Just P.Static) _ _)) = return ()
blockItem (P.D (P.VarDecl name _ _ (Just v))) =
    void $ expr $ P.Assignment (P.Var name) v
blockItem _ = return ()

statement :: P.Statement -> TackyMonad ()
statement (P.Return e) = tell . return . Return =<< expr e
statement (P.Expression e) = void (expr e)
statement (P.Goto lbl) = tell [Jump lbl]
statement (P.Labelled lbl stmt) = tell [Label lbl] >> statement stmt
statement (P.If e1 e2 e3) = do
    cond <- expr e1
    end <- tmpLabel "end"
    case e3 of
            Just e -> do
                elseLbl <- tmpLabel "else"
                tell [JZero cond elseLbl]
                statement e2
                tell [Jump end, Label elseLbl]
                statement e
            Nothing -> tell [JZero cond end] >> statement e2
    tell [Label end]
statement (P.Compound (P.Block items)) = mapM_ blockItem items
statement (P.Break name) = tell [Jump ("break_" ++ name)]
statement (P.Continue name) = tell [Jump ("continue_" ++ name)]
statement (P.Switch e s name cases) = switchStmt e s name cases
statement (P.DoWhile s e name) = do
    start <- tmpLabel "start"
    tell [Label start]
    statement s
    tell [Label ("continue_" ++ name)]
    cond <- expr e
    tell [JNZero cond start, Label ("break_" ++ name)]
statement (P.While e s name) = do
    let contLbl = "continue_" ++ name
    let brkLbl = "break_" ++ name
    tell [Label contLbl]
    cond <- expr e
    tell [JZero cond brkLbl]
    statement s
    tell [Jump contLbl, Label brkLbl]
statement (P.For i c p b name) = do
    let contLbl = "continue_" ++ name
    let brkLbl = "break_" ++ name
    start <- tmpLabel "start"
    initFor i
    tell [Label start]
    case c of
        Just e -> do
            v <- expr e
            tell [JZero v brkLbl]
        Nothing -> return ()
    statement b
    tell [Label contLbl]
    maybe (return ()) (void . expr) p
    tell [Jump start, Label brkLbl]
statement _ = return ()

expr :: P.Expr -> TackyMonad Value
expr (P.Assignment (P.Var v) right) = do
    rs <- expr right
    tell [Copy rs (Var v)]
    return (Var v)
expr (P.CompoundAssignment op (P.Var v) right) = do
    rs <- expr $ P.Binary op (P.Var v) right
    tell [Copy rs (Var v)]
    return (Var v)
expr (P.Unary P.PreInc e) = incDec P.Add e False
expr (P.Unary P.PostInc e) = incDec P.Add e True
expr (P.Unary P.PreDec e) = incDec P.Subtract e False
expr (P.Unary P.PostDec e) = incDec P.Subtract e True
expr (P.Unary op e) = do
    dst <- Var <$> tmpVar
    src <- expr e
    tell [Unary (operand op) src dst]
    return dst
expr (P.Binary P.LogAnd e1 e2) = do
    false <- tmpLabel "false"
    end <- tmpLabel "end"
    dst <- Var <$> tmpVar
    s1 <- expr e1
    tell [JZero s1 false]
    s2 <- expr e2
    tell [ JZero s2 false
         , Copy (Constant 1) dst
         , Jump end
         , Label false
         , Copy (Constant 0) dst
         , Label end]
    return dst
expr (P.Binary P.LogOr e1 e2) = do
    true <- tmpLabel "true"
    end <- tmpLabel "end"
    dst <- Var <$> tmpVar
    s1 <- expr e1
    tell [JNZero s1 true]
    s2 <- expr e2
    tell [ JNZero s2 true
         , Copy (Constant 0) dst
         , Jump end
         , Label true
         , Copy (Constant 1) dst
         , Label end]
    return dst
expr (P.Binary op e1 e2) = do
    dst <- Var <$> tmpVar
    s1 <- expr e1
    s2 <- expr e2
    tell [Binary op s1 s2 dst]
    return dst
expr (P.Constant n) = return (Constant n)
expr (P.FunctionCall name args) = do
    dst <- Var <$> tmpVar
    es <- mapM expr args
    tell [FunctionCall name es dst]
    return dst
expr (P.Var v) = return $ Var v
expr (P.Conditional eCond eIf eElse) = do
    e2Label <- tmpLabel "e2_"
    end <- tmpLabel "end"
    ret <- Var <$> tmpVar
    cond <- expr eCond
    tell [JZero cond e2Label]
    ifIs <- expr eIf
    tell [Copy ifIs ret, Jump end, Label e2Label]
    elseIs <- expr eElse
    tell [Copy elseIs ret, Label end]
    return ret
expr _ = error "Invalid expression!"

switchStmt :: P.Expr -> P.Statement -> [Char] -> [Maybe P.StaticInit] -> TackyMonad ()
switchStmt e s name cases = do
    cond <- expr e
    mapM_ (makeCase cond name) (catMaybes cases)
    tell [Jump (if Nothing `notElem` cases
                then "break_" ++ name
                else name ++ ".default")]
    statement s
    tell [Label ("break_" ++ name)]

makeCase :: Value -> [Char] -> P.StaticInit -> TackyMonad ()
makeCase cond name n = do
    end <- tmpLabel "end"
    dst <- Var <$> tmpVar
    let lblName = name ++ "." ++ show n
    tell [Binary P.Equal cond (Constant n) dst
         , JZero dst end, Jump lblName, Label end]

initFor :: P.ForInit -> TackyMonad ()
initFor (P.InitExpr (Just e)) = void (expr e)
initFor (P.InitExpr Nothing) = return ()
initFor (P.InitDecl d) = blockItem (P.D d)

operand :: P.UnaryOp -> UnaryOp
operand P.Complement = Complement
operand P.Negate = Negate
operand P.Not = Not
operand _ = error "Invalid unary operand!"

incDec :: P.BinaryOp -> P.Expr -> Bool -> TackyMonad Value
incDec op e post = do
    dst <- Var <$> tmpVar
    src <- expr e
    let middle = [Binary op src (Constant 1) dst, Copy dst src]
    if post then do
        ret <- Var <$> tmpVar
        tell $ Copy src ret : middle
        return ret
    else do
        tell middle
        return src

tmpVar :: TackyMonad String
tmpVar = do
    idx <- gets (show . varCt)
    modify $ \x -> x { varCt = 1 + varCt x}
    return $ "tmp." ++ idx

tmpLabel :: String -> TackyMonad String
tmpLabel name = do
    idx <- gets (show . labelCt)
    modify $ \x -> x { labelCt = 1 + labelCt x}
    return $ "label_" ++ name ++ idx