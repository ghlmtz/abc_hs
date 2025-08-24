# Revision history for abc

## 0.8.0.0 -- 2025-08-23

* Chapter 8 complete, I added switch but I noticed there's no support for more advanced expression in cases (like 2 + 3, 1 << 2, etc.) in the unit tests. Looking through the book they don't seem to come in later either, I guess this is something you'd have to introduce on your own.

## 0.7.0.0 -- 2025-08-23

* Chapter 7 done. I have a Reader + State in the semantic analysis for variables, probably a cleaner way of doing that. Also converted variable mappings to Data.Map.

## 0.6.0.0 -- 2025-08-22

* Chapter 6 done, with goto. Defintely turing complete, if annoying to write anything useful.

## 0.5.0.0 -- 2025-08-22

* Chapter 5 done including extra credit. Will need to think about how to reference lvalues in the future, assignment/prefix is hardcoded to Var for now. I assume the book will get into that later.

## 0.4.0.0 -- 2025-08-21

* Chapter 4 tests pass and I got chapter 3 extra credit in. Given the way the AST/IR is generated, I didn't need to generate single-byte registers for the set commands. I'd like to do that once the test suite really starts depending on it.

## 0.3.0.0 -- 2025-08-21

* Chapter 3 tests pass, no extra credit yet. Would also like to clean up code.

## 0.2.0.0 -- 2025-08-21

* Chapter 2 done, with decent amounts of refactoring done as well.

## 0.1.0.0 -- 2025-08-19

* Chapter 1 is done! Still really hacky and code needs cleanup/refactoring but it works.
