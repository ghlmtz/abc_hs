# A Haskell-coded C compiler based on "Writing a C Compiler" by Nora Sandler

A programming project to pass the time.

## Known omissions

- Constant expressions beyond a bare number are not accepted in case statements, nor static variable initializers. (Update: unary negation in cases should work now)