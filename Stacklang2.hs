-- Benjamin Anderson II 934-353-159
-- Due Data: May 11, 2023

-- This code is for HW4 from CS_381 @ Oregon State University in Spring 2023
-- The intention of this assignment is to practice semantics
-- This is part 2

module Stacklang2 where

type Prog = [Cmd]
type Stack = [Either Bool Int]
data Cmd
    = LDI Int
    | LDB Bool
    | LEQ
    | ADD
    | MULT
    | DUP
    | IFELSE Prog Prog
    deriving Show

-- applies program to stack
run :: Prog -> Stack -> Maybe Stack
run [] s = Just s
run (p:ps) s = case semCmd p s of
    Just a -> run ps a
    Nothing -> Nothing

-- semantics
semCmd :: Cmd -> Stack -> Maybe Stack
semCmd a b = case (a, b) of
    ((LDI n), s)                  -> Just (Right(n):s)
    ((LDB b), s)                  -> Just (Left(b):s)
    (LEQ, (Right x):(Right y):s)  -> Just(Left(x<=y):s)
    (ADD, (Right x):(Right y):s)  -> Just(Right(x+y):s)
    (MULT, (Right x):(Right y):s) -> Just(Right(x*y):s)
    (DUP, x:s)                    -> Just(x:x:s)
    ((IFELSE p1 p2), (Left(x):s)) -> if x then
        run p1 s
    else
        run p2 s
    otherwise                     -> Nothing



--- Test Variables
-- stack1 :: Stack
-- stack1 = [Right 1, Right 3, Right 5, Right 7, Right 9]
-- stack2 :: Stack
-- stack2 = [Left True, Right 3]
-- test1 = [LDI 3, DUP, ADD, DUP, MULT]
-- test2 = [LDB True, DUP, IFELSE [LDI 1][LDI 0]]
-- test3 = [LEQ]
-- test4 = [ADD, ADD, MULT, DUP]
-- test5 = [LEQ, IFELSE [][], LDI 9]
-- test6 = [LDI 5, LDI 2, LEQ, IFELSE [LDI 10, DUP][], ADD]
-- test7 = [LDI 5, LDI 7, LEQ, IFELSE [LDI 10, DUP][LDI 20, DUP], ADD]
-- test8 = [LDI 1, LDI 2, LDI 3, LDI 4, LDI 5, ADD, ADD, ADD, ADD]