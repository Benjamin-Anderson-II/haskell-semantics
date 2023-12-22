-- Benjamin Anderson II 934-353-159
-- Due Data: May 11, 2023

-- This code is for HW4 from CS_381 @ Oregon State University in Spring 2023
-- The intention of this assignment is to practice semantics
-- This is part 1

module Stacklang1 where

type Prog = [Cmd]

data Cmd
    = LD Int
    | ADD
    | MULT
    | DUP
    deriving Show

type Stack = [Int]

--applies program to stack
run :: Prog -> Stack -> Maybe Stack
run [] s = Just s
run (p:ps) s = case semCmd p s of
    Just a  -> run ps a
    Nothing -> Nothing

-- semantics
semCmd :: Cmd -> Stack -> Maybe Stack
semCmd a b = case (a, b) of
    ((LD n), s)   -> Just (n:s)
    (ADD, x:y:s)  -> Just (x+y:s)
    (MULT, x:y:s) -> Just (x*y:s)
    (DUP, x:s)    -> Just(x:x:s)
    otherwise     -> Nothing

--- testing variables
-- stack1 = [1,2,3,4,5]
-- test1 = [LD 3,DUP,ADD,DUP, MULT]
-- test2 = [LD 3,ADD]
-- test3 = []
-- test4 = [ADD,ADD,ADD,ADD]