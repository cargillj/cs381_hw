--	HW2: Semantics
--	CS 381 Spring 2014
--	Authors: Aaron Egger, John Cargill

module Hw2 where

--Exercise 1. A Stack Language

type Prog = [Cmd]

data Cmd = LD Int
		 | ADD 
		 | MULT
		 | DUP
		 deriving Show

type Stack = [Int]

--performs a trnsformation on the stack
type D = Stack -> Stack

sem :: Prog -> D
sem []		c = c
sem (o:os) 	c = sem os (semCmd o c)

--not sure how to remove things from stack
--still need error checking
semCmd :: Cmd -> D
semCmd (LD i) 	xs 	= (i:xs)
semCmd ADD		xs 	= ((last xs) + ((last . init) xs)):(drop 2 xs)
semCmd MULT		xs 	= ((last xs) * ((last . init) xs)):(drop 2 xs)
semCmd DUP		xs 	= (last xs):xs

eval :: Prog -> Stack
eval p = sem p ([])

--test program
test1 = [LD 3, DUP, ADD, DUP, MULT]
test2 = [LD 3, ADD]
test3 = []

--Exercise 2. Extending the Stack Language by Macros
--(a)
data Cmd2 = LD2 Int
		  | ADD2
		  | MULT2
		  | DUP2
		  | DEF String [Cmd2]
		  | CALL String
		  deriving Show

--(b)
--(c)

--Exercise 3. Mini Logo

data Cmd3 = Pen Mode
		  | MoveTo Int Int
		  | Seq Cmd3 Cmd3
		  deriving Show

data Mode = Up | Dow
			deriving (Show, Eq) 

type State3 = (Mode, Int, Int)

type Line = (Int, Int, Int, Int)
type Lines = [Line]

semS :: Cmd3 -> State3 -> (State3,Lines)
semS (Pen m) (mode, x, y) = ((m, x, y), [])
semS (MoveTo x y) (mode, x', y') = ((mode, x, y), [(x', y', x, y)])

sem' :: Cmd3 -> Lines
sem' c = snd (semS c (Up, 0, 0))