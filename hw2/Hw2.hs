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

type State2 = (Macros, Stack)

type Macros = [(String, Prog2)]

type Prog2 = [Cmd2]

type D2 = State2 -> State2

sem2 :: Prog2 -> D2
sem2 []		c = c
sem2 (o:os) c = sem2 os (semCmd2 o c)

semCmd2 :: Cmd2 -> D2
semCmd2 (LD2 i)	(macros, xs) 	= (macros, i:xs)
semCmd2 ADD2 	(macros, xs)	= (macros, ((last xs) + ((last . init) xs)):(drop 2 xs))
semCmd2 MULT2 	(macros, xs)	= (macros, ((last xs) * ((last . init) xs)):(drop 2 xs))
semCmd2 DUP2 	(macros, xs)	= (macros, (last xs):xs)
semCmd2 (DEF macro_name cmd_list) 	(macros, xs)	= (([(macro_name, cmd_list)] ++ macros), xs)
semCmd2 (CALL macro_name) 			(macros, xs)	| macro_name == "ADD2" = (macros, snd (semCmd2 ADD2 (macros, xs)))
													| macro_name == "MULT2" = (macros, snd (semCmd2 MULT2 (macros, xs)))
													| macro_name == "DUP2" = (macros, snd (semCmd2 DUP2 (macros, xs)))

eval2 :: Prog2 -> State2
eval2 p = sem2 p ([], [])

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
