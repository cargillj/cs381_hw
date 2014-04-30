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
semCmd (LD i)	(x:xs)	= xs ++ i
semCmd (ADD c)	(x:xs)	= 
semCmd (MULT c)	(x:xs)	=
semCmd (DUP c)	(x:xs)	=

--test program
p :: prog
p = [LD 3, DUP, ADD, DUP, MULT]

--Exercise 2. Extending the Stack Language by Macros
--(a)
--(b)
--(c)

--Exercise 3. Mini Logo
