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
		deriving show

type StackContent = Int

type D = StackContent -> StackContent

type Stack = [StackContent]

sem :: Prog -> D
sem []		c = c
sem (o:os) 	c = sem os (semCmd o c)

semCmd :: Cmd -> D

--Exercise 2. Extending the Stack Language by Macros
--(a)
--(b)
--(c)

--Exercise 3. Mini Logo
