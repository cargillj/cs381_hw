--	HW3: Types
--	CS 381 Spring 2014
--	Authors: Aaron Egger, John Cargill

module Hw3 where

--Exercise 1. A Rank-Based Type Systems for the Stack Language
--(a)
type prog = [Cmd]

data Cmd = LD Int
	 | ADD
	 | MULT
	 | DUP
	 | INC
	 | SWAP
	 | POP Int
	deriving Show

type Stack = [Int]

type D = Maybe -> Maybe

sem :: Prog -> D
sem []		c = c
sem (o:os)	c = sem os (semCmd o c)

semCmd :: Cmd -> D 
semCmd (LD i)   xs = (i:xs) 
semCmd ADD      xs = ((last xs) + ((last . init) xs)):(drop 2 xs) 
semCmd MULT     xs = ((last xs) * ((last . init) xs)):(drop 2 xs)
semCmd DUP      xs = (last xs):xs
semCmd INC      xs = ((last xs) + 1) 
semCmd SWAP     xs = ((last . init xs):(last xs)):(drop 2 xs)
semCmd (POP i)  xs = (drop i xs)  
semCmd _	Error

type Rank = Int
type CmdRank = (Int,Int)

rankC :: Cmd -> CmdRank
rankC (LD i)  = (0, 1)
rankC ADD     = (2, 1)
rankC MULT    = (2, 1)
rankC DUP     = (1, 2)
rankC INC     = (1, 1)
rankC SWAP    = (2, 2)
rankC (POP i) = (i, 0)

rank :: Prog -> Rank -> Maybe Rank
rank [] r | r >= 0 = just r 
rank (x:xs) r | currenttotal >= 0	= rank xs (currenttotal + add)
				where	(sub, add) = rankC x
						currenttotal = r - sub
rank _ _		= Error


rankP :: Prog -> Maybe Rank
rankP xs = rank xs 0 
--(b)
--We can use the type checker to perform only safe evaluations
--
typeSafe :: Expr -> Bool
typeSafe e = (rankP e) /= Error

semStatTc :: Prog -> Maybe Stack
semStatTc p = | typesafe p = Just (sem p ([]))
			| otherwise = Error

--Exercise 2. Shape Language
