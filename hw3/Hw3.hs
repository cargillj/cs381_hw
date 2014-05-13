--	HW3: Types
--	CS 381 Spring 2014
--	Authors: Aaron Egger, John Cargill

module Hw3 where

--Exercise 1. A Rank-Based Type Systems for the Stack Language
--(a)
type Prog = [Cmd]

data Cmd = LD Int
	 | ADD
	 | MULT
	 | DUP
	 | INC
	 | SWAP
	 | POP Int
	deriving Show

type Stack = [Int]

type D = Stack -> Stack

sem :: Prog -> D
sem []		c = c
sem (o:os)	c = sem os (semCmd o c)

semCmd :: Cmd -> D 
semCmd (LD i)   xs = (i:xs) 
semCmd ADD      xs = ((last xs) + ((last . init) xs)):(drop 2 xs) 
semCmd MULT     xs = ((last xs) * ((last . init) xs)):(drop 2 xs)
semCmd DUP      xs = (last xs):xs
semCmd INC      xs = ((last xs) + 1):(drop 1 xs)
semCmd SWAP     xs = ((last . init) xs):(last xs):(drop 2 xs)
semCmd (POP i)  xs = (drop i xs)

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
rank [] r | r >= 0 = Just r
rank (x:xs) r | under >= 0 = rank xs (under+adds)
              where (subs, adds) = rankC x
                    under = r - subs
rank _ _ = Nothing

rankP :: Prog -> Maybe Rank
rankP xs = rank xs 0

--(b)
typeSafe :: Prog -> Bool
typeSafe e = (rankP e) /= Nothing

semStatTc :: Prog -> Maybe Stack
semStatTc p | typeSafe p = Just (sem p ([]))
			| otherwise = Nothing

p1 = [LD 3, DUP, ADD, LD 5, SWAP]

--Exercise 2. Shape Language
