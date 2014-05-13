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
rank (x:xs) r | total >= 0 = rank xs (total+add)
              where (sub, add) = rankC x
                    total = r - sub
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
data Shape = X
		   | TD Shape Shape
		   | LR Shape Shape
		   deriving Show

type BBox = (Int,Int)

--(a)
bbox :: Shape -> BBox
bbox X = (1,1)
bbox (TD s1 s2)
	| s1x >= s2x	= (s1x, s1y + s2y)
	| s1x < s2x		= (s2x, s1y + s2y)
	where	
		(s1x, s1y) = bbox s1 
		(s2x, s2y) = bbox s2

bbox (LR s1 s2)
	| s1y >= s2x	= (s1x + s2x, s1y)
	| s1y < s2x		= (s1x + s2x, s2y)
	where	
		(s1x, s1y) = bbox s1
		(s2x, s2y) = bbox s2

--(b)
rect :: Shape -> Maybe BBox
rect X = Just (1,1)
rect (TD s1 s2)
	| Just s1x == Just s2x = Just (s1x, s1y + s2y)
	| otherwise = Nothing
	where	
		Just (s1x, s1y) = rect s1
		Just (s2x, s2y) = rect s2
rect (LR s1 s2)
	| Just s1y == Just s2y = Just (s1x + s2x, s1y)
	| otherwise = Nothing
	where	
		Just (s1x, s1y) = rect s1
		Just (s2x, s2y) = rect s2

--Exercise 3. 
--a

--1 f and g return lists. The return types of x and y in f are the same, but they are different types in g 
--2 Since one branch of f's conditional statement returns [y], a list, x must also return a list in order
--  to not produce a type error.  This is due to Haskell being a static typing language.
--3 g can take two different types of variables, x and y, therefore it is more general
--4 g is has a more generalized input options, but they both could be the same type

--b
h xs ts = xs ++ (map snd ts)
--c
-- There is no way in Haskell to pattern match a function and its parameters at the
-- same time. Since b defined is defined only as the return type of another function
-- we do not know anything about its type and there is no simple definition for the function

--d
-- No we cannot define the function a -> b. To define this funtion we must know the type of b
-- but we have no knowledge of type b other than the funtion j returns b.
