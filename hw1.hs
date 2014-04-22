--	HW1: Syntax 
-- 	CS 381 Spring 2014
--  Authors: Aaron Egger, John Cargill

module hw1 where 

--Exercise 1. Mini Logo
--(a)
data Cmd = Pen Mode 
	 | Moveto Pos Pos 
	 | Def  Name Cmd 
	 | Call Name 
	 | SeqCmd Cmd Cmd 	  
	 deriving Show 

data Pos = Xy Number | Cord Name
	 deriving Show

data Vals = Val Number
	  | SeqVals Vals Vals
	  deriving Show

data Mode = Up | Down
	 deriving Show

data Pars = Par Name 
	  | SeqPars Pars Pars 
	  deriving Show

type Number = Int
type Name = String

--(b)
vector :: Pos -> Pos -> Pos -> Pos -> Cmd
vector x1 y1 x2 y2 = Def "vector" (SeqCmd (Pen Up) 
	             (SeqCmd (Moveto (x1) (y1)) 
	             (SeqCmd (Pen Down) (Moveto (x2) (y2)))))

--Step takes two pos  x and y which are the starting cord of the step
step :: Int -> Int -> Cmd
step x y = SeqCmd (vector (Xy x) (Xy y) (Xy x) (Xy (succ y)))
	   (vector (Xy x) (Xy (succ y)) (Xy (succ x)) (Xy (succ y)))


--(c)
--steps moves from point x y N number of times
steps :: Int -> Int -> Int -> Cmd
steps _ _ 0 =  Pen Up
steps x y n =  SeqCmd (step x y) (steps (succ x) (succ y) (n-1))


--Exercise 2. Digital Circuit Design Language 
--(a)
data Circuit = Circ Gates Links 
data Gates = Gat Int GateFn 
		| Gats Gates Gates
data GateFn = And 
	    | Or 
	    | Xor 
	    | Not 
data Links = FromTo Int Int Int Int 
	   | FromTos Links Links

--(b) 
circuit = Circ (Gats (Gat 1 Xor) (Gat 2 Xor)) (FromTos (FromTo 1 1 2 1) (FromTo 1 2 2 2))

--(c) 
instance Show Circuit where 
	 show (Circ g l) = show g++show l
 
instance Show Gates where 	
	 show (Gats g1 g2) = show g1++show g2 
	 show (Gat n fn) = show n++":"++show fn++";\n" 

instance Show GateFn where
	 show And = "and" 
	 show Or = "or" 
	 show Xor = "xor" 
	 show Not = "not" 

instance Show Links where 
	 show (FromTos l l') = show l++";\n"++show l'++";" 
	 show (FromTo n v n' v') = "from "++show n++"."++show v++" to "++show n'++"."++show v'


--Exercise 3. Designing Abstract Syntax 
data Expr = N Int
	  | Plus Expr Expr
	  | Times Expr Expr
	  | Neg Expr 
	  deriving Show 

data Op = Add | Multiply | Negate 
	deriving Show 

data Exp = Num Int 
	 | Apply Op [Exp] 
	 deriving Show
--(a)
--a Is the first abstract syntax representation of -(3+4)*7
a = Times (N 7) (Neg(Plus (N 3) (N 4)))
--b is the second abstract syntax respresentation of -(3+4)*7
b = Apply Multiply [(Num 7), (Apply Negate [(Apply Add [Num 4 , Num 3])])] 

--(b) Advantages and Disadvantages 
--The First representation is easier for me to understand and use because it only has one datatype with multiple instances of the data type. 
--Similarly the first representation is easier to use because it creates a simpler output that is in prefix notation. 
--The second representation causes the user to put in unneccisary constructor names like Apply before applying the Expression Op. 
--The second reprsentation also causes the user to to use an excess of parentheses and square brackets. 

--(c)
translate :: Expr -> Exp 
translate (N x) = (Num x)
translate (Plus x y) = Apply Add[translate x, translate y]
translate (Times x y) = Apply  Multiply[translate x, translate y] 
translate (Neg x) = Apply Negate [translate x]  
