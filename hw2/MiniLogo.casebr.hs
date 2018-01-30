--Brett Case (casebr)

import Prelude hiding (Num)
import Data.List


--1.Define the abstract syntax of MiniLogo as a set of 
--Haskell data types. You should use built-in types 
--for num, var, and macro. (If you want to define a 
--type Num, you will have to hide that name from the 
--Prelude).

type Num = Int
type Var = String
type Macro = String

type Prog = [Cmd]

data Mode = Up
          | Down
          deriving (Show, Eq)

data Expr = Thing Var
          | Number Num
          | Add Expr Expr
          deriving (Show, Eq)

data Cmd = Pen Mode
         | Move (Expr, Expr)
         | Define Macro [Var] Prog
         | Call Macro [Expr]
         deriving (Show, Eq)

--2.Define a MiniLogo macro line (x1,y1,x2,y2) that 
--(starting from anywhere on the canvas) draws a line
--segment from (x1,y1) to (x2,y2).
line :: Cmd
line = Define "line" ["x1", "y1", "x2", "y2"]
           [Pen Up, Move (Thing "x1", Thing "y1"), Pen Down, Move (Thing "x2", Thing "y2")]


 
