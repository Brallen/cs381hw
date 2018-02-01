--Brett Case (casebr)

import Prelude hiding (Num)
import Data.List

type Num = Int
type Var = String
type Macro = String

--1.
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

--2.
--Minilogo Macro:
--define line(x1, y1, x2, y2){
--    pen up;
--    move(x1,y1);
--    pen down;
--    move(x2,y2);
--} 

line :: Cmd
line = Define "line" ["x1", "y1", "x2", "y2"]
        [Pen Up, Move (Thing "x1", Thing "y1"), Pen Down, Move (Thing "x2", Thing "y2")]

--3.
--Minilogo Macro:
--define nix(x, y, w, h){
--    line(x, y, x+w, y+h)
--    line(x, y+h, x+w, y)
--}

nix :: Cmd
nix = Define "nix" ["x", "y", "w", "h"]
        [Call "line" [Thing "x", Thing "y", Add (Thing "x") (Thing "w"), Add (Thing "y") (Thing "h")],
         Call "line" [Thing "x", Add (Thing "y") (Thing "h"), Add (Thing "x") (Thing "w"), Thing "y"]]
 
--4.
--makes steps that start at the top and work down recursively

steps :: Int -> Prog
steps 0 = []
steps x = [Call "line" [Number x, Number x, Number (x - 1), Number x], 
           Call "line" [Number (x-1), Number x, Number (x-1), Number (x-1)]]
           ++ steps (x-1)

--5.

macros :: Prog -> [Macro]
macros []     = []
macros (x:xs) = case x of
                 Define n _ _ -> n : macros xs
                 _            -> macros xs

--6.

--takes expressions and turns them into strings
prettyHelper :: Expr -> String
prettyHelper (Thing v)  = v
prettyHelper (Number n) = (show n)
prettyHelper (Add x y)  = prettyHelper x ++ " + " ++ prettyHelper y

--takes a pen mode and returns a string
prettyPen :: Mode -> String
prettyPen x = (show x)

pretty :: Prog -> String
pretty [] = ""
pretty ((Pen x):cmds) = "pen " ++ (prettyPen x) ++ ";\n" ++ (pretty cmds)
pretty ((Move (x, y)):cmds) = "move" ++ "(" ++ (prettyHelper x) ++ "," ++ (prettyHelper y) ++ ");\n" ++ (pretty cmds)
pretty (Define n v p:cmds) = "define " ++ n ++ " (" ++ intercalate "," v ++"){\n"++ (pretty p) ++"}\n" ++ (pretty cmds) 
pretty ((Call m e):cmds) = "call " ++ m ++ " (" ++ intercalate ", " (map prettyHelper e) ++ ")\n" ++ (pretty cmds)

