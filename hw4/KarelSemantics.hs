-- Brett Case (casebr)
-- Noah Berks (berksn)

module KarelSemantics where

import Prelude hiding (Either(..))
import Data.Function (fix)

import KarelSyntax
import KarelState


-- | Valuation function for Test.
test :: Test -> World -> Robot -> Bool
test Empty _ r = isEmpty r
test Beeper w r = hasBeeper (getPos r) w
test (Clear d) w r = isClear (relativePos d r) w
test (Not t) w r = not (test t w r)
test (Facing c) w r = if c == getFacing r
                         then True
                         else False

-- | Valuation function for Stmt.
stmt :: Stmt -> Defs -> World -> Robot -> Result
stmt Shutdown   _ _ r = Done r
stmt PickBeeper _ w r = let p = getPos r
                        in if hasBeeper p w
                              then OK (decBeeper p w) (incBag r)
                              else Error ("No beeper to pick at: " ++ show p)
stmt Move d w r = let p = neighbor (getFacing r) (getPos r)
                  in if isClear p w
		      then OK w (setPos p r)
                      else Error ("Blocked at: " ++ show p)
stmt PutBeeper d w r = if getBag r == 0
                          then Error "No beeper to put."
			  else OK (incBeeper (getPos r) w) (decBag r)
stmt (Turn dir) d w r = OK w (setFacing (cardTurn dir (getFacing r)) r) 
stmt (Block []) _ w r = OK w r 
stmt (Block (s:sx)) d w r = case stmt s d w r of
                               OK newW newR -> stmt (Block sx) d newW newR
			       otherwise -> otherwise 
stmt (If t s1 s2) d w r = if test t w r then stmt s1 d w r
                          else stmt s2 d w r
stmt (Call m) d w r = case lookup m d of
                      Just s -> stmt s d w r
		      Nothing -> Error ("Undefined macro: " ++ m)
stmt (Iterate 0 s) d w r = OK w r
stmt (Iterate i s) d w r = case stmt s d w r of
                           OK newW newR -> stmt (Iterate (i-1) s) d newW newR
			   otherwise -> otherwise
stmt (While t s) d w r = if test t w r 
                         then case stmt s d w r of
			 	OK newW newR -> stmt (While t s) d newW newR
				otherwise -> otherwise
			 else OK w r
    
-- | Run a Karel program.
prog :: Prog -> World -> Robot -> Result
prog (m,s) w r = stmt s m w r
