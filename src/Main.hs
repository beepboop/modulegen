{-# LANGUAGE CPP, TemplateHaskell, GADTs #-}
-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :  Andrey Shiray <shiray.and@gmail.com>
-- Stability   :  Experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main (
    main
) where

import Control.Monad (unless)
import Data.List (stripPrefix)
import System.Exit (exitFailure)
import Test.QuickCheck.All (quickCheckAll)

import System.IO.Unsafe  -- be careful!
import System.Random

-- Datatype of formulas, taken(with slight modifications) from stackowwerflow
-- --------------------

data Formula ts where
  Body   :: Term Bool                     -> Formula ()
  Forall :: Show a
         => [a] -> (Term a -> Formula as) -> Formula (a, as)

data Term t where
  Con     :: t         -> Term t
  And     :: Term Bool -> Term Bool -> Term Bool
  Or      :: Term Bool -> Term Bool -> Term Bool
  Imp      :: Term Bool -> Term Bool -> Term Bool
  Not           :: Term Bool -> Term Bool
  Smaller :: Term Int  -> Term Int  -> Term Bool
  Plus    :: Term Int  -> Term Int  -> Term Int
  Name    :: String    -> Term t    -- to facilitate pretty printing

instance Show t => Show (Term t) where
  show (Con v)       = show v
  show (Not n)       = "\\neg " ++ show n
  show (And p q)     = "(" ++ show p ++ " \\wedge " ++ show q ++ ")"
  show (Or p q)      = "(" ++ show p ++ " \\vee " ++ show q ++ ")"
  show (Imp n m)     = "(" ++ show n ++ " \\rightarrow "  ++ show m ++ ")"
  show (Smaller n m) = "(" ++ show n ++ " < "  ++ show m ++ ")"
  show (Plus n m)    = "(" ++ show n ++ " + "  ++ show m ++ ")"
  show (Name name)   = name

--instance Show (Formula ts) where
--  show = show' ["X_{" ++ show i ++ "}" | i <- [0..]]
--    where
--      show' :: [String] -> Formula ts' -> String
--      show' ns     (Body body)   = show body
--      show' (n:ns) (Forall vs p) = "\\forall " ++ n ++ "::" ++ show vs ++ ". " ++ show' ns (p (Name n))

-- simplier instance without \forall for clean print
instance Show (Formula ts) where
  show = show' ["X_{" ++ show i ++ "}" | i <- [1..]]
    where
      show' :: [String] -> Formula ts' -> String
      show' ns     (Body body)   = show body
      show' (n:ns) (Forall vs p) = show' ns (p (Name n))

-- Example formulas
-- ----------------
ex1 :: Formula ()
ex1 = Body (Con True)

ex2 :: Formula (Int, ())
ex2 = Forall [1..10] $ \n ->
        Body $ n `Smaller` (n `Plus` Con 1)

--ex3 :: Formula (Bool, (Int, ()))
ex3 = Forall [False, True] $ \a ->
      Forall [False, True] $ \b ->
      Forall [False, True] $ \c ->
        Body $ a `Or` ((Not b) `Imp` (c `And` a))


-- TODO quick and dirty
rndvar e a b c = case (e) of { (1) -> a; (2) -> b; (3) -> c; (4) -> (Not a); (5) -> (Not b); (6) -> (Not c) }
rndop e = case (e) of { (1) ->  And; (2) -> Or; (3) -> Imp;(4) ->  And; (5) -> Or; (6) -> Imp }

gen_formula [e1,e2,e3,e4,e5,e6,e7] = Forall [False, True] $ \x ->
               Forall [False, True] $ \y ->
               Forall [False, True] $ \z ->
                Body $  (rndop e2) (rndvar e1 x y z)  ((rndop e4) (rndvar e3 x y z) ((rndop e6) (rndvar e5 x y z)  (rndvar e7 x y z)))

print_formula e = (show . gen_formula) e

make_init = "\\documentclass[a4paper,10pt]{article}\n \\usepackage[pdftex]{graphicx}\n \\begin{document}"
make_finale = "\n\\end{document}"


make_table  e = "\n\\subsection{(40 points)}\n"++tables!!((sum e) `mod` l)
 where
  tables=["\\input{tab1.tex}\n","\\input{tab2.tex}\n"]
  l = length tables


make_formula  e = "\n\\subsection{(30 points)}\n Make truth tables for the following formulas:\n\\[\n" ++ (print_formula e) ++ "\n\\]\n\\[\n" ++ (print_formula $ reverse e) ++ "\n\\]\n"

make_quest  e = "\n\\subsection{(10 points)}\n" ++  questions!!((sum e) `mod` l)
 where
  questions = ["Give definition of algorithm\n", "Describe  main flowchart building blocks\n", "What are basic properties of algorithms?\n"]
  l = length questions

make_task e' = "\\section{}" ++ (make_table e')  ++  (make_formula e') ++  (make_quest e') ++ "\n\\newpage\n"


take' a b  = (drop a) . (take b)
-- Hello World
exeMain = do
    gen <- getStdGen
    let e = take (7*20)  (randomRs (1,6) gen):: [Int]
    putStrLn make_init

    mapM (\i -> (putStrLn ) $ make_task (take' i (7+i) e)) [0..19]
    putStrLn make_finale


-- This is a clunky, but portable, way to use the same Main module file
-- for both an application and for unit tests.
-- MAIN_FUNCTION is preprocessor macro set to exeMain or testMain.
-- That way we can use the same file for both an application and for tests.
#ifndef MAIN_FUNCTION
#define MAIN_FUNCTION exeMain
#endif
main = MAIN_FUNCTION

