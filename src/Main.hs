{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Main where

import System.Process

import Data.List (nub, elemIndex)

import Base
import AtMostK.Binary

main :: IO ()
main = do
  return ()

cnfToDIMACS :: CNF -> IO ()
cnfToDIMACS cnf = do
  let vars = nub $ concatMap (map snd) cnf
      nss = flip map cnf \clause -> 
        flip map clause \(b, v) -> case elemIndex v vars of
          Just i -> (if b then 1 else -1) * (i + 1)
          Nothing -> error $ "literalToDIMACSnum: variable " ++ show v ++ " not found"
      ls = flip map nss \ns -> unwords $ map show ns ++ ["0"]
  -- print vars -- [debug]
  -- print nss -- [debug]
  writeFile "the.cnf" $ unlines ls
  -- > cnfToDIMACS [[(True, X 1), (False, X 2)], [(True, X 1), (True, X 3)]]
  -- > cnfToDIMACS $ binary id [X 1, X 2, X 3] 2
  -- > wsl -- ./minisat ./the.cnf

solve :: CNF -> IO ()
solve cnf = do
  cnfToDIMACS cnf
  system "wsl -- ./minisat ./the.cnf"
  return ()
  -- > solve $ binary id (map ((,) True . X) [1..3]) 2 ++ [[(True, X 1)],[(True, X 2)]]