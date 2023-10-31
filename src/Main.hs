{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Main where

import Data.List (nub, elemIndex)

main :: IO ()
main = do
  return ()

type ScopeID = String

data Var
  = X Int
  | Aux String
  | Scope ScopeID Var
   deriving (Eq, Show)

type CNF = [Clause]
type Clause = [Literal]
type Literal = (Bool, Var)

cnfToDIMACSnum :: CNF -> [[Int]]
cnfToDIMACSnum cnf =
  let vars = nub $ concatMap (map snd) cnf
  in  flip map cnf \clause -> 
        flip map clause \(b, v) -> case elemIndex v vars of
          Just i -> (if b then 1 else -1) * i + 1
          Nothing -> error $ "literalToDIMACSnum: variable " ++ show v ++ " not found"
