{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Interpreter where

import qualified Data.Map as Map
import Parser
import StateM

type Env = Map.Map String Constant

type M a = StateM (a, Map.Map String a)

eval :: Expression -> StateM (Map.Map String Constant) Constant
eval (Constant c) = return c
eval (Variable v) = do
  e <- get
  case Map.lookup v e of
    Just v' -> return v'
    Nothing -> error $ "Undefined variable:" ++ v
eval (Assign s e) = do
  v <- eval e
  modify (Map.insert s v)
  return v