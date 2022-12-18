{- This module is intended to be imported as 'import Utils.State' -}
module Utils.State(
  {- Re-exports -}
  State,
  evalState,
  runState,
  execState,
  get,
  put,
  gets,
  state,
  {- Defined in this file -}
  module Utils.State
) where

import Control.Monad.State(State, evalState, get, state, runState, execState, gets, put)

import Data.Maybe(isJust, fromJust)

evalStarting :: State a [a] -> a -> [a]
evalStarting st x = x:(evalState st x)

repeatUntil :: ((a, s) -> Bool) -> State s a -> State s a
repeatUntil cond st =
  do
    x <- st
    s <- get
    if cond (x, s)
      then return x
      else repeatUntil cond st

repeatUntilIsJust :: State s (Maybe a) -> State s a
repeatUntilIsJust = fmap fromJust . repeatUntil (\(a, _) -> isJust a)

countUntil :: ((a, s) -> Bool) -> State s a -> State s Int
countUntil pred st = countStarting 0
  where
    countStarting n =
      do
        a <- st
        s <- get
        if pred (a, s)
          then return n
          else countStarting (n+1)
