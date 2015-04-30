
-- |
-- Module      : Reddit.TheButton.Utility
-- Copyright   : (c) Romain Ruetschi, 2015
-- Maintainer  : romain.ruetschi@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Reddit.TheButton.Utility
  ( secToColor
  , ButtonColor(..)
  , mapChan
  ) where

import Control.Monad           (forever)
import Control.Concurrent      (forkIO)
import Control.Concurrent.Chan

data ButtonColor
  = Exclamation
  | Red
  | Orange
  | Yellow
  | Green
  | Blue
  | Purple
  | Question
  deriving (Eq)

instance Show ButtonColor where
  show Exclamation = "exclamation"
  show Red         = "red"
  show Orange      = "orange"
  show Yellow      = "yellow"
  show Green       = "green"
  show Blue        = "blue"
  show Purple      = "purple"
  show Question    = "question"

secToColor :: Float -> ButtonColor
secToColor s | s <= 0.00 = Exclamation
secToColor s | s <= 10.0 = Red
secToColor s | s <= 20.0 = Orange
secToColor s | s <= 30.0 = Yellow
secToColor s | s <= 40.0 = Green
secToColor s | s <= 50.0 = Blue
secToColor s | s <= 60.0 = Purple
secToColor _             = Question

mapChan :: (a -> b) -> Chan a -> IO (Chan b)
mapChan f chan = do
  input <- dupChan chan
  output <- newChan
  forkIO $ forever $ do
    val <- readChan input
    writeChan output (f val)
  return output
