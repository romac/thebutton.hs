
module Main where

import Reddit.TheButton.Connection
import Reddit.TheButton.Types
import Reddit.TheButton.AnyBar
import Reddit.TheButton.Utility

import Control.Monad           (forever)
import Control.Concurrent      (forkIO)
import Control.Concurrent.Chan (readChan, dupChan)

import System.IO (hSetBuffering, BufferMode(..), stdout)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  chan1 <- getTicks
  chan2 <- mapChan btnColor chan1
  forkIO $ abUpdateFromChan "1738" chan2
  forever $ do
    tick <- readChan chan1
    putStrLn (btnColor tick)
  where
    btnColor = show . secToColor . bSecondsLeft

