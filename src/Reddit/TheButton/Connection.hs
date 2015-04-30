{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Reddit.TheButton.Connection
-- Copyright   : (c) Romain Ruetschi, 2015
-- Maintainer  : romain.ruetschi@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Reddit.TheButton.Connection
  ( webSocketClient
  , getTicks
  , runWebSocket
  ) where

import           Wuss                    (runSecureClient)
import           Data.Aeson              (decode)
import           Network.Socket          (withSocketsDo)
import           Data.Text               (Text)
import           Data.Time               (UTCTime)
import qualified Data.Text               as T
import qualified Network.WebSockets      as WS
import           Control.Monad           (forever, unless, mzero)
import           Control.Concurrent      (forkIO)
import           Control.Concurrent.Chan (newChan, writeChan, Chan)

import Reddit.TheButton.Types

getTicks :: IO (Chan ButtonInfo)
getTicks = do
  chan <- newChan
  forkIO (runWebSocket chan)
  return chan

webSocketClient :: Chan ButtonInfo -> WS.ClientApp ()
webSocketClient chan conn = do
  forever $ do
    jsonData <- WS.receiveData conn
    let res = decode jsonData :: Maybe ButtonInfo
    case res of
      Just info -> writeChan chan info
      Nothing -> return ()
  WS.sendClose conn ("Bye!" :: Text)

runWebSocket :: Chan ButtonInfo -> IO ()
runWebSocket chan = withSocketsDo $ runClient (webSocketClient chan)
  where runClient = runSecureClient domain 443 path

domain :: String
domain = "wss.redditmedia.com"

path :: String
path = "/thebutton?h=4e5cc40428c5f09b046482977555e450859832bd&e=1430433665"

