{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main
  ( main
  ) where

import           Data.Aeson
import           Wuss                (runSecureClient)
import           Control.Monad       (forever, unless, mzero)
import           Control.Monad.Trans (liftIO)
import           Network.Socket      (withSocketsDo)
import           System.IO           (hSetBuffering, stdout, BufferMode(..))
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import qualified Network.WebSockets  as WS

data BType = BTypeTicking

data BTopLevel a =
  BTopLevel
  { bType :: Text
  , bPayload :: BResponse a }

instance FromJSON (BTopLevel BTypeTicking) where
  parseJSON (Object v) = BTopLevel <$>
                         v .: "type" <*>
                         v .: "payload"

data BResponse (bType :: BType) where
  BTicking :: { participantsNum :: Text
              , tickMAC :: Text
              , secondsLeft :: Float
              , nowStr :: Text
              } -> BResponse BTypeTicking

deriving instance Show (BResponse a)

instance FromJSON (BResponse BTypeTicking) where
  parseJSON (Object v) = BTicking <$>
                         v .: "participants_text" <*>
                         v .: "tick_mac" <*>
                         v .: "seconds_left" <*>
                         v .: "now_str"
  parseJSON _ = mzero

app :: WS.ClientApp ()
app conn = do
  hSetBuffering stdout LineBuffering
  forever $ do
    jsonData <- WS.receiveData conn
    let Just msg = decode jsonData :: Maybe (BTopLevel BTypeTicking)
    let payload = bPayload msg
    let secLeft = secondsLeft payload
    let color = colorFromSec secLeft
    liftIO $ putStrLn color
  WS.sendClose conn ("Bye!" :: Text)

domain :: String
domain = "wss.redditmedia.com"

path :: String
path = "/thebutton?h=65388673cc25c617cfeabc44b24116458e77b668&e=1429625125"

main :: IO ()
main = withSocketsDo $ runSecureClient domain 443 path app

colorFromSec :: Float -> String
colorFromSec s | s <= 0.0  = "exclamation"
colorFromSec s | s <= 10.0 = "red"
colorFromSec s | s <= 20.0 = "orange"
colorFromSec s | s <= 30.0 = "yellow"
colorFromSec s | s <= 40.0 = "green"
colorFromSec s | s <= 50.0 = "blue"
colorFromSec s | s <= 60.0 = "purple"
colorFromSec _             = "question"

