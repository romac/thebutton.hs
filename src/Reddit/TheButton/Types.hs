{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- |
-- Module      : Reddit.TheButton.Types
-- Copyright   : (c) Romain Ruetschi, 2015
-- Maintainer  : romain.ruetschi@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Reddit.TheButton.Types where

import           Control.Applicative        (pure, (<$>), (<*>), (<|>))
import           Control.Monad              (mzero)
import           Data.Text                  (Text)
import           Data.Time                  (UTCTime)
import           Data.Aeson                 (FromJSON (parseJSON),
                                             ToJSON (toJSON),
                                             Value (String, Object, Bool),
                                             withObject, object,
                                             (.:), (.:?), (.=))
import          Data.Data
import          Data.Typeable

import Data.ByteString.Lazy

data ButtonType
  = ButtonTick
  | ButtonPress
  deriving (Show, Eq, Typeable, Data)

data ButtonInfo
  = ButtonInfo
  { bType :: ButtonType
  , bParticipantsNum :: !Text
  , bTickMAC :: !Text
  , bSecondsLeft :: Float
  , bNow :: !Text }
  deriving (Show, Eq, Typeable, Data)

instance FromJSON ButtonInfo where
  parseJSON (Object v) = do
    p <- v .: "payload"
    ButtonInfo ButtonTick
      <$> p .: "participants_text"
      <*> p .: "tick_mac"
      <*> p .: "seconds_left"
      <*> p .: "now_str"

instance ToJSON ButtonInfo where
  toJSON ButtonInfo{..} = object
    [ "type" .= ("ticking" :: String)
    , "participants_num" .= bParticipantsNum
    , "tick_mac" .= bTickMAC
    , "seconds_left" .= bSecondsLeft
    , "now" .= bNow ]

test :: ByteString
test = "{\"payload\": {\"participants_text\": \"852,626\", \"tick_mac\": \"c5c1594ad025c6f5aa4f31b21b6fa4f0e6f9ffcc\", \"seconds_left\": 46.0, \"now_str\": \"2015-04-29-22-41-26\"}}"

