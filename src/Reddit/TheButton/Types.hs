{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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

data ButtonType
  = ButtonTick
  | ButtonPress
  deriving (Show, Eq)

data ButtonInfo
  = ButtonInfo
  { bType :: ButtonType
  , bParticipantsNum :: !Text
  , bTickMAC :: !Text
  , bSecondsLeft :: Float
  , bNow :: !UTCTime }
  deriving (Show, Eq)

instance FromJSON ButtonInfo where
  parseJSON = withObject "payload" $ \p ->
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

