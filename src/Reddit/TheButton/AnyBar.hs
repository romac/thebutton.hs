{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Reddit.TheButton.AnyBar
-- Copyright   : (c) Romain Ruetschi, 2015
-- Maintainer  : romain.ruetschi@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Reddit.TheButton.AnyBar
  ( abGetSocket
  , abUpdate
  , abUpdateFromChan
  ) where

import Network.Socket
import Control.Exception       (bracket)
import Control.Monad           (forever)
import Control.Concurrent.Chan (readChan, Chan)

import Reddit.TheButton.Types

abGetSocket :: ServiceName -> IO Socket
abGetSocket port = do
  (serverAddr:_) <- getAddrInfo Nothing (Just "127.0.0.1") (Just port)
  sock <- socket (addrFamily serverAddr) Datagram defaultProtocol
  connect sock (addrAddress serverAddr)
  return sock

abUpdate :: Socket -> String -> IO Int
abUpdate = send

abUpdateFromChan :: ServiceName -> Chan String -> IO ()
abUpdateFromChan port chan = bracket (abGetSocket port) sClose handler
  where handler sock = forever (readChan chan >>= abUpdate sock)
