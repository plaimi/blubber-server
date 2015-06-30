{-# LANGUAGE DeriveGeneric #-}

{- |
Module      :  $Header$
Description :  The blubber messaging format for communicating with the server.
Copyright   :  (c) plaimi 2015
License     :  AGPL-3

Maintainer  :  blubber@plaimi.net
-} module Blubber.Server.Message where

import Data.Serialize
  (
  Serialize,
  )
import GHC.Generics
  (
  Generic,
  )

import Blubber.Server.ViewPort
  (
  ViewPort,
  )

-- | The types of messages the blubber server accepts.
data Message = Connect
             -- ^ Ask to shake hands.
             | Disconnect
             -- ^ Ask to part ways.
             | AddPlayer String
             -- ^ Ask to add a new player to the world.
             | UpdateVelocity (Double, Double)
             -- ^ Ask to update the velocity of a player's 'Blub'
             | SrcPls String
             -- ^ Ask the given IP for the source code.
             deriving (Eq, Generic, Show)

-- | The types of replies the blubber server sends.
data Reply = View ViewPort
           -- ^ The latest 'ViewPort' for the client.
           | SrcLink String
           -- ^ A link to the server's source code.
           deriving (Eq, Generic, Show)

instance Serialize Reply

-- | 'Message' uses a 'Serialize' instance to encode its data.
instance Serialize Message
