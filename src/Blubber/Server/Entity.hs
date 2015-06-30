{-# LANGUAGE DeriveGeneric #-}

{- |
Module      :  $Header$
Description :  The Entity type and its operations.
Copyright   :  (c) plaimi 2015
License     :  AGPL-3

Maintainer  :  blubber@plaimi.net
-} module Blubber.Server.Entity where

import Data.Serialize
  (
  Serialize,
  )
import GHC.Generics
  (
  Generic,
  )

import Blubber.Server.Vector
  (
  Vector (Vec),
  (^+^),
  (^-^),
  (^*^),
  (^/^),
  magVec,
  )

-- | An 'Entity' is a 'Blub' with a position and mass.
data Entity = MkEntity
            {position :: Vector -- ^ The position of the 'Blub'.
            ,mass     :: Double -- ^ The mass of the 'Blub'.
            ,entity   :: Blub   -- ^ The Blub itself.
            } deriving (Eq, Generic, Show)

-- | 'Entity's may be ordered based on their 'mass'.
instance Ord Entity where compare e f = compare (mass e) (mass f)

-- | 'Entity' uses a 'Serialize' instance to encode its data.
instance Serialize Entity

-- | The 'Blub' is our hero. It is either a 'NeutralBlub' that may simply be
--   consumed by 'PlayerBlub', or it is indeed a 'PlayerBlub'.
--   A human-controlled 'Blub' has a velocity and a target position in
--   addition to the usual 'Entity' stuff.
data Blub = NeutralBlub
          | PlayerBlub
          {velocity  :: Vector
          ,targetPos :: Vector
          } deriving (Eq, Generic, Show)

-- | 'Blub' uses a 'Serialize' instance to encode its data.
instance Serialize Blub

radius :: Entity -> Double
-- | The 'radius' of the 'Entity'.
radius = sqrt . mass

isNeutral :: Entity -> Bool
-- | 'isNeutral' is a quick hack to check if an 'Entity' holds a 'NeutralBlub'
--   or not.
isNeutral = n . entity where n NeutralBlub = True
                             n _           = False

intersect :: Entity -> Entity -> Maybe Vector
-- | Simple circle-circle-based intersect check of 'Entity's based on their
--   'radius'.
intersect e f | radius e + radius f > magVec v = Just v
              | otherwise                      = Nothing
  where v = position e ^-^ position f

fite :: (Entity, Entity) -> (Maybe Entity, Maybe Entity)
-- | A 'fite' between the passed in 'Entity's. The bigger one eats the
--   smaller. If they are equal no one eats each other. They just hug.
fite (b, c) = case b `intersect` c of
                Just _ | b > c     -> (Just $ b `blubber` c
                                      ,Nothing)
                       | b < c     -> (Nothing
                                      ,Just $ c `blubber` b)
                       | otherwise -> (Just b, Just c)
                Nothing            -> (Just b, Just c)

blubber :: Entity -> Entity -> Entity
-- | Let the first 'Entity' 'blubber' the second 'Entity'. I.e. EAT IT!
--   Omnomnom.
blubber b c | isNeutral b = b
            | otherwise   = b
                          {mass   = mass b + mass c
                          ,entity = (entity b)
                                  {velocity = (velocity (entity b) ^*^ mass b
                                    ^+^ vc ^*^ mass c) ^/^ (mass b + mass c)}
    }
  where vc  | isNeutral c = Vec 0 0
            | otherwise   = velocity (entity c)
