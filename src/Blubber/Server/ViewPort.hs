{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

{- |
Module      :  $Header$
Description :  The ViewPort type and its operations.
Copyright   :  (c) plaimi 2015
License     :  AGPL-3

Maintainer  :  blubber@plaimi.net
-} module Blubber.Server.ViewPort where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Maybe
  (
  fromJust,
  )
import Data.Serialize
  (
  Serialize,
  )
import GHC.Generics
  (
  Generic,
  )

import Blubber.Server.Entity
  (
  Entity,
  mass,
  position,
  radius,
  )
import Blubber.Server.Vector
  (
  Vector (Vec),
  (^-^),
  magVec,
  vx,
  vy,
  )
import Blubber.Server.World
  (
  World,
  neutrals,
  clamp,
  height,
  neutrals,
  players,
  width,
  )

-- | A 'ViewPort' is the part of the 'World' that a client is privy to.
data ViewPort = MkViewPort
              {viewPos         :: Vector
               -- ^ The absolute position of the 'ViewPort'
              ,viewWidth       :: Double
               -- ^ The width of the 'ViewPort'.
              ,viewHeight      :: Double
               -- ^ The height of the 'ViewPort'.
              ,visiblePlayers  :: Map String Entity
              -- ^ The visible 'PlayerBlob's in the 'ViewPort'.
              ,visibleNeutrals :: [Entity]
              -- ^ The visible 'NeutralBlob's in the 'ViewPort'.
              } deriving (Eq, Generic, Show)


-- | 'ViewPort' uses a 'Serialize' instance to encode its data.
instance Serialize ViewPort

mkViewPort :: World -> String -> ViewPort
-- | Make a 'ViewPort' for the passed in client's 'Entity'. If they don't have
--   an 'Entity', then we'll just give them the entire 'World' via
--   'worldViewPort'. The 'ViewPort' also includes the 'mass' of the player's
--   own 'Entity'.
mkViewPort w p =
  case M.lookup p (players w) of
    Just b  -> MkViewPort {viewPos         = position e
                          ,viewWidth       = wid
                          ,viewHeight      = hei
                          ,visiblePlayers  =
                            M.insert (p ++ ": " ++ show (floor (mass e))) b
                          . M.delete p $ getVisiblePlayers (players w)
                                                           (position e)
                                                           (radius e)
                          ,visibleNeutrals = S.toList
                                           $ getVisibles (neutrals w)
                                                         (position e)
                                                         (radius e)
                          }
    Nothing -> worldViewPort w
  where e   = fromJust (M.lookup p (players w)) -- Stooopid.
        wid = radius e * 16
        hei = radius e * 9

getVisiblePlayers :: Map String Entity -> Vector -> Double
                  -> Map String Entity
-- | Figure out which 'PlayerBlob's are visible to a client, based on their
--   'position' and 'radius'.
getVisiblePlayers v pos r = M.filter visibleP v
  where visibleP e = magVec (dev e ^-^ Vec (c 16 vx e) (c 9 vy e)) <= radius e
        dev        = (^-^ pos) . position
        c a d e    = clamp ((-(r * a)) / 2) (r * a / 2) (d (dev e))

getVisibles :: Set Entity -> Vector -> Double -> Set Entity
-- | Figure out which 'NeutralBlob's are visible to a client, based on their
--   'position' and 'radius'.
getVisibles v pos r = S.filter visibleP v
  where visibleP e = magVec (dev e ^-^ Vec (c 16 vx e) (c 9 vy e)) <= radius e
        dev        = (^-^ pos) . position
        c a d e    = clamp ((-(r * a)) / 2) (r * a / 2) (d (dev e))

worldViewPort :: World -> ViewPort
-- | Make a 'ViewPort' of essentieally the whole 'World' and all its
--   'Entity's.
worldViewPort w = MkViewPort
                {viewPos         = Vec 0 0
                ,viewWidth       = width w
                ,viewHeight      = height w
                ,visiblePlayers  = players w
                ,visibleNeutrals = S.elems (neutrals w)
                }

viewPort :: World -> String -> ViewPort
viewPort w p = w `mkViewPort` p
