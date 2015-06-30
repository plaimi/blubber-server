{-# LANGUAGE DeriveGeneric #-}

{- |
Module      :  $Header$
Description :  The blubber world.
Copyright   :  (c) plaimi 2015
License     :  AGPL-3

Maintainer  :  blubber@plaimi.net
-} module Blubber.Server.World where

import Data.Functor
  (
  (<$>),
  )
import qualified Data.List as L
import Data.Maybe
  (
  catMaybes,
  )
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import System.Random
  (
  StdGen,
  randomR,
  )

import Blubber.Server.Entity
  (
  Entity (MkEntity),
  Blub (NeutralBlub, PlayerBlub),
  entity,
  intersect,
  isNeutral,
  fite,
  mass,
  position,
  targetPos,
  velocity,
  )
import Blubber.Server.Vector
  (
  Vector (Vec),
  (^+^),
  (^-^),
  (^*^),
  (^/^),
  magVec,
  vecLimitMag,
  )

-- | The 'World' in which the 'Entity's reside.
data World = MkWorld
           {width    :: Double
           -- ^ The width of the 'World'.
           ,height   :: Double
           -- ^ The height of the 'World'.
           ,players  :: Map String Entity
           -- ^ The 'PlayerBlob's in the 'World' indexed on their names.
           ,neutrals :: Set Entity
           -- ^ The 'NeutralBlob's in the 'World'.
           ,entropy  :: StdGen
           -- ^ The 'entropy' of the 'World', for randomising things.
           } deriving (Show)

addNeutral :: World -> World
-- | Try to add a 'NeutralBlub' to the 'World', if there is room for it. But
--   don't try *very* hard.
addNeutral w | S.size (neutrals w) < 32
            && available w b = w' {neutrals = S.insert b (neutrals w)}
             | otherwise     = w'
  where b                = MkEntity {position = Vec px py
                                    ,mass     = m
                                    ,entity   = NeutralBlub
                                    }
        (m,  entropy')   = randomR (1.0, 5.0) $ entropy w
        (px, entropy'')  = randomR (0.0, width w) entropy'
        (py, entropy''') = randomR (0.0, height w) entropy''
        w'               = w {entropy = entropy'''}

addPlayer :: World -> String -> World
-- | Add a 'PlayerBlub' belonging to the name in the passed in 'String', lest
--   it already has a 'PlayerBlub', in which case it's just ignored.
addPlayer w s | s `M.member` players w
             /= True      = if available w b
                               then w' {players = M.insert s b (players w)}
                               else addPlayer w' s
              | otherwise = w
  where
    b         = MkEntity {position = Vec px py
                         ,mass     = 10.0
                         ,entity   = PlayerBlub {velocity  = Vec 0 0
                                                ,targetPos = Vec 0 0
                                                }
                         }
    (px, e')  = randomR (0.0, width w) $ entropy w
    (py, e'') = randomR (0.0, height w) e'
    w'        = w {entropy = e''}

delPlayer :: World -> String -> World
-- | Delete a 'PlayerBlub' belonging to the name in the passed in 'String',
--   lest it doesn't exist, in which case it's just ignored.
delPlayer w p = w {players = M.delete p (players w)}

available :: World -> Entity -> Bool
-- | For use with 'Entity's that are not yet placed in the 'World', in order
--   to check that the spot you are attempting to place them in is actually
--   available -- i.e. that it doesn't intersect with already placed out
--   'Entity's
available w b = null . catMaybes $ intersect b
            <$> (S.elems (neutrals w) ++ M.elems (players w))

clamp :: Ord a => a -> a -> a -> a
-- | 'clamp' stuff between a min and a max.
clamp l h = max l . min h

createWorld :: StdGen -> World
-- | The initial 'World'.
createWorld s = MkWorld
              {width    = 160
              ,height   = 90
              ,players  = M.empty
              ,neutrals = S.empty
              ,entropy  = s
              }

updateVel :: Double -> Entity -> Entity
-- | Update the 'velocity' of an 'Entity' based on its 'targetPos', and
--   limited by its 'mass'.
updateVel dt e@(MkEntity {entity = b, mass = m})
  | isNeutral e = e
  | otherwise   = e {entity = b {velocity = velocity b ^+^ a ^*^ dt}}
  where a        | magVec dv <= 0.01 = Vec 0 0
                 | otherwise         = vecLimitMag (magVec dv / dt)
                                                 $ dv ^/^ (magVec dv * m)
        dv       = tv ^-^ velocity b
        tv       = vecLimitMag maxSpeed $ targetPos b ^*^ (maxSpeed / m)
        maxSpeed = 64 / (1 + log m)

updatePos :: Double -> World -> Entity -> Entity
-- | Update the 'position of an 'Entity' based on its 'velocity'. Makes sure
--   to 'clamp' the 'Entity' to the 'World' edges.
updatePos dt w e | isNeutral e = e
                 | otherwise   = e {position = clampPos $ position e
                                                      ^+^ velocity (entity e)
                                                      ^*^ dt}
  where clampPos (Vec x y) = Vec (clamp 0 (width  w) x) (clamp 0 (height w) y)

decay :: Double -> Entity -> Entity
-- | Natural decaying of 'Entity' 'mass', so as to not have too huge
-- 'Entity's.
decay dt e | isNeutral e = e
           | mass e > 1  = e {mass = 1 + (mass e - 1) * 0.999 ** dt}
           | otherwise   = e

blubs :: World -> World
blubs = playerBlubbers . neutralBlubbers

playerBlubbers :: World -> World
-- | Check if any one 'Entity' collides with any one other. If they do, delete
--   them out of the recursion, and keep checking the others until every
--   'Entity' is checked with every other.
playerBlubbers w = go (M.toList (players w)) (M.toList (players w)) w
  where
    go :: [(String, Entity)] -> [(String, Entity)] -> World -> World
    go [] _ v      = v
    go _ [] v      = v
    go (a:as) bs v =
      case playerBlubs a bs of
        Nothing             -> go as bs v
        Just ((s, Just a') ,(t, Nothing)) ->
          let z = M.delete t . M.insert s a' $ players v
          in  go as (L.delete a $ M.toList z) v {players = z}
        Just ((s, Nothing) ,(t, Just b')) ->
          let z = M.delete s . M.insert t b' $ players v
          in  go as (L.delete a $ M.toList z) v {players = z}
        Just _                            -> go as bs v

neutralBlubbers :: World -> World
-- | Check if any one 'Entity' collides with any one other. If they do, delete
--   them out of the recursion, and keep checking the others until every
--   'Entity' is checked with every other.
neutralBlubbers w = go (M.toList (players w)) (S.toList (neutrals w)) w
  where
    go :: [(String, Entity)] -> [Entity] -> World -> World
    go [] _ v      = v
    go _ [] v      = v
    go (a:as) bs v =
      case neutralBlubs a bs of
        Nothing           -> go as bs v
        Just ((s, a') ,b) -> let p' = M.insert s a' $ players v
                                 n' = S.delete b    $ neutrals v
                             in  go as (L.delete b $ S.toList n')
                                       v {players  = p', neutrals = n'}

playerBlubs :: (String, Entity) -> [(String, Entity)]
          -> Maybe ((String, Maybe Entity), (String, Maybe Entity))
-- | Given an 'Entity' of a 'PlayerBlub' and its id (as a 'String'), and
--   a list of other such things, let the 'Entity' 'fite' all the ones in the
--   passed in the list.
--
-- It keeps checking until there's a collision. At that point, it figures out
-- which one 'blubber's which. The 'blubber'er grows, and the 'blubber'ee is
-- deleted. Ruthless.
--
-- If there is no collision it returns 'Nothing'. If there is one, it returns
-- a 'Just' with two tuples -- one for each 'Blub'. The survivor will be
-- a 'Just', the other one 'Nothing'.
playerBlubs _ [] = Nothing
playerBlubs (s, a) ((t, b):bs)
  | s /= t     = case fite (a, b) of
                   (Just _,  Just _)  -> playerBlubs (s, a) bs
                   (Just a', Nothing) -> Just ((s, Just a'), (t, Nothing))
                   (Nothing, Just b') -> Just ((s, Nothing), (t, Just b'))
                   -- This next one should not happen at all.
                   (Nothing, Nothing) -> Just ((s, Nothing), (t, Nothing))
  | otherwise  = playerBlubs (s, a) bs

neutralBlubs :: (String, Entity) -> [Entity]
             -> Maybe ((String, Entity), Entity)
-- | Given an 'Entity' of a 'PlayerBlub' and its id (as a 'String'), and
--   a list of 'NeturalBlub's 'Entity's, let the 'PlayerBlub' 'Entity' 'fite'
--   all the 'NeutralbBlub's in the passed in list.
--
-- It keeps checking until there's a collision. At that point, it returns the
-- now rather quiute big 'PlayerBlub' and the poor 'NeutralBlub' it has
-- blubbered. If there is no collision it just returns 'Nothing'.
neutralBlubs _ [] = Nothing
neutralBlubs (s, a) (b:bs) = case fite (a, b) of
                               (Just a', Nothing) -> Just ((s, a'), b)
                               _                  -> neutralBlubs (s, a) bs

setTargetPos :: String -> Double -> Double -> World -> Map String Entity
-- | Check if the client in the passed in 'String' is the owner of an
-- 'Entity'; if so, set its 'targetPos' to the passed in 'Double's.
setTargetPos p x y w =
  case M.lookup p (players w) of
    Just e    | isNeutral e -> players w
              | otherwise   -> M.insert p e {entity = (entity e)
                                        {targetPos = Vec x y}} (players w)
    Nothing -> players w

updateWorld :: Double -> World -> World
-- | Update the 'World'. 'updateVel', 'updatePos', 'decay', and check for
-- 'blubbers'.
updateWorld dt w =
  blubs w {players = M.map (decay dt . updatePos dt w
                                       . updateVel dt) (players w)}


handleInput :: Ord c => [(c, (Double, Double))] -> Map c String -> World
            -> World
-- | Checks if a client has a player. If it does, then update the 'targetPos'
-- of the player's 'Entity'.
handleInput [] _ w = w
handleInput ((c,(x, y)):as) cs w =
  case M.lookup c cs of
    Just p  -> handleInput as cs $ w {players = setTargetPos p x y w}
    Nothing -> handleInput as cs w
