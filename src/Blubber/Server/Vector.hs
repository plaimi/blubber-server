{-# LANGUAGE DeriveGeneric #-}

{- |
Module      :  $Header$
Description :  The vector type and its operations.
Copyright   :  (c) plaimi 2015
License     :  AGPL-3

Maintainer  :  blubber@plaimi.net
-} module Blubber.Server.Vector where

import Data.Serialize
  (
  Serialize,
  )
import GHC.Generics
  (
  Generic,
  )

-- A simple vector type with an x and a y.
data Vector = Vec
            {vx :: Double -- ^ The x position.
            ,vy :: Double -- ^ The y position.
            } deriving (Eq, Generic, Show)

-- | 'Vector' uses a 'Serialize' instance to encode its data.
instance Serialize Vector

infixl 6 ^-^
(^-^) :: Vector -> Vector -> Vector
-- | 'Vector' subtraction.
(Vec x y) ^-^ (Vec x' y') = Vec (x - x') (y - y')

infixl 6 ^+^
-- | 'Vector' addition.
(^+^) :: Vector -> Vector -> Vector
(Vec x y) ^+^ (Vec x' y') = Vec (x + x') (y + y')

infixl 7 ^*^
-- | Multiply a 'Vector' with a scalar.
(^*^) :: Vector -> Double -> Vector
(Vec x y) ^*^ n = Vec (n * x) (n * y)

infixl 7 ^/^
-- | Divide a 'Vector' by a scalar.
(^/^) :: Vector -> Double -> Vector
(Vec x y) ^/^ n = Vec (n / x) (n / y)

magVec :: Vector -> Double
-- | Calculate the magnitude of a 'Vector'.
magVec (Vec x y) = sqrt (x * x + y * y)

vecLimitMag :: Double -> Vector -> Vector
-- | Limit the magnitude of a 'Vector' to a scalar.
vecLimitMag m v | magVec v > m = v ^*^ (m / magVec v)
                | otherwise    = v
