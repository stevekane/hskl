module Geometry.Cuboid (area, volume) where

area :: Float -> Float -> Float -> Float
area a b c = (a * b * 2) + (a * b * 2) + (b * c * 2)

volume :: Float -> Float -> Float -> Float
volume a b c = a * b * c
