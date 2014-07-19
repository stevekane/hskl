module Geometry.Cube (area, volume) where

area :: Float -> Float
area = (*6) . (^2)

volume :: Float -> Float
volume = (^3)
