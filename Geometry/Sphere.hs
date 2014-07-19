module Geometry.Sphere (area, volume) where

area :: Float -> Float
area r = 4 * pi * (r^2)

volume :: Float -> Float
volume r = (4.0 / 3.0) * pi * (r^3)
