-- this is what I want to implement in Python, but the syntax is getting in the way.

-- Syntax of specail surfaces corresponds to PHITS manual Table 5.76
data Plane = {A :: Real, B :: Real, C :: Real, D :: Real} | {axis :: "x" | "y" | "z", D :: Real}
| (Real, Real, Real) (Real, Real, Real) (Real, Real, Real)

data Sphere = {radius :: Real, center :: (Real, Real, Real)}

data Cylinder {parallelTo :: "x" | "y" | "z", }

-- in general, a surface is set by f(x,y,z) = 0. In PHITS, there are a limited selection.
data  = Real -> Real -> Real -> Real | (Real, Real, Real) -> Real |
  where
