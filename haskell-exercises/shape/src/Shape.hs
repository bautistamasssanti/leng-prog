module Shape where

data Point = Point { x::Double, y:: Double} deriving (Eq, Show)

data Circle    = Circle    Point Double deriving (Eq, Show)
data Rectangle = Rectangle Point Point deriving (Eq, Show)


-- A point from a tuple Pair
point::(Double, Double) -> Point
point (a,b) = Point{x = a, y = b}

-- The origin
origin::Point
origin = Point{x = 0.0, y = 0.0}

-- Rectangle from a Tuple where (x0 y0) == origin
rectangle::(Double, Double) -> Rectangle
rectangle (a,b) = Rectangle origin (point (a,b))

base::Rectangle -> Double
base (Rectangle pointA pointB) = (x pointB) - (x pointA)

height::Rectangle -> Double
height (Rectangle pointA pointB) = (y pointB) - (y pointA)

-- Circle from radius
circle::Double -> Circle
circle radius = Circle origin radius

-- Clase Shift

class Shift a where
   shift::a -> (Double, Double) -> a
   
instance Shift Point where
   shift point (xMove, yMove) = Point{x=(x point) + xMove, y=(y point) + yMove}
   
instance Shift Rectangle where
   shift (Rectangle pointA pointB) (xMove, yMove) = (Rectangle (shift pointA (xMove, yMove)) (shift pointB (xMove, yMove)))
   
instance Shift Circle where
   shift  (Circle pointA radius) (xMove, yMove) = Circle (shift pointA (xMove, yMove)) radius
   
-- Define the Surface class

class Surface a where
  surface::a -> Double

instance Surface Rectangle where
  surface rectangle = base rectangle * height rectangle

instance Surface Circle where
  surface (Circle _ radius) = pi * radius**2
   
