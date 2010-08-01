{-# LANGUAGE DeriveDataTypeable, PatternGuards #-}
module Tim.Smallpt.Protocol.Render(
  Inputs(..),
  Refl(..),
  Sphere(..),
  Vec(..),
  (|*|), 
  (|+|), 
  (|-|),
  cross,
  dot, 
  norm, 
  vmult) where
    
import Data.Data
import Data.Typeable

data Vec a = Vec a a a
             deriving (Data, Typeable)

data Refl = DIFF
          | SPEC
          | REFR
          deriving (Data, Typeable)

data Sphere a = Sphere { radius :: a,
                         position :: Vec a,
                         emission :: Vec a,
                         colour :: Vec a,
                         refl :: Refl }
                deriving (Data, Typeable)

data Inputs a = Inputs { width :: Int,
                         height :: Int,
                         samples :: Int,
                         scene :: [Sphere a] }
                deriving (Data, Typeable)

instance Functor Vec where
  fmap f (Vec x y z) = Vec (f x) (f y) (f z)

(|+|) :: Num a => Vec a -> Vec a -> Vec a
(Vec x1 y1 z1) |+| (Vec x2 y2 z2) = Vec (x1 + x2) (y1 + y2) (z1 + z2)

(|-|) :: Num a => Vec a -> Vec a -> Vec a
(Vec x1 y1 z1) |-| (Vec x2 y2 z2) = Vec (x1 - x2) (y1 - y2) (z1 - z2)

(|*|) :: Num a => Vec a -> a -> Vec a
v |*| n = fmap (* n) v

vmult :: Num a => Vec a -> Vec a -> Vec a
(Vec x1 y1 z1) `vmult` (Vec x2 y2 z2) = Vec (x1 * x2) (y1 * y2) (z1 * z2)

norm :: Floating a => Vec a -> Vec a
norm v = let Vec x y z = v in v |*| (1 / sqrt ((x * x) + (y * y) + (z * z)))

dot :: Num a => Vec a -> Vec a -> a
(Vec x1 y1 z1) `dot` (Vec x2 y2 z2) = (x1 * x2) + (y1 * y2) + (z1 * z2)

cross :: Num a => Vec a -> Vec a -> Vec a
(Vec x1 y1 z1) `cross` (Vec x2 y2 z2) = Vec (y1 * z2 - z1 * y2) (z1 * x2 - x1 * z2) (x1 * y2 - y1 * x2)

infixl 6 |+|
infixl 6 |-|
infixl 7 |*|

