
module LTree3 where

import Cp
import Data.Monoid
import Svg
import Control.Applicative
import List

-- (1) Datatype definition -----------------------------------------------------
type Point = (Int, Int)
type Side = Int
type Tri = (Point, Side)

data LTree3 a = Tri a | Nodo (LTree3 a) (LTree3 a) (LTree3 a) deriving (Show, Eq, Ord)

inLTree3 :: Either a ((LTree3 a,LTree3 a),LTree3 a) -> LTree3 a
inLTree3 = either Tri (uncurry (uncurry Nodo)) 

outLTree3 :: LTree3 a -> Either a ((LTree3 a, LTree3 a),LTree3 a)
outLTree3 (Tri a)       = i1 a
outLTree3 (Nodo t1 t2 t3) = i2 ((t1,t2),t3)

baseLTree3 g f = g -|- ((f >< f) >< f)

-- (2) Ana + cata + hylo -------------------------------------------------------

recLTree3 f = baseLTree3 id f          -- that is:  id -|- ((f >< f) >< f)

cataLTree3 g = g . (recLTree3 (cataLTree3 g)) . outLTree3

anaLTree3 f = inLTree3 . (recLTree3 (anaLTree3 f) ) . f

hyloLTree3 f g = cataLTree3 f . anaLTree3 g

-- (3) geraSierp + folhasSierp + sierpinski
geraSierp :: (Tri,Int) -> LTree3 Tri
geraSierp = anaLTree3 g2
    where
        g2 (t,0) = i1 t
        g2 (((x,y),s),n) = i2 ((t1,t2),t3) where
            t1 = (((x,y), s `div` 2), n-1)
            t2 = (((x+ s `div` 2,y), s `div` 2), n-1)
            t3 = (((x,y+ s `div` 2), s `div` 2), n-1)

folhasSierp :: LTree3 Tri -> [Tri]
folhasSierp = cataLTree3 g1
    where
        g1 = either singl (uncurry (++) . ((uncurry (++) >< id)))

sierpinski :: (Tri,Int) -> [Tri]
sierpinski = folhasSierp . geraSierp

-- Correr código

type Svg = String
tri2svg :: Tri -> Svg
tri2svg (p, c) = (red . polyg) [p, p .+ (0, c), p .+ (c, 0)]

base :: Tri
base = ((0, 0), 32)

desenha x = picd'' [scale 0.44 (0, 0) (x >>= tri2svg)]

---------------------------- end of library ----------------------------------