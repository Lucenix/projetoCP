
module Ex4 where 

import Cp
import List
import Probability
import Prelude
import Data.Char

type Bit = Int

type Bin = [Bit]

type Bit3 = (Bit,Bit,Bit)

tobin = rtrim 5 . pad 5 . dec2bin

frombin = bin2dec . rtrim 5

bin2dec :: Bin -> Int
bin2dec [a] = a
bin2dec b   = bin2dec(init b) * 2 + last b

rtrim n a = drop (length a - n) a

dec2bin 0 = []
dec2bin n = dec2bin m ++ [b] where (m,b) = (div n 2, mod n 2)

pad n x = take m zeros ++ x where
   m = n-length x
   zeros = 0:zeros

bflip :: Bit -> Dist Bit
bflip 0 = D[(0,0.96), (1,0.04)]
bflip 1 = D[(1,0.90), (0,0.10)]

bflips = propagate bflip

enc :: Char -> Bin
enc c = tobin (ord c - ord 'A')

dec :: Bin -> Char
dec b = chr(frombin b + ord 'A') 

dec' :: Dist Bin -> Dist Char
dec' = fmap dec

propagate:: Monad m => (t -> m a) -> [t] -> m [a]
propagate f = cataList (g f) where
    g f = either (return . nil) (g2 f)
    g2 f (a,b) = do {x <- (f a); y <- b; return (x:y)}

v3 (0, 0, 0) = 0
v3 (0, 0, 1) = 0
v3 (0, 1, 0) = 0
v3 (0, 1, 1) = 1
v3 (1, 0, 0) = 0
v3 (1, 0, 1) = 1
v3 (1, 1, 0) = 1
v3 (1, 1, 1) = 1

bflip3:: Bit3 -> Dist Bit3
bflip3 (a,b,c) = do {x <- bflip a; y <- bflip b; z <- bflip c; return (x,y,z) }

propagate3:: (Monad m) => (Bit3 -> m Bit3) -> [Bit] -> m [Bit]
propagate3 f = cataList (g f) where
    g f = either (return . nil) (g2 f)
    g2 f (a,b) = do {x <- ((fmap v3) . f) (a,a,a); y <- b; return (x:y)}
