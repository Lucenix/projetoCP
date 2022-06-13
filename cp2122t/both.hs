import Cp
import LTree

t0 = Fork (
        Fork (
             Fork (Leaf 2,Leaf 7),
             Fork (Leaf 5,Leaf 4)),
        Fork (
             Fork (Leaf 8,Leaf 6),
             Fork (Leaf 1,Leaf 3)))
res0 = (5,4)

t1 = Fork (
        Fork (
            Fork (Leaf 7,Leaf 3),
            Fork (Leaf 1,Leaf 2)),
        Fork (
            Fork (Leaf 5,Leaf 10),
            Fork (Leaf 8,Leaf 1)))
res1 = (8,3)

t2 = Fork (
        Fork (
            Fork (
                Fork (Leaf 39,Leaf 79),
                Fork (Leaf 1,Leaf 74)),
            Fork (
                Fork (Leaf 42,Leaf 34),
                Fork (Leaf 59,Leaf 99))),
        Fork (
            Fork (
                Fork (Leaf 68,Leaf 58),
                Fork (Leaf 80,Leaf 36)),
            Fork (
                Fork (Leaf 70,Leaf 8),
                Fork (Leaf 33,Leaf 79))))
res2 = (39,70)

alice :: Ord c => LTree c -> c
alice (Leaf x) = id x
alice (Fork (t1,t2)) = ((uncurry max).(bob >< bob)) (t1,t2)

bob :: Ord c => LTree c -> c
bob (Leaf x) = id x
bob (Fork (t1,t2)) = ((uncurry min).(alice >< alice)) (t1,t2)



both :: LTree Integer -> (Integer, Integer)
both x = cataLTree (split (either id (uncurry max.(p2 >< p2))) (either id (uncurry min.(p1 >< p1)))) x