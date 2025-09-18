import Graphics.Gloss
pointAintercaler :: Point -> Point -> Point
pointAintercaler (xa, ya) (xb, yb) = ((xa + xb + yb - ya) / 2, (ya + yb + xa - xb) / 2)

dragonOrdre :: Point -> Point -> Int -> Path
dragonOrdre a b 0 = [a,b]
dragonOrdre a b n =
  let c = pointAintercaler a b
      left  = dragonOrdre a c (n-1)   -- path from A to C
      right = dragonOrdre b c (n-1)   -- path from B to C
  in left ++ tail (reverse right)

main :: IO ()
main = animate (InWindow "Dragon (animated orders)" (500,500) (100,100)) white frame
  where
    a = (50,250)
    b = (450,250)
    frame t = Line (dragonOrdre a b (fromInteger (floor t) `mod` 20))