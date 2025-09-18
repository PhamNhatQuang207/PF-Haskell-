import Graphics.Gloss
pointAintercaler :: Point -> Point -> Point
pointAintercaler (xa, ya) (xb, yb) = ((xa + xb + yb - ya) / 2, (ya + yb + xa - xb) / 2)

pasDragon :: Path -> Path
pasDragon [] = []
pasDragon [p] = [p]
pasDragon pts =
  let pairs = zip pts (tail pts)
      pairToList i (a,b) = [a, if even i then pointAintercaler a b else pointAintercaler b a]
  in concat (zipWith pairToList [0..] pairs) ++ [last pts]

dragon :: Point -> Point -> [Path]
dragon a b = iterate pasDragon [a,b]


main = animate (InWindow "Dragon" (500, 500) (0, 0)) white (dragonAnime (50,250) (450,250))

dragonAnime a b t = Line (dragon a b !! (round t `mod` 20))
