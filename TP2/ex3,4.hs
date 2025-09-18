pasPascal :: [Integer] -> [Integer]
pasPascal l = zipWith (+) (0:l) (l++[0])

pascal :: [[Integer]]
pascal = iterate pasPascal [1]