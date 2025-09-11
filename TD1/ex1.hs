monExp1 :: Int -> Int -> Int
monExp1 a n = if n == 0 then 1 
           else a * monExp1 a (n-1)

monExp2 :: Int -> Int -> Int
monExp2 a n = go a n 1
  where
    go a n acc
      | n == 0    = acc
      | otherwise = go a (n - 1) (a * acc)

monExp3 :: Int -> Int -> Int
monExp3 a n | n == 0 = 1
            | n == 1 = a
            | otherwise =
            let (q, r) = n `quotRem` 2
                halfResult = monExp3 a q
                squareResult = halfResult * halfResult
            in if r == 0
                then squareResult
                else a * squareResult

monExp4 :: Int -> Int -> Int
monExp4 a n = product (replicate n a)


main = do
    print (monExp4 5 2)