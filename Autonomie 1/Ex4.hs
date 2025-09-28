w1 :: Int -> Int -> Int
w1 a b = let (qa,ra) = a `quotRem` 2 
             (qb,rb) = b `quotRem` 2 
         in if ra > 0 && rb >0
            then w1 qa qb
            else qa + qb

w2 :: Int -> Int -> Int
w2 a b = s*s + 4*d*d
        where s = a + b
              d = a - b