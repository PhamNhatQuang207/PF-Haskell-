-- Q5.1: Ma trận đơn vị
idMat :: Int -> [[Int]]
idMat n = [ [if i == j then 1 else 0 | j <- [1..n]] | i <- [1..n] ]

-- Q5.2: Tích vô hướng (dot product)
dot :: Num a => [a] -> [a] -> a
dot xs ys = sum (zipWith (*) xs ys)

-- Q5.3: Chuyển vị ma trận
transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([]:_) = []
transpose x = map head x : transpose (map tail x)

-- Q5.4: Nhân ma trận
matMul :: Num a => [[a]] -> [[a]] -> [[a]]
-- matMul a b = [[dot row col | col <- transpose b] | row <- a]
-- hoặc dùng map:
matMul a b = map (\row -> map (dot row) (transpose b)) a


-- Ví dụ test
main :: IO ()
main = do
    let a = [[1,2,3],[4,5,6]]
    let b = [[7,8],[9,10],[11,12]]

    putStrLn "Ma tran don vi 3x3:"
    print (idMat 3)

    putStrLn "Tich vo huong [1,2,3] . [4,5,6]:"
    print (dot [1,2,3] [4,5,6])

    putStrLn "Chuyen vi cua [[1,2,3],[4,5,6]]:"
    print (transpose a)

    putStrLn "Nhan ma tran a * b:"
    print (matMul a b)
