exApprox x n = foldr (\k acc -> 1 + x / fromIntegral k * acc) 1 [1..n]

expApprox x n = res
  where
    (_,_,res) = foldl go (1,1,1) [1..n]
    go (xk,kf,res) k = (xk * x,kf*k,res + (xk*x) / (kf*k))