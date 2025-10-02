sommeCarres :: Num a => [a] -> a
sommeCarres = foldl (\acc x -> x*x + acc) 0