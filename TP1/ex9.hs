function :: (a -> a) -> a -> Int -> [a]
function f x 0 = [x]
function f x n = x : function f (f x) (n-1) 

f a = a * 2