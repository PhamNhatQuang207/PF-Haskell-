 
sommeDeXaY x y =
  if (x == y) then x
  else 
    x + sommeDeXaY (x+1) y

main = do
  print (sommeDeXaY 3 5)