-- A Polish expression is a list of operators or values
type ExpPolonaise a = [Either BinOp a]

data BinOp = Add | Sous | Mult | Div
data Expression a = Val a | BOp BinOp (Expression a) (Expression a)

-- Convert a Polish expression into an Expression tree
expPToExpression :: ExpPolonaise a -> Maybe (Expression a)
expPToExpression ep =
  case go ep of
    Just ([], e) -> Just e       -- success only if no tokens remain
    _            -> Nothing      -- fail if parsing incomplete or invalid
  where
    -- Parse one expression and return leftover tokens + the tree
    go :: ExpPolonaise a -> Maybe (ExpPolonaise a, Expression a)

    go [] = Nothing
      -- No tokens left when an expression is expected -> invalid

    go (Right v : ep0) = Just (ep0, Val v)
      -- A value creates a leaf node; return it with remaining tokens

    go (Left op : ep0) =
      case go ep0 of
        Nothing -> Nothing
        Just (ep1, e1) ->
          case go ep1 of
            Nothing -> Nothing
            Just (ep2, e2) -> Just (ep2, BOp op e1 e2)
      -- An operator requires two subexpressions in prefix order.
      -- We parse the left one (e1), then the right one (e2),
      -- and combine them into a binary operation node.
