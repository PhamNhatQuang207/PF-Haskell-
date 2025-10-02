data Op = Plus | Div | Mult | Minus
  deriving (Show, Eq)

data Exp = Val Float | Bop Op Exp Exp
  deriving (Show, Eq)

type NP = [Either Op Float]

-- helper
applyOp :: Op -> Exp -> Exp -> Exp
applyOp op e1 e2 = Bop op e1 e2

-- parse Polish notation to Maybe [Exp]
parse :: NP -> Maybe [Exp]
parse = go [] . reverse            -- read tokens right-to-left
  where
    go stack [] = Just stack      -- return stack in current order
    go stack (tok:toks) = case tok of
      Right n -> go (Val n : stack) toks
      Left op -> case stack of
        (e1:e2:rest) -> go (applyOp op e1 e2 : rest) toks
        _            -> Nothing