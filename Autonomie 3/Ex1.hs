import qualified Data.Map as M

data Trie = Trie
  { endOfWord :: Bool
  , children  :: M.Map Char Trie
  } deriving (Show)


emptyTrie :: Trie
emptyTrie = Trie False M.empty

insert :: String -> Trie -> Trie
insert []     (Trie _ ch) = Trie True ch
insert (c:cs) (Trie end ch) =
  Trie end (M.alter (Just . insert cs . maybe emptyTrie id) c ch)


member :: String -> Trie -> Bool
member []     (Trie end _) = end
member (c:cs) (Trie _ ch) =
  case M.lookup c ch of
    Nothing -> False
    Just t  -> member cs t
