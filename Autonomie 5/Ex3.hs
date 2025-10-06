mystere :: (a->b->Bool)-> [a] -> [b] -> [(a,b)]
mystere p l1 l2 = snd <$>
                (filter fst $
                (\a b -> (p a b, (a,b))) <$> l1 <*> l2)