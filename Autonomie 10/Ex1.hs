module Exercice1 where

-- 1. Định nghĩa cấu trúc
data Tournoi a = Joueur { vainqueur :: a }
               | Match { vainqueur :: a, tG :: (Tournoi a), tD :: (Tournoi a) }
               deriving (Show, Eq)

-- 2. Q 1.1
joueMatch :: (Ord a) => Tournoi a -> Tournoi a -> Tournoi a
joueMatch t1 t2 = 
    let v1 = vainqueur t1
        v2 = vainqueur t2
    in if v1 <= v2 
       then Match v1 t1 t2
       else Match v2 t1 t2

-- 3. Q 1.2
jouerUnTour :: (Ord a) => [Tournoi a] -> [Tournoi a]
jouerUnTour [] = []
jouerUnTour [x] = [x]
jouerUnTour (x:y:xs) = (joueMatch x y) : jouerUnTour xs

jouerTournoi :: (Ord a) => [a] -> Maybe (Tournoi a)
jouerTournoi [] = Nothing
jouerTournoi xs = loop (map Joueur xs)
  where
    loop [t] = Just t
    loop ts  = loop (jouerUnTour ts)

main :: IO ()
main = do
    putStrLn "=== BAT DAU GIAI DAU (EXERCICE 1) ==="
    
    -- Ví dụ: Danh sách các con số (độ ưu tiên)
    -- Số càng nhỏ thì độ ưu tiên càng cao (như Dijkstra)
    let participants = [10, 5, 8, 3, 12 , 11]
    
    putStrLn $ "Danh sach dau vao: " ++ show participants
    
    putStrLn "\nDang xay dung cay dau loai..."
    let ketQua = jouerTournoi participants
    
    case ketQua of
        Nothing -> putStrLn "Khong co nguoi choi nao!"
        Just tree -> do
            putStrLn "\nCau truc cay cuoi cung (Hien thi dang text):"
            print tree
            putStrLn $ "=> NGUOI VO DICH (Goc cay): " ++ show (vainqueur tree)