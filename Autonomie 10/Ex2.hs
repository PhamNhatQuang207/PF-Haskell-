module Exercice2 where

-- ==========================================
-- KHAI BÁO KIỂU DỮ LIỆU (Data Types)
-- ==========================================

-- CP: Cấu trúc chứa Giá trị, Độ ưu tiên, và Khóa
-- a: kiểu giá trị, b: kiểu độ ưu tiên (so sánh được), c: kiểu khóa (so sánh được)
data CP a b c = CP { 
    valeur   :: a, 
    priorite :: b, 
    clef     :: c 
} deriving (Show, Eq)

-- TABR: Tournoi Arbre Binaire de Recherche (Cây đấu loại lai cây tìm kiếm)
data TABR a b c = Joueur { vainqueur :: CP a b c }
                | Match  { 
                    vainqueur :: CP a b c,
                    maxClefG  :: c,          -- Khóa lớn nhất bên trái
                    maxClefD  :: c,          -- Khóa lớn nhất bên phải
                    tG        :: TABR a b c, -- Cây con trái
                    tD        :: TABR a b c  -- Cây con phải
                } deriving (Show, Eq)

-- Hàm phụ trợ: Lấy khóa lớn nhất của một cây TABR
-- Nếu là Match, khóa lớn nhất luôn nằm ở nhánh phải (maxClefD) do tính chất BST
getMaxKey :: TABR a b c -> c
getMaxKey (Joueur cp) = clef cp
getMaxKey (Match _ _ md _ _) = md

-- ==========================================
-- Q 2.1: XÂY DỰNG CÂY (Construction)
-- ==========================================

-- Hàm joueMatch: Tạo một trận đấu từ 2 cây con.
-- Giả định quan trọng: Tất cả khóa của t1 < Tất cả khóa của t2.
joueMatch :: (Ord b) => TABR a b c -> TABR a b c -> TABR a b c
joueMatch t1 t2 = 
    let v1 = vainqueur t1
        v2 = vainqueur t2
        -- Người thắng là người có độ ưu tiên NHỎ HƠN (Min-Heap)
        winner = if priorite v1 <= priorite v2 then v1 else v2
        
        -- Cập nhật khóa lớn nhất cho nút mới
        mg = getMaxKey t1
        md = getMaxKey t2
    in Match { 
        vainqueur = winner, 
        maxClefG  = mg, 
        maxClefD  = md, 
        tG        = t1, 
        tD        = t2 
    }

-- Hàm jouerUnTour: Ghép cặp các cây trong danh sách đã sắp xếp theo khóa
jouerUnTour :: (Ord b) => [TABR a b c] -> [TABR a b c]
jouerUnTour [] = []
jouerUnTour [x] = [x] -- Nếu lẻ, người cuối cùng được vào thẳng vòng sau
jouerUnTour (x:y:xs) = (joueMatch x y) : jouerUnTour xs

-- Hàm jouerTournoi (Bổ sung để hoàn thiện): Xây dựng cây hoàn chỉnh từ danh sách lá
jouerTournoi :: (Ord b) => [TABR a b c] -> Maybe (TABR a b c)
jouerTournoi [] = Nothing
jouerTournoi xs = loop xs
  where
    loop [t] = Just t
    loop ts  = loop (jouerUnTour ts)

-- ==========================================
-- Q 2.2: THAY ĐỔI ĐỘ ƯU TIÊN (Modification)
-- ==========================================

-- Hàm ajuste: Tìm lá có khóa c, thay đổi độ ưu tiên bằng hàm f, cập nhật lại người thắng
ajuste :: (Ord b, Ord c) => (b -> b) -> c -> TABR a b c -> TABR a b c
ajuste f key targetTree = case targetTree of
    -- Trường hợp cơ sở: Là lá (Joueur)
    Joueur cp -> 
        if clef cp == key 
        then Joueur (cp { priorite = f (priorite cp) }) -- Cập nhật độ ưu tiên
        else targetTree -- Không tìm thấy (hoặc sai khóa), giữ nguyên

    -- Trường hợp đệ quy: Là nút (Match)
    Match _ mg _ left right ->
        if key <= mg 
        then 
            -- Khóa cần tìm nằm bên trái (dựa vào tính chất BST)
            let newLeft = ajuste f key left
            in joueMatch newLeft right -- Gọi lại joueMatch để tính lại người thắng tại nút này
        else 
            -- Khóa cần tìm nằm bên phải
            let newRight = ajuste f key right
            in joueMatch left newRight
            
-- ==========================================
-- Q 2.3: XÓA PHẦN TỬ NHỎ NHẤT (Suppression)
-- ==========================================

-- Hàm supprMin: Xóa người thắng cuộc (Gốc của cây)
supprMin :: (Ord b) => TABR a b c -> Maybe (TABR a b c)
supprMin (Joueur _) = Nothing -- Xóa lá duy nhất -> Cây rỗng
supprMin (Match v _ _ left right) =
    -- So sánh priority của người thắng hiện tại với priority của người thắng bên trái
    if priorite v == priorite (vainqueur left)
    then 
        -- Người thắng đến từ nhánh TRÁI -> Xóa trong nhánh trái
        case supprMin left of
            Nothing -> Just right -- Nếu nhánh trái bị xóa hết, nhánh phải đôn lên
            Just newLeft -> Just (joueMatch newLeft right) -- Nếu còn, đấu lại
    else 
        -- Người thắng đến từ nhánh PHẢI -> Xóa trong nhánh phải
        case supprMin right of
            Nothing -> Just left -- Nếu nhánh phải bị xóa hết, nhánh trái đôn lên
            Just newRight -> Just (joueMatch left newRight) -- Nếu còn, đấu lại

-- ==========================================
-- PHẦN KIỂM THỬ (TESTING)
-- ==========================================

-- Tạo dữ liệu mẫu
p1, p2, p3, p4 :: CP String Int Char
p1 = CP "Alice"   10 'A'
p2 = CP "Bob"      5 'B'
p3 = CP "Charlie"  8 'C'
p4 = CP "David"   20 'D'

-- Tạo danh sách các lá (Joueur) từ dữ liệu mẫu
-- Lưu ý: Danh sách này PHẢI được sắp xếp theo Khóa (A, B, C, D)
dsLa :: [TABR String Int Char]
dsLa = [Joueur p1, Joueur p2, Joueur p3, Joueur p4]

-- Hàm tiện ích để in người thắng cuộc cho gọn
inNguoiThang :: Maybe (TABR String Int Char) -> String
inNguoiThang Nothing = "Cay rong!"
inNguoiThang (Just t) = 
    let w = vainqueur t
    in "Nguoi thang: " ++ valeur w ++ " (Diem: " ++ show (priorite w) ++ ")"

main :: IO ()
main = do
    putStrLn "--- 1. KHOI TAO CAY (Q 2.1) ---"
    let cayTournoi = jouerTournoi dsLa
    
    -- Mong đợi: Bob (5) thắng Alice (10). Charlie (8) thắng David (20).
    -- Chung kết: Bob (5) thắng Charlie (8). -> Bob Vô địch.
    putStrLn $ "Ket qua ban dau: " ++ inNguoiThang cayTournoi
    
    putStrLn "\n--- 2. THAY DOI DO UU TIEN (Q 2.2) ---"
    putStrLn "Tinh huong: David ('D') luyen tap va giam diem xuong con 1 (Gioi nhat)."
    
    -- Cập nhật: Tìm ID 'D', đổi priority thành 1
    -- Lưu ý: cayTournoi đang là Maybe, ta cần lấy giá trị bên trong để dùng
    let (Just t) = cayTournoi
    let cayMoi = ajuste (\_ -> 1) 'D' t
    
    putStrLn $ "Ket qua sau khi David buff suc manh: " ++ inNguoiThang (Just cayMoi)
    -- Mong đợi: David (1) < Bob (5) -> David chiếm ngôi vô địch.

    putStrLn "\n--- 3. XOA NGUOI THANG (Q 2.3) ---"
    putStrLn "Tinh huong: Xoa nguoi vo dich hien tai (David) khoi giai dau."
    
    let caySauKhiXoa = supprMin cayMoi
    
    putStrLn $ "Ket qua sau khi xoa David: " ++ inNguoiThang caySauKhiXoa
    -- Mong đợi: David đi. Bob (5) quay lại làm vua.