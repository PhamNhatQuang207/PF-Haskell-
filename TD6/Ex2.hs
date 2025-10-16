{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
import System.IO (hFlush, stdout)
import Data.List (sortOn, intercalate)

type Cellule = String
type Ligne = [Cellule]
type CSV = [Ligne]

--2.1
decoupe_rec :: Eq a => a -> [a] -> [[a]]
decoupe_rec _ [] = [[]]
decoupe_rec sep (x:xs)
  | x == sep  = [] : decoupe_rec sep xs
  | otherwise = let (h:t) = decoupe_rec sep xs in (x:h) : t

decoupe_foldr :: Eq a => a -> [a] -> [[a]]
decoupe_foldr sep = foldr helper [[]]
  where
    helper x (h:t)
      | x == sep  = [] : h : t
      | otherwise = (x:h) : t
    helper _ [] = [[]]

--2.2
csv :: String -> CSV
csv content = map (decoupe_rec ',') (lines content)

--2.3
(!?) :: [a] -> Int -> Maybe a
list !? n = lookup n (zip [0..] list)

(!??) :: [a] -> Int -> Maybe a
[] !?? _ = Nothing
(x:_) !?? 0 = Just x
(_:xs) !?? n
  | n < 0     = Nothing
  | otherwise = xs !?? (n - 1)

--2.4
joinN_rec :: Int -> Int -> CSV -> CSV -> CSV
joinN_rec _ _ [] _ = []
joinN_rec i j (l1:rest_csv1) csv2 =
  let val1 = l1 !? i
      matching_l2s = filter (\l2 -> (l2 !? j) == val1) csv2
      new_joins = map (l1 ++) matching_l2s
  in new_joins ++ joinN_rec i j rest_csv1 csv2

joinN_fold :: Int -> Int -> CSV -> CSV -> CSV
joinN_fold i j csv1 csv2 = concatMap find_matches csv1
  where
    find_matches l1 =
      let val1 = l1 !? i
          matching_l2s = filter (\l2 -> (l2 !? j) == val1) csv2
      in map (l1 ++) matching_l2s

--2.5
avance :: Int -> Int -> Ligne -> CSV -> (CSV, CSV)
avance i j l1 sorted_csv2 =
  let key1 = l1 !? i
      -- Bỏ qua các hàng có giá trị cột j nhỏ hơn
      -- `Nothing` được coi là nhỏ nhất
      csv2_rest = dropWhile (\l2 -> (l2 !? j) < key1) sorted_csv2
      -- Lấy các hàng có giá trị cột j bằng
      csv2_match = takeWhile (\l2 -> (l2 !? j) == key1) csv2_rest
  in (csv2_match, csv2_rest)

--2.6
join :: Int -> Int -> CSV -> CSV -> CSV
join i j csv1 csv2 =
  let sorted_csv1 = sortOn (!? i) csv1
      sorted_csv2 = sortOn (!? j) csv2
  in go sorted_csv1 sorted_csv2
  where
    go :: CSV -> CSV -> CSV
    go [] _ = []
    go (l1:rest_csv1) current_csv2 =
      let (matches, next_csv2) = avance i j l1 current_csv2
          new_joins = map (l1 ++) matches
      in new_joins ++ go rest_csv1 next_csv2

--2.7
main :: IO ()
main = do
  -- Lấy thông tin cho tệp 1
  putStr "Enter path for first CSV file: "
  hFlush stdout
  filePath1 <- getLine
  putStr "Enter join column index for first file: "
  hFlush stdout
  colStr1 <- getLine
  let col1 = read colStr1 :: Int

  -- Lấy thông tin cho tệp 2
  putStr "Enter path for second CSV file: "
  hFlush stdout
  filePath2 <- getLine
  putStr "Enter join column index for second file: "
  hFlush stdout
  colStr2 <- getLine
  let col2 = read colStr2 :: Int

  -- Đọc tệp và phân tích
  content1 <- readFile filePath1
  content2 <- readFile filePath2
  let csv1 = csv content1
  let csv2 = csv content2

  -- Thực hiện kết nối và in kết quả
  putStrLn "\n--- Joined CSV Result ---"
  let result_csv = join col1 col2 csv1 csv2
  let output_string = unlines (map (intercalate ",") result_csv)
  putStr output_string