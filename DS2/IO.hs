import Control.Monad (guard)
import Text.Printf (printf)
import Control.Applicative (some, many, (<|>))
askNameAge :: IO()
askNameAge =  (printResults <$> getLine <*> readLn) >>= putStr

printResults :: String -> Int -> String
printResults n a = "Chao mung " ++ n ++ ", " ++ show a ++ " tuoi"

validatePassword :: String -> Maybe String
validatePassword pass = pass <$ guard (length pass > 8)

getSafePassword :: IO ()
getSafePassword = do
    putStr "Enter password"
    pass <- getLine
    case validatePassword pass of
        Just safePass -> putStrLn ("Your safe password is: " ++ safePass)
        Nothing -> putStrLn "Password is too short!" >> getSafePassword

getSafePassword2 :: IO String
getSafePassword2 = do
    putStr "Enter password: "
    
    -- Đây là lúc phép thuật xảy ra
    -- "action" <|> "retry"
    (getLine >>= validateIO) <|> retry
  where
    -- 1. HÀNH ĐỘNG (ACTION)
    -- Lấy một chuỗi và validate nó.
    -- Hành động này sẽ "thất bại" (gọi 'empty') nếu guard sai.
    validateIO :: String -> IO String
    validateIO pass = do
        guard (length pass > 8) -- Nếu false, 'guard' sẽ gọi 'empty'
        return pass             -- Dòng này chỉ chạy nếu guard là true

    -- 2. THỬ LẠI (RETRY)
    -- Đây là "lựa chọn thay thế", nó chỉ chạy khi 'validateIO' thất bại.
    retry :: IO String
    retry = do
        putStrLn "Password is too short, try again:"
        getSafePassword2 -- Gọi lại chính nó để thử lại

inputScore :: IO ()
inputScore = do
    putStr "Enter all the score"
    score <- many (readLn :: IO Double)
    case score of
        [] -> putStrLn "Khong co diem nao."
        scores -> do
            let total = sum scores
            
            -- b. Đếm số lượng
            let count = length scores
            
            -- c. Tính trung bình (Phải dùng fromIntegral để chuyển Int sang Double)
            let avg = total / fromIntegral count
            -- d. In kết quả
            -- (dùng printf "%.2f\n" avg để in 2 chữ số thập phân cho đẹp)
            putStr "Diem trung binh la: "
            printf "%.2f\n" avg

    
getMember :: IO String
getMember = do
    putStr "Input member's name "
    getLine >>= valid
        where
            valid :: String -> IO String
            valid s = do
                guard (s /= "x")
                return s

main :: IO ()
main = do
    (some getMember >>= printTeam) <|> printError
        where
            printTeam :: [String] -> IO ()
            printTeam members = putStrLn ("Your team: " ++ show members)

            printError :: IO ()
            printError = putStrLn "Error: At least 1 member"
