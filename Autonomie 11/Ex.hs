module Main where

import Parser
import Data.Char (isAlphaNum, isSpace)
import System.IO

-- ==========================================
-- EXERCICE 1: Mô hình dữ liệu (Data Models)
-- ==========================================

-- Q 1.1: Định nghĩa Template và Element
data Element = Text String 
             | Var String
             deriving (Show, Eq)

type Template = [Element]

-- ==========================================
-- EXERCICE 2: Parser các mô hình
-- ==========================================

-- Hàm tiện ích: Bỏ qua khoảng trắng (space, tab, newline)
spaces :: Parser String
spaces = many (carQuand isSpace)

-- Q 2.1: Parser một biến (Placeholder) {{ variable }}
-- Sử dụng chaine từ Parser.hs và carQuand với isAlphaNum
parsePlaceholder :: Parser Element
parsePlaceholder = do
    _ <- chaine "{{"
    _ <- spaces
    name <- some (carQuand isAlphaNum) -- Tên biến phải có ít nhất 1 ký tự
    _ <- spaces
    _ <- chaine "}}"
    pure (Var name)

-- Q 2.2: Parser văn bản đơn giản (Text)
-- Đọc các ký tự KHÔNG phải là dấu '{' 
-- (Theo đề bài: văn bản không chứa ngoặc nhọn ngoài phần biến)
parseText :: Parser Element
parseText = do
    content <- some (carQuand (/= '{'))
    pure (Text content)

-- Q 2.3: Parser toàn bộ Template
-- Template là danh sách gồm nhiều Placeholder hoặc Text nối tiếp nhau
parseTemplate :: Parser Template
parseTemplate = many (parsePlaceholder <|> parseText)

-- ==========================================
-- EXERCICE 3: Khởi tạo và hiển thị (Instancier)
-- ==========================================

-- Định nghĩa kiểu dữ liệu JSON (như đề bài cung cấp)
data JSON = JsonNull
          | JsonBool Bool
          | JsonInt Int
          | JsonFloat Float
          | JsonString String
          | JsonArray [JSON]
          | JsonObject [(String, JSON)]
          deriving (Show, Eq)

-- Q 3.1: Hàm instElement
-- Thay thế Element bằng giá trị từ JSON
instElement :: JSON -> Element -> Maybe String
instElement _ (Text s) = Just s
instElement (JsonObject pairs) (Var name) = 
    case lookup name pairs of
        Just (JsonString s) -> Just s
        Just (JsonInt i)    -> Just (show i)
        Just (JsonBool b)   -> Just (show b) -- True/False
        _                   -> Nothing -- Không tìm thấy hoặc sai kiểu dữ liệu
instElement _ (Var _) = Nothing -- JSON không phải là Object

-- Q 3.2: Hàm instTemplate
-- Dùng Applicative Notation để nối chuỗi kết quả
instTemplate :: JSON -> Template -> Maybe String
instTemplate json template = 
    concat <$> sequence (map (instElement json) template)

-- ==========================================
-- PHẦN BỔ SUNG: Parser JSON (Giả lập)
-- ==========================================
-- Đề bài nói "parses JSON que vous avez fait en TD" (bạn đã làm ở bài trước).
-- Để code này chạy được, tôi viết một parser JSON đơn giản ở đây để xử lý file test.json.

parseJSONString :: Parser String
parseJSONString = do
    _ <- car '"'
    s <- many (carQuand (/= '"'))
    _ <- car '"'
    pure s

parseJSONInt :: Parser Int
parseJSONInt = do
    d <- some (carQuand (`elem` ['0'..'9']))
    pure (read d)

parseJSONBool :: Parser Bool
parseJSONBool = (chaine "true" >> pure True) <|> (chaine "false" >> pure False)

parseJSONPair :: Parser (String, JSON)
parseJSONPair = do
    _ <- spaces
    k <- parseJSONString
    _ <- spaces
    _ <- car ':'
    _ <- spaces
    v <- parseJSONValue
    pure (k, v)

parseJSONObject :: Parser JSON
parseJSONObject = do
    _ <- car '{'
    _ <- spaces
    -- Đọc cặp đầu tiên
    p1 <- parseJSONPair
    -- Đọc các cặp tiếp theo (có dấu phẩy đằng trước)
    ps <- many (do _ <- spaces; _ <- car ','; parseJSONPair)
    _ <- spaces
    _ <- car '}'
    pure (JsonObject (p1:ps))

parseJSONValue :: Parser JSON
parseJSONValue = (JsonString <$> parseJSONString)
             <|> (JsonInt <$> parseJSONInt)
             <|> (JsonBool <$> parseJSONBool)
             <|> parseJSONObject

parseJSON :: Parser JSON
parseJSON = do
    _ <- spaces
    obj <- parseJSONObject
    _ <- spaces
    pure obj

-- Hàm tiện ích đề bài yêu cầu: evalParser
-- Trả về giá trị nếu parse thành công
evalParser :: Parser a -> String -> Maybe a
evalParser p s = case runParser p s of
    Just (res, _) -> Just res -- Lấy kết quả parse được
    Nothing       -> Nothing

-- ==========================================
-- Q 3.3: Main
-- ==========================================

main :: IO ()
main = do
    -- 1. Đọc file
    -- Lưu ý: Bạn cần tạo file test.mustache và test.json để chạy thử
    templateContent <- readFile "test.mustache"
    jsonContent <- readFile "test.json"

    -- 2. Xử lý logic
    let result = do
            -- Parse Template
            tmpl <- case evalParser parseTemplate templateContent of
                Just t -> Right t
                Nothing -> Left "Erreur fichier: test.mustache"
            
            -- Parse JSON
            jsonData <- case evalParser parseJSON jsonContent of
                Just j -> Right j
                Nothing -> Left "Erreur fichier: test.json"

            -- Instantiate
            case instTemplate jsonData tmpl of
                Just res -> Right res
                Nothing -> Left "Erreur données: test.json si l'instanciation..."

    -- 3. Xuất kết quả
    case result of
        Left err -> putStrLn err
        Right txt -> do
            writeFile "test.txt" txt
            putStrLn "Thành công! Kiểm tra file test.txt"