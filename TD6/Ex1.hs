{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
wc_do :: FilePath -> IO Int
wc_do filePath = do
  content <- readFile filePath
  let wordList = words content
  let count = length wordList
  return count

wc_monadic :: FilePath -> IO Int
wc_monadic filePath =
  readFile filePath >>= \content ->
  return (length (words content))

wc_applicative :: FilePath -> IO Int
wc_applicative filePath = length . words <$> readFile filePath