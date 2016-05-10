module RLE where

import Data.List ( group )

-- | 런 렝스 압축
--
-- >>> rle ""
-- ""
-- >>> rle "A"
-- "A1"
-- >>> rle "AAABBCCCCAAA"
-- "A3B2C4A3"
--

{-
rle :: String -> String
rle = undefined
rle "" = ""
rle (h:t) = aux 1 h t where  -- 최초의 1문자를 기억하고, 1문자가 나왔으므로 연장은 1
    aux :: Int -> Char -> String -> String
    aux runLength prevChar "" = prevChar : show runLength    -- 나머지가 없다면 끝
    aux runLength prevChar (c:s)                             -- 나머지가 있을 경우
        | c == prevChar = aux (runLength + 1) prevChar s     -- 같은 문자라면 연장을 카운트 업
        | otherwise = prevChar : shows runLength (aux 1 c s) -- 다른문자라면 새롭게 1부터 카운트

rle = concatMap (\s -> head s : show (length s)) . group

rle = fromCharAndRunLength . toCharAndRunLength
rle = concat . map rl2str . map toPair . group
rle = concat . map (rl2str . toPair) . group
rle = concatMap (rl2str . toPair) . group
rle = concatMap (\s -> rl2str (toPair s)) . group
rle = concatMap (\s -> rl2str (head s, length s)) . group

-- | 문자와 그 연장의 쌍의 리스트를 출력 문자열로 변환한다.
fromCharAndRunLength :: [(Char, Int)] -> String
fromCharAndRunLength = concatMap rl2str

rl2str :: (Char, Int) -> String
rl2str (c,n) = c : show n

-- | 입력 문자열을 문자와 그 연장의 쌍의 리스트로 변환한다.
toCharAndRunLength :: String -> [(Char, Int)]
toCharAndRunLength = map toPair . group

toPair :: String -> (Char, Int)
toPair str = (head str, length str)
-}

rle :: String -> String
rle = concatMap (\s -> head s : show (length s)) . group
