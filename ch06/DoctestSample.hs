-- DoctestSample.hs
module DoctestSample where

-- | 문자열 중의 스페이스의 개수
--
-- >>> countSpace ""
-- 0
-- >>> countSpace "abracadabra"
-- 0
-- >>> countSpace "Hello, World!"
-- 1
-- >>> countSpace "    "
-- 4
--
-- prop> countSpace s == sum [1| c <- s, c == ' ' ]
--
countSpace :: String -> Int
countSpace = length . filter (' ' ==)
--countSpace = length . filter (\x -> ' ' == x || '\t' == x)

