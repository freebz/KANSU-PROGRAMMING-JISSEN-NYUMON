-- Optional.hs
-- $ ghc --make Optional
-- $ echo "1 + 1" | ./Optional
-- 2
-- $ echo "2 - 1" | ./Optional
-- 1
-- $ echo "3 * 3" | ./Optional
-- 9
-- $ echo "4 / 2" | ./Optional
-- 2
-- $ echo "4 / 0" | ./Optional
-- invalid

-- 문자열을 정수로 변환. 변환할 수 없다면 무효
toNum :: String -> Maybe Int
toNum s = case reads s of
       [(n,"")] -> Just n
       _        -> Nothing

-- 사칙 연산. 연산할 수 없다면 무효
addOp :: Int -> Int -> Maybe Int
addOp a b = Just (a + b)
subOp :: Int -> Int -> Maybe Int
subOp a b = Just (a - b)
mulOp :: Int -> Int -> Maybe Int
mulOp a b = Just (a * b)
divOp :: Int -> Int -> Maybe Int
divOp _ 0 = Nothing
divOp a b = Just (a `div` b)

-- "+","-","*","/" 중 하나의 문자열을 연산으로 변환. 그 외는 무효
toBinOp :: String -> Maybe (Int -> Int -> Maybe Int)
toBinOp "+" = Just addOp
toBinOp "-" = Just subOp
toBinOp "*" = Just mulOp
toBinOp "/" = Just divOp
toBinOp _   = Nothing

eval :: String -> Maybe Int
eval expr = do
  -- 스페이스로 분활, 세 개로 분할할 수 없다면 무효
  -- "1 + 2" -> "1", "+", "2"
  let [ sa, sop, sb ] = words expr
  a <- toNum sa     -- 문자열을 숫자로 변환
  op <- toBinOp sop -- 문자열을 연산자로 변환
  b <- toNum sb     -- 문자열을 숫자로 변환
  a `op` b          -- 숫자 연산 숫자의 계산

main :: IO ()
main = getLine >>= putStrLn . maybe "invalid" show . eval
