-- VisitorIsTooLarge.hs
-- $ ghc --make VisitorIsTooLarge
-- $ ./VisitorIsTooLarge
-- 1 + (2 + 3)^2
-- 26

-- 식
data Expr a = Plus (Expr a) (Expr a) -- 덧셈의 식
            | Square (Expr a)        -- 제곱의 식
            | Number a               -- 숫자의 식

-- 식의 평가를 실시하는 함수
evalExpr :: Expr Int -> Int
evalExpr (Plus e1 e2) = evalExpr e1 + evalExpr e2
evalExpr (Square e)   = evalExpr e ^ (2 :: Int)
evalExpr (Number n)   = n

-- 식을 문자열로 하는 함수
showExpr :: Expr Int -> String
showExpr (Plus e1 e2) = showExpr e1 ++ " + " ++ showExpr e2
showExpr (Square e)   = "(" ++ showExpr e ++ ")^2"
showExpr (Number n)   = show n

main :: IO ()
main = do
  -- e = 1 (2 + 3)^2
  -- 실제로는 구문 분석 등에 의해 좀 더 크고 복잡한 것을 가정
  let e = Plus (Number 1) (Square (Plus (Number 2) (Number 3)))
  putStrLn (showExpr e)
  print (evalExpr e)
