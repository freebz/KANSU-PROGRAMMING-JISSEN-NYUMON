-- PowerOf2.hs
module PowerOf2
    ( PowerOf2          -- 타입만을 노출시킨다 (생성자를 은페한다)
    -- PowerOf2(..)     -- 이거라면 생성자까지 보인다.
    , makePowerOf2      -- '생성 함수도 노출시킨다.
    , exponentPowerOf2  -- ''exponentPowerOf2 함수도 노출시킨다.
    ) where             -- 모듈을 정의하려면 소스의 처음에 이렇게 정의한다.

-- | 2의 거듭제곱 타입(을 새로 도입)
newtype PowerOf2 = PowerOf2 Integer deriving (Eq, Show)

-- | 정수 값이 2의 거듭제곱인지 아닌지 판정
isPowerOf2 :: Integer -> Bool
isPowerOf2 n
    | n == 1 = True
    | n < 1 || odd n = False
    | otherwise = isPowerOf2 (n `div` 2)

-- | 정수 값에서 2의 거듭제곱 타입으로 변환
--
-- >>> makePowerOf2 1
-- Just (PowerOf2 1)
-- >>> makePowerOf2 2
-- Just (PowerOf2 2)
-- >>> makePowerOf2 3
-- Nothing
-- >>> makePowerOf2 1024
-- Just (PowerOf2 1024)

makePowerOf2 :: Integer -> Maybe PowerOf2
makePowerOf2 n
    | isPowerOf2 n = Just (PowerOf2 n) -- 2의 거듭제곱이라면 PowerOf2 타입의 값을 생성한다.
    | otherwise = Nothing              -- 그렇지 않다면 아무것도 얻을 수 없다.


-- | 2의 거듭제곱에 대해서 그것이 2의 몇 제곱인지를 얻는다.
--   단, 그 이외일 때는 오류이므로 2의 거듭제곱 이외를 부여해서는 안 된다.
--
-- >>> exponentPowerOf2 1
-- 0
-- >>> exponentPowerOf2 2
-- 1
-- >>> exponentPowerOf2 3
-- *** Exception: 3 must be a power of 2.
-- >>> exponentPowerOf2 4
-- 2
-- >>> exponentPowerOf2 1024
-- 10
-- >>> exponentPowerOf2 (-1)
-- *** Exception: -1 must be a power of 2.

{-
exponentPowerOf2 :: Integer -> Integer
exponentPowerOf2 = exponentPowerOf2' 0 where
    exponentPowerOf2' :: Integer -> Integer -> Integer
    exponentPowerOf2' r n
        | n == 1 = r
        | n < 1 || odd n = error (shows n " must be a power of 2.")
        | otherwise = exponentPowerOf2' (r + 1) (n `div` 2)

exponentPowerOf2 :: PowerOf2 -> Integer
exponentPowerOf2 (PowerOf2 n) = exponentPowerOf2' 0 n where
    exponentPowerOf2' :: Integer -> Integer -> Integer
    exponentPowerOf2' r n
        | n == 1         = r
        | n < 1 || odd n = error (shows n " must be a power of 2.")
        | otherwise      = exponentPowerOf2' (r + 1) (n `div` 2)
-}

exponentPowerOf2 :: PowerOf2 -> Integer -- 입력의 타입을 Integer에서 PowerOf2로 변경
exponentPowerOf2 (PowerOf2 n) = exponentPowerOf2' 0 n where
    exponentPowerOf2' :: Integer -> Integer -> Integer
    exponentPowerOf2' r n
        | n == 1         = r
        -- 더 이상 절대 오류 케이스가 없으므로 오류 케이스를 지운다.
        -- | n < 1 || odd n = error (shows n " must be a power of 2.")
        | otherwise      = exponentPowerOf2' (r + 1) (n `div` 2)
