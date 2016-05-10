-- Sample.hs
module Sample where

import PowerOf2

-- | 3은 2의 거듭제곱이 아니지만 (생성자에 나타나 있으므로) 2의 거듭제곱 타입을 가진 값으로서 만들어 버린다.
-- 그러나 잘못된 값이므로 example은 "*** Exception: 3 must be a power of 2." 를 출력한다.
example :: IO ()
--example = print (exponentPowerOf2 (PowerOf2 3))
example = case makePowerOf2 3 of
            Just n -> print (exponentPowerOf2 n)
            Nothing -> putStrLn "3 is invalid"

