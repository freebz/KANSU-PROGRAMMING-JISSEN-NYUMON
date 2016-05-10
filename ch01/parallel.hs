-- parallel.hs
-- parallel 패키지가 필요
-- $ ./parallel +RTS -N4
-- primes: 78498
import Prelude
import Data.Int
import Control.Parallel.Strategies

-- 소수 판정
isPrime :: Int32 -> Bool
isPrime x = all (\n -> x `mod` n /= 0) [ 2 .. toEnum (floor $ sqrt $ fromIntegral x) ]

-- 2부터 1000000까지의 수
arr :: [Int32]
arr = [ 2 .. 1000000 ]

main :: IO ()
main = do

  let arr' = map isPrime arr `using` parListChunk 256 rpar
  -- arr의 각 요소에 isPrime을 적용
  putStr "primes: " >> print (length $ filter id arr')
  -- 소수판정에서 참으로 된 것의 개수 표시
