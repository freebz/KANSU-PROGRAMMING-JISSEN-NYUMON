-- | sudoku.hs
-- $ ghc --make sudoku
-- $ ./sudoku
-- [3,6,7,8,4,1,9,5,2]
-- [2,1,8,7,9,5,4,3,6]
-- [5,9,4,3,2,6,7,8,1]
-- [4,3,1,5,7,2,8,6,9]
-- [9,7,5,6,8,4,1,2,3]
-- [8,2,6,1,3,9,5,4,7]
-- [6,4,2,9,1,8,3,7,5]
-- [1,5,3,4,6,7,2,9,8]
-- [7,8,9,2,5,3,6,1,4]

import Data.List
import Data.Function

-- | 칸(의 좌표)
type Cell = (Int, Int)

-- | 게임 판 상황
type Board = [(Cell, Int)]

-- | 스토쿠의 해결자
solve :: Board -> [Board]
solve board | length board == 81 = [board]
solve board = [ (cell, n) : board
              | let remains = cells \\ map fst board
              , let cell = maximumBy (compare `on` length . used board) remains
              , n <- [1..9] \\ used board cell
              ] >>= solve

-- | 81칸 전체
cells :: [Cell]
cells = [ (x, y) | x <- [0..8], y <- [0..8] ]

-- | 칸이 소속된 구간
area :: Cell -> Int
area (x, y) = y `div` 3 * 3 + x `div` 3

-- | 특정 게임 판의 상황에서 특정 칸의 주변에서 사용되는 숫자를 열거한다.
used :: Board -> Cell -> [Int]
used board cell = nub [ n
                      | (cell', n) <- board
                      , any (\f -> f cell == f cell') [ snd, fst, area]
                      ]

main :: IO ()
main = case solve problem of
         answer : _ -> mapM_ print $ format answer
         []         -> putStrLn "invalid problem"

-- | 해답을 간단히 포매팅한다.
format :: Board -> [[Int]]
format = map (map snd) . transpose . groupBy ((==) `on` (fst . fst)) . sort

-- | 문제
problem :: Board
problem = [ ((3, 0), 8)
          , ((5, 0), 1)
          , ((6, 1), 4)
          , ((7, 1), 3)
          , ((0, 2), 5)
          , ((4, 3), 7)
          , ((6, 3), 8)
          , ((6, 4), 1)
          , ((1, 5), 2)
          , ((4, 5), 3)
          , ((0, 6), 6)
          , ((7, 6), 7)
          , ((8, 6), 5)
          , ((2, 7), 3)
          , ((3, 7), 4)
          , ((3, 8), 2)
          , ((6, 8), 6)
          ]
