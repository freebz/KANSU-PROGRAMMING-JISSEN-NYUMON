{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE EmptyCase #-}
-- 추론 규칙
module InferenceRule
    ( People(..)
    , Food(..)
    , Eat(..)
    , NotEat(..)
    ) where

data (:==:) :: k -> k -> * where -- 타입의 등가성
    Refl :: a :==: a -- 반사율

data Bottom -- 모순

type Not p = p -> Bottom  -- 부정

type a :/=: b = Not (a :==: b) -- 동등한 타입이 아니다.

class (a :: k) `Neq` (b :: k) where
    neq :: a :/=: b
    neq x = case x of {}

-- 인물
data People = Shion    -- 시온
            | Chinpei  -- 진평
            | Kanta    -- 강타
              deriving (Eq, Show)

class IsPeople (people :: People)
instance IsPeople Shion
instance IsPeople Chinpei
instance IsPeople Kanta
instance Shion `Neq` Chinpei
instance Shion `Neq` Kanta
instance Chinpei `Neq` Kanta
instance Chinpei `Neq` Shion
instance Kanta `Neq` Shion
instance Kanta `Neq` Chinpei

-- 먹을 것
data Food = Curry -- 카레
          | Soba  -- 국수
          | Ramen -- 라면
            deriving (Eq, Show)

class IsFood (food :: Food)
instance IsFood Curry
instance IsFood Soba
instance IsFood Ramen

instance Curry `Neq` Soba
instance Curry `Neq` Ramen
instance Soba  `Neq` Ramen
instance Soba  `Neq` Curry
instance Ramen `Neq` Curry
instance Ramen `Neq` Soba

-- 「누군가가 무언가를 먹었다」라는 타입
data Eat :: People -> Food -> * where
    -- 추론 규칙: 어떤 사람이 먹을 것1과 먹을 것2를 먹지 않았다면, 먹을 것 3을 먹었을 것이다
    EatRemainFood :: ( IsPeople p  -- p는 사람
                     , IsFood f1   -- f1은 먹을 것
                     , IsFood f2   -- f2는 먹을 것
                     , IsFood f3   -- f3은 먹을 것
                     , f1 `Neq` f2 -- f1과 f2는 다른 먹을 것
                     , f2 `Neq` f3 -- f2와 f3은 다른 먹을 것
                     , f3 `Neq` f1 -- f3과 f1은 다른 먹을 것
                     ) =>
                     p `NotEat` f1 -- p는 f1을 먹지 않았다.
                  -> p `NotEat` f2 -- p는 f2를 먹지 않았다.
                  -> p `Eat` f3    -- 그렇다면 p는 f3을 먹었을 것이다.
    -- 추론 규칙: 어떤 먹을 것은 사람1과 사람2가 먹지 않았다면, 사람3이 먹었을 것이다.
    RemainPeopleEat :: ( IsPeople p1 -- p1은 사람
                     , IsPeople p2   -- p2는 사람
                     , IsPeople p3   -- p3은 사람
                     , IsFood f      -- f는 먹을 것
                     , p1 `Neq` p2   -- p1과 p2는 다른 사람
                     , p2 `Neq` p3   -- p2와 p3은 다른 사람
                     , p3 `Neq` p1   -- p3과 p1은 다른 사람
                     ) =>
                     p1 `NotEat` f   -- p1은 f를 먹지 않았다.
                  -> p2 `NotEat` f   -- p2는 f를 먹지 않았다.
                  -> p3 `Eat` f      -- 그렇다면 p3은 f을 먹었을 것이다.

-- 「누군가가 무언가를 먹지 않았다」라는 타입
data NotEat :: People -> Food -> * where
    -- 추론 규칙: 어떤 사람이 먹을 것1을 먹었다면, 다른 음식은 먹지 않는다.
    NotEatAnotherFood :: ( IsPeople p     -- p는 사람
                         , IsFood f1      -- f1은 먹을 것
                         , IsFood f2      -- f2는 먹을 것
                         , f1 `Neq` f2    -- f1과 f2는 다른 먹을 것
                         ) =>
                         p `Eat` f1       -- p는 f1을 먹었다.
                      -> p `NotEat` f2    -- 그렇다면 p는 f2를 먹지 않았을 것이다.
    -- 추론 규칙: 어떤 먹을 것을 어떤 사람이 먹었다면, 다른 사람은 먹지 않는다.
    AnotherPeopleNotEat :: ( IsPeople p1  -- p1은 사람
                           , IsPeople p2  -- p2는 사람
                           , IsFood f     -- f는 먹을 것
                           , p1 `Neq` p2  -- p1과 p2는 다른 사람
                           ) =>
                           p1 `Eat` f     -- p1은 f를 먹었다.
                        -> p2 `NotEat` f  -- 그렇다면 p2는 f를 먹지 않을 것이다.
