{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Logic where

import InferenceRule

-- | 올바른 해답
--
-- 전제를 의미하는 타입에서 올바른 해답을 의미하는 타입의 결과를 던는 함수는 구현할 수 있다.
--
correctAnswer :: ( Chinpei `NotEat` Soba -- 진평의 증언: 「국수라면 부족해」라는 말로부터, 진평은 국수를 먹지 않는다.
                 , Kanta `NotEat` Curry  -- 강타의 증언: 「카레라이스 싫어」라는 말로부터, 강타는 카레를 먹지 않는다.
                 , Kanta `NotEat` Soba   -- 강타의 증언: 「국수도 싫어」라는 말로부터, 강타는 국수를 먹지 않는다.
                 ) -- 이러한 위의 세 개의 「전제」로부터 결론은 다음과 같이 된다.
              -> ( Shion `Eat` Soba      -- 시온은 국수를 먹었다.
                 , Chinpei `Eat` Curry   -- 진평은 카레를 먹었다.
                 , Kanta `Eat` Ramen     -- 강타는 라면을 먹었다.
                 )
correctAnswer (chinpeiUnsatisfySoba, kantaHateCurry, kantaHateSoba) =
    (shionEatSoba, chinpeiEatCurry, kantaEatRamen) where
    -- 강타는 카레와 국수가 싫으므로, 라면을 먹었다.
    kantaEatRamen = EatRemainFood kantaHateCurry kantaHateSoba
    -- 라면은 강타가 먹었으므로, 진평은 라면을 먹지 않는다.
    chinpeiNotEatRamen = AnotherPeopleNotEat kantaEatRamen
    -- 진평은 국수로는 만족하지 못하고, 라면도 먹지 않으므로, 케레를 먹었다.
    chinpeiEatCurry = EatRemainFood chinpeiUnsatisfySoba chinpeiNotEatRamen
    -- 카레는 진평이 먹었으므로, 시온은 카레를 먹지 않았다.
    shionNotEatCurry = AnotherPeopleNotEat chinpeiEatCurry
    -- 라면은 강타가 먹었으므로, 시온은 라면을 먹지 않았다.
    shionNotEatRamen = AnotherPeopleNotEat kantaEatRamen
    -- 시온은 카레도 라면도 먹지 않았으므로, 국수를 먹었다.
    shionEatSoba = EatRemainFood shionNotEatCurry shionNotEatRamen
