module Augur.Types (Money, ModelConfig (..), MonthState (..)) where

import Data.Decimal
import Data.Time.Calendar.Month

type Money = Decimal

data ModelConfig = ModelConfig
    { salary :: Money
    , effectiveTaxRate :: Decimal
    , expenses :: [(String, Decimal)]
    , retirementSavingPct :: [(String, Decimal)]
    }
    deriving (Show)

data MonthState = MonthState
    { month :: Month
    , income :: Money
    , totalExpenses :: Money
    , netChange :: Money
    , cashBalance :: Money
    }
    deriving (Show)
