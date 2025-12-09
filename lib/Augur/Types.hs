module Augur.Types (Money, AccountBalances, ModelConfig (..), MonthState (..)) where

import Data.Decimal
import Data.Map qualified as M
import Data.Time.Calendar.Month

type Money = Decimal

type AccountBalances = M.Map String Money
type AccountAllocations = M.Map String Decimal

data ModelConfig = ModelConfig
    { salary :: Money
    , effectiveTaxRate :: Decimal
    , expenses :: [(String, Decimal)]
    , trad401kContrib :: Decimal
    , roth401kContrib :: Decimal
    , emergencyFundMonths :: Integer
    }
    deriving (Show)

data MonthState = MonthState
    { month :: Month
    , income :: Money
    , totalExpenses :: Money
    , netChange :: Money
    , cashBalance :: Money
    , emergencyFundBalance :: Money
    , balTrad401k :: Money
    , balRoth401k :: Money
    , taxes :: Money
    }
    deriving (Show)
