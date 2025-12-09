module Augur.Types (Money, AccountBalances, ModelConfig (..), MonthState (..), Account (..), AccountType(..)) where

import Data.Decimal
import Data.Map qualified as M
import Data.Time.Calendar.Month

type Money = Decimal

type AccountBalances = M.Map String Money
type AccountAllocations = M.Map String Decimal

data AccountType = Roth | Traditional | Taxable
    deriving (Show)

data ModelConfig = ModelConfig
    { salary :: Money
    , effectiveTaxRate :: Decimal
    , expenses :: [(String, Decimal)]
    , trad401kContrib :: Decimal
    , roth401kContrib :: Decimal
    , emergencyFundMonths :: Integer
    , annualReturn :: Decimal
    , salaryGrowthRate :: Decimal
    , inflationRate :: Decimal
    }
    deriving (Show)

data Account = Account
    { balance :: Money
    , contributions :: Money
    , gains :: Money
    , accountType :: AccountType
    }
    deriving (Show)

data MonthState = MonthState
    { month :: Month
    , income :: Money
    , totalExpenses :: Money
    , netChange :: Money
    , cashBalance :: Money
    , emergencyFundBalance :: Money
    , trad401k :: Account
    , roth401k :: Account
    , taxes :: Money
    }
    deriving (Show)
