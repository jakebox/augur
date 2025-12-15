module Augur.Types (
    Money,
    AccountBalances,
    ModelConfig (..),
    MonthState (..),
    Account (..),
    AccountType (..),
    TaxBracket (..),
    AccountUpdate (..),
) where

import Data.Decimal
import Data.Map qualified as M
import Data.Time.Calendar.Month
import GHC.Generics (Generic)

type Money = Decimal

type AccountBalances = M.Map String Money
type AccountAllocations = M.Map String Decimal

data AccountType = Roth | Traditional | Taxable | Cash | Emergency
    deriving (Show, Generic)

data AccountUpdate = AccountUpdate
    { contribution :: Money
    , account :: Account
    }

data TaxBracket = TaxBracket
    { threshold :: Money
    , rate :: Decimal
    }
    deriving (Show)

data ModelConfig = ModelConfig
    { initialSalary :: Money
    , start :: Month
    , taxRate :: Decimal
    , expenses :: [(String, Decimal)]
    , trad401kContrib :: Decimal
    , roth401kContrib :: Decimal
    , brokerageContrib :: Decimal
    , emergencyFundMonths :: Integer
    , annualReturn :: Decimal
    , salaryGrowthRate :: Decimal
    , inflationRate :: Decimal
    , retirement :: Month
    }
    deriving (Show)

data Account = Account
    { balance :: Money
    , contributions :: Money
    , gains :: Money
    , accountType :: AccountType
    }
    deriving (Show, Generic)

data MonthState = MonthState
    { month :: Month
    , income :: Money
    , totalExpenses :: Money
    , trad401k :: Account
    , roth401k :: Account
    , brokerage :: Account
    , cash :: Account
    , emergencyFund :: Account
    , taxes :: Money
    , salary :: Money
    }
    deriving (Show, Generic)
