{-# LANGUAGE TemplateHaskell #-}

module Augur.Types where

import Data.Decimal
import Data.Map qualified as M
import Data.Time.Calendar.Month
import GHC.Generics (Generic)
import Lens.Micro.TH (makeLenses, makeLensesFor)

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
    { _balance :: Money
    , _contributions :: Money
    , _gains :: Money
    , accountType :: AccountType
    }
    deriving (Show, Generic)

emptyAccount :: AccountType -> Account
emptyAccount = Account 0 0 0

data MonthState = MonthState
    { _month :: Month
    , _income :: Money
    , _totalExpenses :: Money
    , _trad401k :: Account
    , _roth401k :: Account
    , _brokerage :: Account
    , _cash :: Account
    , _emergencyFund :: Account
    , _taxes :: Money
    , _salary :: Money
    }
    deriving (Show, Generic)

makeLenses ''Account
makeLenses ''MonthState
