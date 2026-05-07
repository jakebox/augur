module Augur.Config (defaultConfig, defaultExpenses) where

import Augur.Types
import Data.Time.Calendar.Month

defaultExpenses :: [(String, Money)]
defaultExpenses =
    [ ("Rent", 2200)
    , ("Utilities", 150)
    , ("Internet/Phone", 70)
    , ("Groceries", 220)
    , ("Dining Out", 200)
    , ("Transportation", 300)
    , ("Car Insurance", 165)
    , ("Health Insurance", 60)
    , ("Discretionary", 200)
    ]

defaultConfig :: ModelConfig
defaultConfig =
    ModelConfig
        { initialSalary = 140_000
        , start
        , retirement
        , taxRate = 0.35
        , expenses = defaultExpenses
        , trad401kContrib = 0.03
        , roth401kContrib = 0.20
        , brokerageContrib = 0.75
        , emergencyFundMonths = 1
        , annualReturn = 0.08
        , salaryGrowthRate = 0.04
        , inflationRate = 0.03
        }
  where
    start = MkMonth $ (2026 * 12) + (5 - 1)
    retirement = addMonths (30 * 12) start
