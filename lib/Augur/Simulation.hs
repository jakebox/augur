module Augur.Simulation (
    initState,
    updateMonth,
    simulate,
    monthlyExpenses,
    calculateEmergencyFund,
) where

import Augur.Types
import Data.Decimal
import Data.Map qualified as M
import Data.Time.Calendar.Month
import GHC.Conc (retry)

initState :: Month -> MonthState
initState startMonth = MonthState (addMonths (-1) startMonth) 0 0 0 0 0 roth trad 0
  where
    roth = Account 0 0 0 Roth
    trad = Account 0 0 0 Traditional

monthlyExpenses :: ModelConfig -> Money
monthlyExpenses config = sum $ map snd config.expenses

calculateTaxes :: ModelConfig -> Money -> Money
calculateTaxes config taxableIncome = taxableIncome * config.effectiveTaxRate

calculateReturnMonth :: ModelConfig -> Account -> Money
calculateReturnMonth config acc = realToFrac $ balanceDouble * monthlyFactor
  where
    balanceDouble = realToFrac acc.balance :: Double
    monthlyFactor = (1 + realToFrac config.annualReturn :: Double) ** (1/12) - 1

updateMonth :: ModelConfig -> MonthState -> MonthState
updateMonth config prev =
    MonthState
        { month
        , income = netIncome
        , totalExpenses
        , netChange
        , cashBalance
        , emergencyFundBalance
        , roth401k
        , trad401k
        , taxes = prev.taxes + taxes
        }
  where
    month = addMonths 1 prev.month
    grossIncome = config.salary / 12
    taxableIncome = grossIncome - trad401kContrib

    -- netIncome: Income after tax
    taxes = calculateTaxes config taxableIncome
    netIncome = taxableIncome - taxes

    totalExpenses = monthlyExpenses config
    -- netChange: Income after expenses
    netChange = netIncome - totalExpenses

    -- Retirement is calculated based on income
    trad401kContrib = grossIncome * config.trad401kContrib
    roth401kContrib = netIncome * config.roth401kContrib

    trad401kGrowth = calculateReturnMonth config prev.trad401k
    roth401kGrowth = calculateReturnMonth config prev.roth401k

    trad401k =
        Account
            { balance = prev.trad401k.balance + trad401kContrib + trad401kGrowth
            , contributions = prev.trad401k.contributions + trad401kContrib
            , gains = prev.trad401k.gains + trad401kGrowth
            , accountType = Traditional
            }

    roth401k =
        Account
            { balance = prev.roth401k.balance + roth401kContrib + roth401kGrowth
            , contributions = prev.roth401k.contributions + roth401kContrib
            , gains = prev.roth401k.gains + roth401kGrowth
            , accountType = Roth
            }

    -- Everything else is calculated after tax, after expenses, using money from netChange
    cashBalance = prev.cashBalance + netChange - emergencyFundContrib
    emergencyFundContrib = emergencyFundBalance - prev.emergencyFundBalance
    emergencyFundBalance = updateEmergencyFundBalance prev.emergencyFundBalance (calculateEmergencyFund config) netChange

updateEmergencyFundBalance :: Money -> Money -> Money -> Money
updateEmergencyFundBalance curr target income
    | curr >= target = curr
    | otherwise = min (curr + (income * flex_pct)) target
  where
    flex_pct = 0.7

calculateEmergencyFund :: ModelConfig -> Money
calculateEmergencyFund c = n * e
  where
    e = monthlyExpenses c
    n = realToFrac c.emergencyFundMonths

simulate :: Int -> ModelConfig -> MonthState -> [MonthState]
simulate n config initial = take n $ tail $ iterate (updateMonth config) initial
