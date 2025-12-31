module Augur.Calculations (
    calculateReturnMonth,
    calculateEmergencyFund,
    monthlyExpenses,
    calculateTaxes,
    calculateExpenses,
    calculateContribution,
    calculateSalaryMonth,
) where

import Augur.Types
import Data.Decimal
import Data.Map qualified as M
import Data.Time.Calendar.Month

calculateReturnMonth :: ModelConfig -> Account -> Money
calculateReturnMonth config acc = realToFrac $ balanceDouble * monthlyFactor
  where
    balanceDouble = realToFrac acc._balance :: Double
    monthlyFactor = (1 + realToFrac config.annualReturn :: Double) ** (1 / 12) - 1

calculateEmergencyFund :: ModelConfig -> Money
calculateEmergencyFund c = realToFrac c.emergencyFundMonths * monthlyExpenses c

monthlyExpenses :: ModelConfig -> Money
monthlyExpenses config = sum $ map snd config.expenses

calculateTaxes :: ModelConfig -> Money -> [Money] -> Integer -> Money
calculateTaxes config gross deductions yearsElapsed = (config.taxRate * timeMultiplier) * (gross - sum deductions)
  where
    timeMultiplier = (1 + config.inflationRate) ^ yearsElapsed

calculateExpenses :: ModelConfig -> Integer -> Money
calculateExpenses config yearsElapsed = sum $ map snd expenses
  where
    adjust = (1 + config.inflationRate) ^ yearsElapsed
    expenses = config.expenses

{- FOURMOLU_DISABLE -}
calculateContribution :: ModelConfig -> Money -> Integer -> Account -> Money
calculateContribution config pool yearsElapsed account =
    case account.accountType of
        Roth        -> min (config.roth401kContrib * pool) monthly401kLimit
        Traditional -> min (config.trad401kContrib * pool) monthly401kLimit
        Taxable     -> config.brokerageContrib * pool
        Cash        -> pool
        Emergency   ->
          let target = calculateEmergencyFund config
              current = account._balance
              needed = max 0 (target - current)
          in min (pool * 0.6) needed
  where
    monthly401kLimit = (23_500 * (1.015 ^ yearsElapsed)) / 12

{- FOURMOLU_ENABLE -}

calculateSalaryMonth :: ModelConfig -> Integer -> Money
calculateSalaryMonth config yearsElapsed = (config.initialSalary * ((1 + config.salaryGrowthRate) ^ yearsElapsed)) / 12
