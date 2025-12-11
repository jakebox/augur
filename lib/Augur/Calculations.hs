module Augur.Calculations (
    calculateReturnMonth,
    calculateEmergencyFund,
    monthlyExpenses,
    calculateTaxes,
    calculateExpenses,
    calculateContribution,
    calculateSalaryMonth,
    calculateEmergencyFundContribution,
    accountContribution,
) where

import Augur.Types
import Data.Decimal
import Data.Map qualified as M
import Data.Time.Calendar.Month

calculateReturnMonth :: ModelConfig -> Account -> Money
calculateReturnMonth config acc = realToFrac $ balanceDouble * monthlyFactor
  where
    balanceDouble = realToFrac acc.balance :: Double
    monthlyFactor = (1 + realToFrac config.annualReturn :: Double) ** (1 / 12) - 1

calculateEmergencyFundContribution :: ModelConfig -> Account -> Money -> Money
calculateEmergencyFundContribution config account available
    | curr >= target = curr
    | otherwise = min (curr + (available * flex_pct)) target
  where
    target = calculateEmergencyFund config
    curr = account.balance
    flex_pct = 0.7

calculateEmergencyFund :: ModelConfig -> Money
calculateEmergencyFund c = realToFrac c.emergencyFundMonths * monthlyExpenses c

monthlyExpenses :: ModelConfig -> Money
monthlyExpenses config = sum $ map snd config.expenses

calculateTaxes :: ModelConfig -> Money -> [Money] -> Integer -> Money
calculateTaxes config gross deductions yearsElapsed = (config.taxRate / timeMultiplier) * (gross - sum deductions)
  where
    timeMultiplier = config.inflationRate ^ yearsElapsed

calculateExpenses :: ModelConfig -> Integer -> Money
calculateExpenses config yearsElapsed = sum $ map snd expenses
  where
    adjust = (1 + config.inflationRate) ^ yearsElapsed
    expenses = config.expenses

{- FOURMOLU_DISABLE -}
calculateContribution :: ModelConfig -> Money -> Integer -> AccountType -> Money
calculateContribution config pool yearsElapsed accountType =
    case accountType of
        Roth        -> min (config.roth401kContrib * pool) monthly401kLimit
        Traditional -> min (config.trad401kContrib * pool) monthly401kLimit
        Taxable     -> config.brokerageContrib * pool
        Cash        -> pool
  where
    monthly401kLimit = (23_500 * (1.015 ^ yearsElapsed)) / 12
{- FOURMOLU_ENABLE -}

accountContribution :: Account -> Account -> Money
accountContribution new old = new.contributions - old.contributions

calculateSalaryMonth :: ModelConfig -> Integer -> Money
calculateSalaryMonth config yearsElapsed = (config.initialSalary * ((1 + config.salaryGrowthRate) ^ yearsElapsed)) / 12
