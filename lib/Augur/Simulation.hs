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

initState :: ModelConfig -> MonthState
initState config = MonthState (addMonths (-1) config.start) 0 0 0 0 0 roth trad brokerage 0 config.initialSalary
  where
    roth = Account 0 0 0 Roth
    trad = Account 0 0 0 Traditional
    brokerage = Account 0 0 0 Taxable

monthlyExpenses :: ModelConfig -> Money
monthlyExpenses config = sum $ map snd config.expenses

updateMonth :: ModelConfig -> MonthState -> MonthState
updateMonth config prev =
    MonthState
        { month
        , income = netIncome
        , totalExpenses = expenses
        , netChange
        , cashBalance = prev.cashBalance + netChange - sum savingsMinus 
        , emergencyFundBalance
        , roth401k
        , trad401k
        , brokerage
        , taxes = prev.taxes + taxes
        , salary
        }
  where
    month = addMonths 1 prev.month
    yearsElapsed = diffMonths month config.start `div` 12

    -- 1. Calculate income
    salary = config.initialSalary * ((1 + config.salaryGrowthRate) ^ yearsElapsed)
    grossIncome = salary / 12

    -- 2. Functions to calculate retiremenet contributions
    accountContribution :: Account -> Account -> Money
    accountContribution new old = new.contributions - old.contributions
  
    trad401k = updateAccount config grossIncome yearsElapsed prev.trad401k

    -- 3. Calculate taxes
    taxDeductableItems = [accountContribution trad401k prev.trad401k]
    taxes = calculateTaxes config grossIncome taxDeductableItems
    netIncome = grossIncome - taxes

    -- 4. Contribute to a Roth 401k
    roth401k = updateAccount config netIncome yearsElapsed prev.roth401k
    roth401kContrib = accountContribution roth401k prev.roth401k

    -- 5. Calculate cash after basic expenses
    expenses = calculateExpenses config.expenses yearsElapsed
    netChange = netIncome - expenses - roth401kContrib

    -- 6. Ensure filled emergency fund
    emergencyFundBalance = updateEmergencyFundBalance prev.emergencyFundBalance (calculateEmergencyFund config) netChange
    emergencyFundContrib = emergencyFundBalance - prev.emergencyFundBalance

    -- 7. Contribute to a brokerage account after emergency fund
    availableForBrokerage = max 0 (netChange - emergencyFundContrib)
    brokerage = updateAccount config availableForBrokerage yearsElapsed prev.brokerage

    -- 8. Calculate leftover cash after post-tax expenses and saving
    savingsMinus =
        [ emergencyFundContrib
        , accountContribution brokerage prev.brokerage
        ]

calculateTaxes :: ModelConfig -> Money -> [Money] -> Money
calculateTaxes config gross deductions = config.taxRate * (gross - sum deductions)

calculateExpenses :: [(String, Money)] -> Integer -> Money
calculateExpenses expenses yearsElapsed = sum $ map snd expenses

updateAccount :: ModelConfig -> Money -> Integer -> Account -> Account
updateAccount config pool yearsElapsed account = case account.accountType of
    Roth ->
        Account
            { balance = account.balance + contribution + gain
            , contributions = account.contributions + contribution
            , gains = account.gains + gain
            , accountType = Roth
            }
      where
        contribution = max (config.roth401kContrib * pool) (monthly401kLimit * config.roth401kContrib)
    Traditional ->
        Account
            { balance = account.balance + contribution + gain
            , contributions = account.contributions + contribution
            , gains = account.gains + gain
            , accountType = Traditional
            }
      where
        contribution = max (config.trad401kContrib * pool) (monthly401kLimit * config.trad401kContrib)
    Taxable ->
        Account
            { balance = account.balance + contribution + gain
            , contributions = account.contributions + contribution
            , gains = account.gains + gain
            , accountType = Taxable
            }
      where
        contribution = config.brokerageContrib * pool
        
  where
    gain = calculateReturnMonth config account
    monthly401kLimit = (23500 * (1.015 ^ yearsElapsed)) / 12

calculateReturnMonth :: ModelConfig -> Account -> Money
calculateReturnMonth config acc = realToFrac $ balanceDouble * monthlyFactor
  where
    balanceDouble = realToFrac acc.balance :: Double
    monthlyFactor = (1 + realToFrac config.annualReturn :: Double) ** (1 / 12) - 1

updateEmergencyFundBalance :: Money -> Money -> Money -> Money
updateEmergencyFundBalance curr target available
    | curr >= target = curr
    | otherwise = min (curr + (available * flex_pct)) target
  where
    flex_pct = 0.7

calculateEmergencyFund :: ModelConfig -> Money
calculateEmergencyFund c = n * e
  where
    e = monthlyExpenses c
    n = realToFrac c.emergencyFundMonths

simulate :: Int -> ModelConfig -> MonthState -> [MonthState]
simulate n config initial = take n $ drop 1 $ iterate (updateMonth config) initial
