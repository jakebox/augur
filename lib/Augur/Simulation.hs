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

calculateReturnMonth :: ModelConfig -> Account -> Money
calculateReturnMonth config acc = realToFrac $ balanceDouble * monthlyFactor
  where
    balanceDouble = realToFrac acc.balance :: Double
    monthlyFactor = (1 + realToFrac config.annualReturn :: Double) ** (1 / 12) - 1

calculateRetirementContributions :: ModelConfig -> Money -> Integer -> (Money, Money)
calculateRetirementContributions config grossIncome yearsElapsed = (trad401kContrib, roth401kContrib)
  where
    -- Calculate 401k limit (grows with inflation)
    monthly401kLimit = (23500 * (1.015 ^ yearsElapsed)) / 12

    -- Calculate desired contributions
    desiredTrad = grossIncome * config.trad401kContrib
    desiredRoth = grossIncome * config.roth401kContrib
    totalDesired = desiredTrad + desiredRoth

    -- Cap contributions proportionally if they exceed limit
    (trad401kContrib, roth401kContrib) =
        if totalDesired > monthlyLimit
            then
                let scale = monthlyLimit / totalDesired
                 in (desiredTrad * scale, desiredRoth * scale)
            else (desiredTrad, desiredRoth)

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

    -- 2. Calculate retiremenet contributions
    (trad401kContrib, roth401kContrib) = calculateRetirementContributions config grossIncome yearsElapsed

    -- 3. Calculate taxes
    taxes = config.taxRate * (grossIncome - trad401kContrib)
    taxableIncome = grossIncome - trad401kContrib
    netIncome = taxableIncome - taxes

    -- 4. Calculate expenses
    expenseMultiplier = (1 + config.inflationRate) ^ yearsElapsed
    totalExpenses = monthlyExpenses config * expenseMultiplier

    -- 5. Calculate cash
    netIncomeAfterRoth = netIncome - roth401kContrib
    netChange = netIncomeAfterRoth - totalExpenses

    -- 6. Emergency fund
    emergencyFundTarget = calculateEmergencyFund config
    emergencyFundContrib = if prev.emergencyFundBalance >= emergencyFundTarget
                           then 0
                           else min (netChange * 0.7) (emergencyFundTarget - prev.emergencyFundBalance)
    emergencyFundBalance = prev.emergencyFundBalance + emergencyFundContrib

    -- 7. Brokerage (gets whatever is left)
    availableForBrokerage = netChange - emergencyFundContrib
    brokerageContrib = max 0 (availableForBrokerage * config.brokerageContrib)

    -- 8. Cash
    cashBalance = prev.cashBalance + availableForBrokerage - brokerageContrib

    -- 9. Account growth
    trad401kGrowth = calculateReturnMonth config prev.trad401k
    roth401kGrowth = calculateReturnMonth config prev.roth401k
    brokerageGrowth = calculateReturnMonth config prev.brokerage

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

    brokerage =
        Account
            { balance = prev.brokerage.balance + brokerageContrib + brokerageGrowth
            , contributions = prev.brokerage.contributions + brokerageContrib
            , gains = prev.brokerage.gains + brokerageGrowth
            , accountType = Taxable
            }

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
