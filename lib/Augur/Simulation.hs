module Augur.Simulation (
    initState,
    updateMonth,
    simulate,
    monthlyExpenses,
) where

import Augur.Calculations
import Augur.Types

import Data.Decimal
import Data.List (mapAccumL)
import Data.Map qualified as M
import Data.Time.Calendar.Month
import Debug.Trace

initState :: ModelConfig -> MonthState
initState config = MonthState (addMonths (-1) config.start) 0 0 trad roth brokerage cash emergencyFund 0 config.initialSalary
  where
    roth = Account 0 0 0 Roth
    trad = Account 0 0 0 Traditional
    brokerage = Account 0 0 0 Taxable
    cash = Account 0 0 0 Cash
    emergencyFund = Account 0 0 0 Emergency

updatePreTax :: ModelConfig -> MonthState -> Integer -> (MonthState, Money)
updatePreTax config prev yrs =
    let
        -- 1. Calculate month's gross income
        grossIncome = calculateSalaryMonth config yrs

        -- Traditional 401k
        trad401kUpdate = updateAccountFilled prev.trad401k grossIncome

        -- Calculate taxes and net income
        taxDeductableItems = [trad401kUpdate.contribution]
        taxes = calculateTaxes config grossIncome taxDeductableItems yrs
        income = grossIncome - taxes

        remainder = income - trad401kUpdate.contribution
     in
        ( MonthState
            { month = prev.month
            , income
            , totalExpenses = prev.totalExpenses
            , roth401k = prev.roth401k
            , trad401k = trad401kUpdate.account
            , brokerage = prev.brokerage
            , emergencyFund = prev.emergencyFund
            , cash = prev.cash
            , taxes
            , salary = grossIncome * 12
            }
        , remainder
        )
  where
    updateAccountFilled account money = updateAccount config money yrs account

updatePostTax :: ModelConfig -> MonthState -> Integer -> Money -> MonthState
updatePostTax config afterTax yrs bal =
    let
        -- Calculate take-home cash after taxes/pre-tax contributions and basic expenses
        totalExpenses = calculateExpenses config yrs
        remainder = bal - totalExpenses

        -- Contribute to emergency fund
        contrib = calculateEmergencyFundContribution config afterTax.emergencyFund remainder
        emergencyFundUpdate = updateAccountFilled afterTax.emergencyFund contrib

        remainder' = remainder - emergencyFundUpdate.contribution

        (_, [newRoth, newBrokerage, newCash]) =
            mapAccumL processAccount remainder' [afterTax.roth401k, afterTax.brokerage, afterTax.cash]
     in
        MonthState
            { month = afterTax.month
            , income = afterTax.income
            , totalExpenses
            , roth401k = newRoth
            , trad401k = afterTax.trad401k
            , brokerage = newBrokerage
            , emergencyFund = emergencyFundUpdate.account
            , cash = newCash
            , taxes = afterTax.taxes
            , salary = afterTax.salary
            }
  where
    updateAccountFilled account money = updateAccount config money yrs account
    processAccount :: Money -> Account -> (Money, Account)
    processAccount remainder account = (max 0 remainder - updatedAcc.contribution, updatedAcc.account)
      where
        updatedAcc = updateAccountFilled account remainder

updateMonth :: ModelConfig -> MonthState -> MonthState
updateMonth config prev =
    let
        -- 0. Basic time details
        month = addMonths 1 prev.month
        yearsElapsed = diffMonths month config.start `div` 12

        -- 1. Pre-tax update
        (preTax, remainder) = updatePreTax config prev yearsElapsed

        -- 2. Post-tax update
        postTax :: MonthState = updatePostTax config preTax yearsElapsed remainder
     in
        MonthState
            { month
            , income = preTax.income
            , totalExpenses = postTax.totalExpenses
            , roth401k = postTax.roth401k
            , trad401k = preTax.trad401k
            , brokerage = postTax.brokerage
            , emergencyFund = postTax.emergencyFund
            , cash = postTax.cash
            , taxes = preTax.taxes
            , salary = preTax.salary
            }

updateAccount :: ModelConfig -> Money -> Integer -> Account -> AccountUpdate
updateAccount config pool yearsElapsed account =
    AccountUpdate
        contribution
        Account
            { balance = account.balance + contribution + gain
            , contributions = account.contributions + contribution
            , gains = account.gains + gain
            , accountType = account.accountType
            }
  where
    gain = calculateReturnMonth config account
    contribution = calculateContribution config pool yearsElapsed account.accountType

simulate :: Int -> ModelConfig -> MonthState -> [MonthState]
simulate n config initial = take n $ drop 1 $ iterate (updateMonth config) initial
