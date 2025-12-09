module Augur.Simulation
    ( initState
    , updateMonth
    , simulate
    , monthlyNetIncome
    , monthlyExpenses
    ) where

import Augur.Types
import Data.Time.Calendar.Month

initState :: Month -> MonthState
initState startMonth = MonthState (addMonths (-1) startMonth) 0 0 0 0

monthlyNetIncome :: ModelConfig -> Money
monthlyNetIncome config = monthlySalary * taxPercent * retirementAdjust
  where
    monthlySalary = config.salary / 12
    taxPercent = 1 - config.effectiveTaxRate
    retirementAdjust = 1 - sum (map snd config.retirementSavingPct)

monthlyExpenses :: ModelConfig -> Money
monthlyExpenses config = sum $ map snd config.expenses

updateMonth :: ModelConfig -> MonthState -> MonthState
updateMonth config prev = MonthState { month, income, totalExpenses, netChange, cashBalance }
  where
    month = addMonths 1 prev.month
    income = monthlyNetIncome config
    totalExpenses = monthlyExpenses config
    netChange = income - totalExpenses
    cashBalance = prev.cashBalance + netChange

simulate :: Int -> ModelConfig -> MonthState -> [MonthState]
simulate n config initial = take n $ tail $ iterate (updateMonth config) initial
