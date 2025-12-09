module Augur.Pretty (
    printSummary,
    printMonthState,
    printSimulation,
    formatMoney,
) where

import Augur.Types

import Augur.Simulation (calculateEmergencyFund, monthlyExpenses, monthlyNetIncome)
import Data.Decimal
import Text.Printf

formatMoney :: Money -> String
formatMoney amount = printf "%.0f" (realToFrac amount :: Double)

printSummary :: ModelConfig -> IO ()
printSummary config = do
    putStrLn "Monthly Summary:"
    putStrLn $ "  Gross Monthly: $" ++ formatMoney (config.salary / 12)
    putStrLn $ "  Net Monthly: $" ++ formatMoney (monthlyNetIncome config)
    putStrLn $ "  Total Expenses: $" ++ formatMoney (monthlyExpenses config)
    putStrLn $ "  Monthly Savings: $" ++ formatMoney (monthlyNetIncome config - monthlyExpenses config)
    putStrLn $ "  Target Emergency Fund Size: $" ++ formatMoney (calculateEmergencyFund config)
    putStrLn "\nExpense Breakdown:"
    mapM_ (\(name, amt) -> putStrLn $ "  " ++ name ++ ": $" ++ formatMoney amt) config.expenses

printMonthState :: MonthState -> IO ()
printMonthState state = do
    putStrLn $ "Month: " ++ show state.month
    putStrLn $ "Income: $" ++ formatMoney state.income
    putStrLn $ "Expenses: $" ++ formatMoney state.totalExpenses
    putStrLn $ "Net Change: $" ++ formatMoney state.netChange
    putStrLn $ "Cash Balance: $" ++ formatMoney state.cashBalance

printSimulation :: [MonthState] -> IO ()
printSimulation states = do
    putStrLn "\nSimulation Results:"
    putStrLn header
    mapM_ printRow states
  where
    header = "Month\t\tIncome\t\tExpenses\tNet\t\tBalance\t\tEmergency Fund"
    printRow s =
        putStrLn $
            show s.month
                ++ "\t\t$"
                ++ formatMoney s.income
                ++ "\t\t$"
                ++ formatMoney s.totalExpenses
                ++ "\t\t$"
                ++ formatMoney s.netChange
                ++ "\t\t$"
                ++ formatMoney s.cashBalance
                ++ "\t\t$"
                ++ formatMoney s.emergencyFundBalance
