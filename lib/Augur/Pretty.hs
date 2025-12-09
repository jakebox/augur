module Augur.Pretty (
    printSummary,
    printMonthState,
    printSimulation,
    formatMoney,
) where

import Augur.Types

import Augur.Simulation (calculateEmergencyFund, monthlyExpenses)
import Data.Decimal
import Text.Printf
import qualified Data.Map as M

-- Prints the money amount as a string, padded with spaces to 10 characters wide, 
-- with 0 decimal places.
formatMoney :: Money -> String
formatMoney amount = printf "%10.0f" (realToFrac amount :: Double)

-- Helper function to format an individual Account's details
formatAccount :: Account -> String
formatAccount a =
    "Balance: $" ++ formatMoney a.balance ++
    " | Contrib: $" ++ formatMoney a.contributions ++
    " | Gains: $" ++ formatMoney a.gains

printSummary :: ModelConfig -> IO ()
printSummary config = do
    putStrLn "Monthly Summary:"
    putStrLn $ "  Gross Monthly: $" ++ formatMoney (config.initialSalary / 12)
    -- putStrLn $ "  Net Monthly: $" ++ formatMoney (monthlyNetIncome config)
    putStrLn $ "  Total Expenses: $" ++ formatMoney (monthlyExpenses config)
    -- putStrLn $ "  Monthly Savings: $" ++ formatMoney (monthlyNetIncome config - monthlyExpenses config)
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
    -- 1. Print the header using printf for alignment
    printf "%-5s %13s %13s %10s %10s %10s %10s %10s\n"
        ("Month" :: String)
        ("Income" :: String)
        ("Expenses" :: String)
        ("Net" :: String)
        ("Balance" :: String)
        ("E-Fund" :: String)
        ("Taxes" :: String)
        ("Salary" :: String)
    putStrLn $ replicate 73 '-' -- Separator line
    
    mapM_ printRow states
  where
    -- Header variable is no longer needed

    printRow :: MonthState -> IO ()
    printRow s = do
        -- 1. Print the main row with fixed-width money values (widths match the header)
        putStrLn $
            printf "%-5s %s %s %s %s %s %s %s"
                (show s.month)  -- 5 wide (left aligned)
                (formatMoney s.income)
                (formatMoney s.totalExpenses)
                (formatMoney s.netChange)
                (formatMoney s.cashBalance)
                (formatMoney s.emergencyFundBalance)
                (formatMoney s.taxes)
                (formatMoney s.salary)

        -- 2. Print the detailed retirement accounts on new, indented lines
        putStrLn $ "\n    Traditional 401k: " ++ formatAccount s.trad401k
        putStrLn $ "    Roth 401k:        " ++ formatAccount s.roth401k
        putStrLn $ "    Brokerage :        " ++ formatAccount s.brokerage
        putStrLn "" -- Add a blank line for visual separation between months
