module Augur.Pretty (
    printSummary,
    printMonthState,
    printSimulation,
    formatMoney,
) where

import Augur.Types

import Augur.Calculations
import Text.Printf

-- Prints the money amount as a string, padded with spaces to 10 characters wide, 
-- with 0 decimal places.
formatMoney :: Money -> String
formatMoney amount = printf "%10.0f" (realToFrac amount :: Double)

-- Helper function to format an individual Account's details
formatAccount :: Account -> String
formatAccount a =
    printf "  %-12s Balance: $%s | Contrib: $%s | Gains: $%s"
        (show a.accountType)
        (formatMoney a._balance)
        (formatMoney a._contributions)
        (formatMoney a._gains)

printSummary :: ModelConfig -> IO ()
printSummary config = do
    putStrLn "Monthly Summary:"
    putStrLn $ "  Gross Monthly: $" ++ formatMoney (config.initialSalary / 12)
    putStrLn $ "  Total Expenses: $" ++ formatMoney (monthlyExpenses config)
    putStrLn $ "  Target Emergency Fund Size: $" ++ formatMoney (calculateEmergencyFund config)
    putStrLn "\nExpense Breakdown:"
    mapM_ (\(name, amt) -> putStrLn $ "  " ++ name ++ ": $" ++ formatMoney amt) config.expenses

printMonthState :: MonthState -> IO ()
printMonthState state = do
    putStrLn $ "Month: " ++ show state._month
    putStrLn $ "Income: $" ++ formatMoney state._income

printSimulation :: [MonthState] -> IO ()
printSimulation states = do
    putStrLn "\nSimulation Results:"
    -- 1. Print the header using printf for alignment
    printf "%-5s %13s %10s %10s\n"
        ("Month" :: String)
        ("Income" :: String)
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
            printf "%-5s %s %s %s"
                (show s._month)  -- 5 wide (left aligned)
                (formatMoney s._income)
                (formatMoney s._taxes)
                (formatMoney s._salary)

        -- 2. Print the detailed retirement accounts on new, indented lines
        putStrLn $ formatAccount s._trad401k
        putStrLn $ formatAccount s._roth401k
        putStrLn $ formatAccount s._brokerage
        putStrLn $ formatAccount s._cash
        putStrLn $ formatAccount s._emergencyFund
        putStrLn "" -- Add a blank line for visual separation between months
