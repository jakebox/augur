module Augur (augurMain) where

import Augur.Types
import Augur.Config
import Augur.Simulation
import Augur.Pretty
import Data.Time.Calendar.Month

augurMain :: IO ()
augurMain = do
  let salary = 140_000
      nMonths = 12
      startMonth = MkMonth $ (2026 * 12) + (5 - 1)
      config = defaultConfig salary
      initialState = initState startMonth
      months = simulate nMonths config initialState

  putStrLn $ replicate 60 '='
  printSummary config
  putStrLn $ replicate 60 '='
  printSimulation months
  putStrLn "\nFinal Balance after 12 months:"
  putStrLn $ "$" <> formatMoney (cashBalance $ last months)

