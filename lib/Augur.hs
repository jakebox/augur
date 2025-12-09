module Augur (augurMain) where

import Augur.Types
import Augur.Config
import Augur.Simulation
import Augur.Pretty
import Data.Time.Calendar.Month

augurMain :: IO ()
augurMain = do
  let startMonth = MkMonth $ (2026 * 12) + (5 - 1)
      initialState = initState startMonth
      months = simulate 6 defaultConfig initialState

  printSummary defaultConfig
  printSimulation months

