module Augur (augurMain) where

import Augur.Config
import Augur.Pretty
import Augur.Simulation
import Augur.Types

augurMain :: IO ()
augurMain = do
    let state = initState defaultConfig
        months = simulate (40 * 12) defaultConfig state

    printSummary defaultConfig
    printSimulation [last months]
