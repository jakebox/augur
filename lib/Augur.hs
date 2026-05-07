module Augur (augurMain) where

import Augur.Config
import Augur.Pretty
import Augur.Simulation
import Augur.Export (writeToJson)

augurMain :: IO ()
augurMain = do
    let state = initState defaultConfig
        months = simulate (12 * 3) defaultConfig state

    printSummary defaultConfig
    -- printSimulation months
    writeToJson months
