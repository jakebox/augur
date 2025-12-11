{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Augur.Export (writeToJson) where

import Augur.Pretty (formatMoney)
import Augur.Types
import Data.Aeson
import Data.ByteString.Char8 qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Decimal (Decimal)
import Data.Time.Calendar.Month (Month)
import Text.Printf (printf)

instance ToJSON MonthState
instance ToJSON Account
instance ToJSON AccountType

instance ToJSON Decimal where
  toJSON number = toJSON decFloat
    where
      decFloat :: Float = read decStr
      decStr :: String = printf "%.2f" (realToFrac number :: Double)

writeToJson :: [MonthState] -> IO ()
writeToJson ms = BL.writeFile "out.json" $ encode ms
