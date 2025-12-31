module Augur.Simulation where

import Augur.Calculations
import Augur.Types

import Control.Monad.Reader
import Control.Monad.State
import Data.Time.Calendar.Month
import Lens.Micro
import Lens.Micro.Mtl

type Sim a = ReaderT ModelConfig (State MonthState) a

initState :: ModelConfig -> MonthState
initState cfg =
    MonthState
        { _month = addMonths (-1) cfg.start
        , _income = 0
        , _trad401k = emptyAccount Traditional
        , _roth401k = emptyAccount Roth
        , _brokerage = emptyAccount Taxable
        , _cash = emptyAccount Cash
        , _emergencyFund = emptyAccount Emergency
        , _taxes = 0
        , _salary = cfg.initialSalary
        }

updatePreTax :: Integer -> Sim Money
updatePreTax yrs = do
    cfg <- ask

    let grossIncome = calculateSalaryMonth cfg yrs

    contribution <- zoom trad401k $ do
      account <- get
      let gain = calculateReturnMonth cfg account
          contrib = calculateContribution cfg grossIncome yrs account

      balance += (gain + contrib)
      contributions += contrib
      gains += gain

      return contrib

    let deductions = [contribution]
        taxes' = calculateTaxes cfg grossIncome deductions yrs

    taxes .= taxes'
    income .= (grossIncome - taxes')
    salary .= (grossIncome * 12)

    return (grossIncome - taxes')

updateMonth :: Sim ()
updateMonth = do
    month %= addMonths 1
    cfg <- ask
    yrs <- getYearsElapsed

    net <- updatePreTax yrs

    let postExpSurplus = net - calculateExpenses cfg yrs

    _ <- fillAccount postExpSurplus roth401k
        >>= (`fillAccount` emergencyFund)
        >>= (`fillAccount` brokerage)
        >>= (`fillAccount` cash)

    pure ()

fillAccount :: Money -> Lens' MonthState Account -> Sim Money
fillAccount pool target = do
    cfg <- ask
    yrs <- getYearsElapsed

    zoom target $ do
        account <- get
        let gain = calculateReturnMonth cfg account
            contrib = calculateContribution cfg pool yrs account

        balance += (gain + contrib)
        contributions += contrib
        gains += gain

        return (pool - contrib)

getYearsElapsed :: Sim Integer
getYearsElapsed = do
    startMonth <- asks start -- Reader
    current <- use month -- State
    pure $ diffMonths current startMonth `div` 12

stepMonth :: ModelConfig -> MonthState -> MonthState
stepMonth config = execState (runReaderT updateMonth config)

simulate :: Int -> ModelConfig -> MonthState -> [MonthState]
simulate n config initial = take n $ drop 1 $ iterate (stepMonth config) initial
