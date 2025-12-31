module Augur.Simulation (
    initState,
    updateMonth,
    simulate,
    monthlyExpenses,
) where

import Augur.Calculations
import Augur.Types

import Control.Monad.Reader
import Control.Monad.State
import Data.Decimal
import Data.List (mapAccumL)
import Data.Map qualified as M
import Data.Time.Calendar.Month
import Debug.Trace
import Lens.Micro
import Lens.Micro.Mtl

type Sim a = ReaderT ModelConfig (State MonthState) a

initState :: ModelConfig -> MonthState
initState cfg =
    MonthState
        { _month = addMonths (-1) cfg.start
        , _income = 0
        , _totalExpenses = 0
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

    zoom trad401k $ do
        gain <- gets (calculateReturnMonth cfg)
        balance += gain

    let deductions = []
        taxesDone = calculateTaxes cfg grossIncome deductions yrs

    taxes .= taxesDone
    income .= (grossIncome - taxesDone)
    salary .= (grossIncome * 12)

    return (grossIncome - taxesDone)

getYearsElapsed :: Sim Integer
getYearsElapsed = do
    startMonth <- asks start -- Reader
    current <- use month -- State
    pure $ diffMonths current startMonth `div` 12


updateMonth :: Sim ()
updateMonth = do
    month %= addMonths 1
    cfg <- ask
    yrs <- getYearsElapsed

    -- 1. Income and pre-tax
    remainder <- updatePreTax yrs

    let monthlyExps = calculateExpenses cfg yrs
    totalExpenses .= monthlyExps
    let postExpSurplus = remainder - monthlyExps

    leftover <-
        fillAccount postExpSurplus roth401k
            >>= (`fillAccount` emergencyFund)
            >>= (`fillAccount` brokerage)

    zoom cash $ balance += leftover

fillAccount :: Money -> Lens' MonthState Account -> Sim Money
fillAccount pool target = do
    cfg <- ask
    yrs <- getYearsElapsed

    zoom target $ do
        acc <- get
        let gain = calculateReturnMonth cfg acc
            contrib = calculateContribution cfg pool yrs acc

        balance += (gain + contrib)
        contributions += contrib
        gains += gain

        return (pool - contrib)

stepMonth :: ModelConfig -> MonthState -> MonthState
stepMonth config = execState (runReaderT updateMonth config)

simulate :: Int -> ModelConfig -> MonthState -> [MonthState]
simulate n config initial = take n $ drop 1 $ iterate (stepMonth config) initial
