module WithFree (someFunc) where

import           Data.Functor.Free (Free)
import qualified Data.Functor.Free as Free
-- import           Data.Functor.Identity (Identity (..))
import qualified Numeric.AD        as AD

type Expr = Free Num

tax :: Expr Double
tax = incomeTax + capitalGainsTax
  where
    incomeTax = income * incomeTaxRate
    incomeTaxRate = pure 0.25
    income = wages + taxableInterest
    wages = pure 100
    taxableInterest = interest - nontaxableInterest
    interest = pure 50
    nontaxableInterest = pure 20
    capitalGainsTax = netCapitalGains * capitalGainsTaxRate
    capitalGainsTaxRate = pure 0.15
    netCapitalGains = max 0 <$> capitalGains - capitalLosses
    capitalLosses = pure 200
    capitalGains = pure 400

someFunc :: IO ()
someFunc = do
    print tax
    print $ AD.grad Free.counit tax
