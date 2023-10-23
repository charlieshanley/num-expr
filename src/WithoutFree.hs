{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}

module WithoutFree where

import           Data.Functor.Identity (Identity)
import qualified Numeric.AD            as AD

data Expr' a
    = Pure a
    | Add (Expr' a) (Expr' a)
    | Subtr (Expr' a) (Expr' a)
    | Mult (Expr' a) (Expr' a)
    | Negate (Expr' a)
    | Abs (Expr' a)
    | Signum (Expr' a)
    | FromInteger Integer
    deriving (Show, Functor, Foldable, Traversable)

type Expr f a = Expr' (f a)

instance Num (Expr' a) where
    (+) = Add
    (-) = Subtr
    (*) = Mult
    negate = Negate
    abs = Abs
    signum = Signum
    fromInteger = FromInteger

eval :: Num a => Expr' a -> a
eval = \case
    Pure a        -> a
    Add a b       -> eval a + eval b
    Subtr a b     -> eval a - eval b
    Mult a b      -> eval a * eval b
    Negate a      -> negate $ eval a
    Abs a         -> abs $ eval a
    Signum a      -> signum $ eval a
    FromInteger i -> fromInteger i


tax :: Expr Identity Double
tax = incomeTax + capitalGainsTax
  where
    incomeTax = income * incomeTaxRate
    incomeTaxRate = Pure 0.25
    income = wages + taxableInterest
    wages = Pure 100
    taxableInterest = interest - nontaxableInterest
    interest = Pure 50
    nontaxableInterest = Pure 20
    capitalGainsTax = netCapitalGains * capitalGainsTaxRate
    capitalGainsTaxRate = Pure 0.15
    netCapitalGains = max 0 <$> capitalGains - capitalLosses
    capitalLosses = Pure 200
    capitalGains = Pure 400

someFunc :: IO ()
someFunc = do
    print tax
    print $ AD.grad eval tax
