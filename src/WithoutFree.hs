{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}

-- TODO:
--   - add a pretty-printer
--   - add metadata
--   - explore some example use-cases
--   - explore using Functors other than Identity
--   - maybe improve ergonomics

module WithoutFree where

-- import           Data.Functor.Identity (Identity)
import qualified Numeric.AD    as AD
import           Prettyprinter

data Expr a
    = Pure a
    | Add (Expr a) (Expr a)
    | Subtr (Expr a) (Expr a)
    | Mult (Expr a) (Expr a)
    | Negate (Expr a)
    | Abs (Expr a)
    | Signum (Expr a)
    | FromInteger Integer
    deriving (Show, Functor, Foldable, Traversable)

instance Num (Expr a) where
    (+) = Add
    (-) = Subtr
    (*) = Mult
    negate = Negate
    abs = Abs
    signum = Signum
    fromInteger = FromInteger

instance Pretty a => Pretty (Expr a) where
    pretty = \case
        Pure a        -> pretty a
        Add a b       -> align $ vsep [lparen <+> pretty a, pretty "+" <+> pretty b, rparen]
        Subtr a b     -> align $ vsep [lparen <+> pretty a, pretty "-" <+> pretty b, rparen]
        Mult a b      -> align $ vsep [lparen <+> pretty a, pretty "*" <+> pretty b, rparen]
        Negate a      -> pretty "negate" <+> pretty a
        Abs a         -> pretty "abs" <+> pretty a
        Signum a      -> pretty "signum" <+> pretty a
        FromInteger i -> pretty i

eval :: Num a => Expr a -> a
eval = \case
    Pure a        -> a
    Add a b       -> eval a + eval b
    Subtr a b     -> eval a - eval b
    Mult a b      -> eval a * eval b
    Negate a      -> negate $ eval a
    Abs a         -> abs $ eval a
    Signum a      -> signum $ eval a
    FromInteger i -> fromInteger i


tax :: Expr Double
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
