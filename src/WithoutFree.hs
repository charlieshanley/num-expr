{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}

-- TODO:
--   - add metadata
--   - explore some example use-cases
--   - explore using Functors other than Identity
--   - maybe improve ergonomics

module WithoutFree where

-- import           Data.Functor.Identity (Identity)
-- import           Data.Text     (Text)
-- import           Data.Void     (Void)
import qualified Numeric.AD    as AD
import           Prettyprinter

data Expr info a = Expr (Maybe info) (Expr' info a)
    deriving (Show, Functor, Foldable, Traversable)

data Expr' info a
    = Lit a
    | Add (Expr info a) (Expr info a)
    | Subtr (Expr info a) (Expr info a)
    | Mult (Expr info a) (Expr info a)
    | Negate (Expr info a)
    | Abs (Expr info a)
    | Signum (Expr info a)
    | FromInteger Integer
    deriving (Show, Functor, Foldable, Traversable)

lit :: a -> Expr info a
lit a =  Expr Nothing (Lit a)


infixl 5 ?:
(?:) :: Expr info a -> info -> Expr info a
Expr _ a ?: info = Expr (Just info) a

instance Num (Expr info a) where
    a + b = Expr Nothing (Add a b)
    a - b = Expr Nothing (Subtr a b)
    a * b = Expr Nothing (Mult a b)
    negate a = Expr Nothing (Negate a)
    abs a = Expr Nothing (Abs a)
    signum a = Expr Nothing (Signum a)
    fromInteger i = Expr Nothing (FromInteger i)

instance (Pretty a, Pretty info) => Pretty (Expr info a) where
    pretty = \case
        Expr info (Lit a)     -> pretty a <> maybe mempty ((pretty " ?" <+>) . pretty) info
        Expr info (Add a b)   -> bin "+" a b info
        Expr info (Subtr a b) -> bin "-" a b info
        Expr info (Mult a b)  -> bin "*" a b info
        Expr info (Negate a)  -> un "negate" a info
        Expr info (Abs a)     -> un "abs" a info
        Expr info (Signum a)  -> un "signum" a info
        Expr info (FromInteger i)  -> pretty i
        where
            bin op a b info = align $ vsep [lparen <+> pretty a, pretty op <+> pretty b, rparen <> maybe mempty ((pretty " ?" <+>) . pretty) info]
            un op a _info = pretty op <+> pretty a

eval :: Num a => Expr info a -> a
eval = \case
    Expr _ (Lit a)         -> a
    Expr _ (Add a b)       -> eval a + eval b
    Expr _ (Subtr a b)     -> eval a - eval b
    Expr _ (Mult a b)      -> eval a * eval b
    Expr _ (Negate a)      -> negate $ eval a
    Expr _ (Abs a)         -> abs $ eval a
    Expr _ (Signum a)      -> signum $ eval a
    Expr _ (FromInteger i) -> fromInteger i


tax :: Expr String Double
tax = incomeTax + capitalGainsTax
  where
    incomeTax = income * incomeTaxRate
    incomeTaxRate = lit 0.25 ?: "Income tax rate"
    income = wages + taxableInterest ?: "Income"
    wages = lit 100 ?: "Wages"
    taxableInterest = interest - nontaxableInterest
    interest = lit 50 ?: "Interest"
    nontaxableInterest = lit 20 ?: "Nontaxable interest"
    capitalGainsTax = netCapitalGains * capitalGainsTaxRate
    capitalGainsTaxRate = lit 0.15 ?: "Capital gains tax rate"
    netCapitalGains = max 0 <$> capitalGains - capitalLosses
    capitalLosses = lit 200 ?: "Capital losses"
    capitalGains = lit 400 ?: "Capital gains"

someFunc :: IO ()
someFunc = do
    print tax
    print $ AD.grad eval tax
