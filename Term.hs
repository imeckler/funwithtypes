{-# LANGUAGE IncoherentInstances, FlexibleInstances, FlexibleContexts, UndecidableInstances, GADTs, RecordWildCards, NoMonomorphismRestriction #-}
import Text.Parsec
import Control.Applicative hiding ((<|>))
import System.IO.Unsafe
import Data.IORef

varCounter :: IORef Int
varCounter = unsafePerformIO $ newIORef 0

getFreshVar :: () -> String
getFreshVar () = 
    "x" ++ 
    (show . unsafePerformIO $ modifyIORef varCounter (+ 1) >> readIORef varCounter)


data Term a where
    Atom :: a -> Term a
    Var :: String -> Term a
    Abs :: String -> Term b -> Term (a -> b)
    App :: Term (a -> b) -> Term a -> Term b
    Pr  :: Term a -> Term b -> Term (a, b)
    Pi1 :: Term ((a, b) -> a)
    Pi2 :: Term ((a, b) -> b)

instance Show (Term a) where
    show t = case t of
        Atom a -> "ATOM"
        Var x -> x
        Abs v f -> "\\" ++ v ++ " -> " ++ show f
        App f x -> "(" ++ show f ++ ")" ++ show x
        Pr a b -> "<" ++ show a ++ ", " ++ show b ++ ">"
        Pi1 -> "p1"
        Pi2 -> "p2"

interleave :: [[a]] -> [a]
interleave []  = []
interleave xss = (map head xss') ++ interleave (map tail xss')
    where xss' = filter (not . null) xss

class HasTerms a where
    canonicalTerm :: Term a
    canonicalTerm = Var "x"

    varTL :: [Term a]
    varTL = [Var ("y" ++ show i) | i <- [1..]]

    appTL :: [Term a]
    appTL = []  
    --appTL = [t | t@(App _ _) <- terms]

    absTL :: [Term a]
    absTL = []
    --absTL = [t | t@(Abs _ _) <- terms]

    prTL  :: [Term a]
    prTL = []
    --prTL = [t | t@(Pr _ _) <- terms]

    atomTL :: [Term a]
    atomTL = []
    --atomTL = [t | t@(Atom _) <- terms]

    terms :: [Term a]
    terms = interleave [varTL, appTL, absTL, prTL, atomTL]

--instance HasTerms ((a, b) -> a) where
--    canonicalTerm = Abs "x" (Pi1 $ Var "x")

instance (HasTerms b) => HasTerms (a -> b) where
    absTL = [Abs (getFreshVar ()) t | t <- terms]
    canonicalTerm = Abs (getFreshVar ()) canonicalTerm

instance (HasTerms a, HasTerms b) => HasTerms (a, b) where
    --appTL = [App f x | f <- terms, x <- terms]
    absTL = []
    atomTL = []
    prTL  = [Pr a b | a <- terms, b <- terms]
    canonicalTerm = Pr canonicalTerm canonicalTerm

instance HasTerms a where

--instance (HasTerms (a -> b), HasTerms a) => HasTerms b where
--    appTL = [App f x | f <- terms, x <- terms]


instance HasTerms () where
    atomTL = [Atom ()]
    canonicalTerm = Atom ()

data Proof a ass where
    Assume :: a -> Proof a a
    ModPon :: Proof (a -> b) x -> Proof a y -> Proof b (x, y)
    Conj :: Proof a x -> Proof b y -> Proof (a, b) (x, y)


