import Control.Applicative hiding ((<|>))
import System.IO.Unsafe
import Data.IORef

varCounter :: IORef Int
varCounter = unsafePerformIO $ newIORef 0

getFreshVar :: () -> String
getFreshVar () = 
    "x" ++ 
    (show . unsafePerformIO $ modifyIORef varCounter (+ 1) >> readIORef varCounter)


data Term = Pr Term Term
          | Abs String Term
          | App Term Term
          | Var String
          | Unit

instance Show Term where
    show t = case t of
        Unit -> "()"
        Var x -> x
        Abs v f -> "\\" ++ v ++ " -> " ++ show f
        App f x -> "(" ++ show f ++ ")" ++ show x
        Pr a b -> "<" ++ show a ++ ", " ++ show b ++ ">"


data Type = UnitTy
          | PrTy Type Type
          | AbsTy Type Type

instance Show Type where
    show t = case t of
        UnitTy -> "A"
        PrTy u v -> "(" ++ show u ++ " x " ++ show v ++ ")"
        AbsTy u v -> show u ++ " -> " ++ show v

interleave :: [[a]] -> [a]
interleave []  = []
interleave xss = (map head xss') ++ interleave (map tail xss')
    where xss' = filter (not . null) xss

diagonalize :: [[a]] -> [a]
diagonalize xss = go [] xss
    where go active (y:yys) = map head active ++ go (y : map tail active) yys
          --go active [] = interleave active


allTypes :: [Type]
allTypes = UnitTy : interleave [
                       [PrTy u v | u <- allTypes, v <- allTypes]
                     , [AbsTy u v | u <- allTypes, v <- allTypes]
                     ]

termsOfType :: Type -> [Term]
termsOfType UnitTy = [Unit] ++ appTermsOfType UnitTy
termsOfType ty = interleave [appTermsOfType ty, absTermsOfType ty, prTermsOfType ty]

absTermsOfType :: Type -> [Term]
absTermsOfType (AbsTy u v) = [Abs (getFreshVar ()) t | t <- termsOfType v]
absTermsOfType _ = []

appTermsOfType :: Type -> [Term]
appTermsOfType t = diagonalize [ 
        diagonalize [
            [App f x | f <- termsOfType (AbsTy a t)]
            | x <- termsOfType a
        ]
        | a <- allTypes
    ]

prTermsOfType :: Type -> [Term]
prTermsOfType (PrTy u v) = diagonalize [[Pr a b | a <- termsOfType u] | b <- termsOfType v]
prTermsOfType _ = []