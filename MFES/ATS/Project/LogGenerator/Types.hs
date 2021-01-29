{-# LANGUAGE TemplateHaskell #-}

module Types where

import Numeric

------------------------------------------------------------------------- UTILITIES ----------------------------------------------------------------------

ezShowComma :: [String] -> String
ezShowComma = ezShow ","

ezShow :: String -> [String] -> String
ezShow sep = init . concatMap (++ sep)

------------------------------------------------------------------------- UTILITIES ----------------------------------------------------------------------

type Id a = (ID, a)
type ID = Int
type Name = String
type Weight = Float
type NIF = Int
type Quantity = Float
type Range = (Int,Int)
type PriceF = Float

data Price = Pr { pr :: Float }

instance Show Price where
    show (Pr p) = showFFloat (Just 1) p ""

data Radius = R Int deriving Eq

instance Show Radius where
    show (R x) = show $ (toEnum x :: Float)

data Loc = L Float Float deriving Eq

instance Show Loc where
    show (L x y) = ezShowComma [show x, show y]

data User = U Name Loc

instance Eq User where
    (U _ l1) == (U _ l2) = l1 == l2

instance Show User where
    show (U n l) = "Utilizador:" ++ ezShowComma [n, show l]

data Volunteer = V Name Loc Radius

instance Eq Volunteer where
    (V _ l1 _) == (V _ l2 _) = l1 == l2

instance Show Volunteer where
    show (V n l r) = "Voluntario:" ++ ezShowComma [n, show l, show r]

data Transporter = T Name Loc NIF Radius Price

instance Eq Transporter where
    (T n1 l1 nf1 _ _) == (T n2 l2 nf2 _ _) = l1 == l2 || n1 == n2 || nf1 == nf2

instance Show Transporter where
    show (T n l nf r p) = "Transportadora:" ++ ezShowComma [n, show l, show nf, show r, show p]

data Store = S Name Loc deriving Eq

instance Show Store where
    show (S n l) = "Loja:" ++ ezShowComma [n, show l]

-- Unitary price
data Product = P Name deriving Eq

instance Show Product where
    show (P n) = n

data ProductInOrder = PIO {_prod :: Id Product, price :: PriceF, quant :: Quantity}

instance Eq ProductInOrder where
    _ == _ = False

instance Show ProductInOrder where
    show (PIO (id, pn) pr q) = ezShowComma ["p" ++ show id, show pn, showFFloat (Just 6) q "", showFFloat (Just 1) pr ""] 

-- userID, StoreID, total price, product list
data Order = O ID ID PriceF [ProductInOrder]

instance Eq Order where
    _ == _ = False  

instance Show Order where
    show (O uid sid p l) = "Encomenda:" ++ ezShowComma ["u" ++ show uid, "l" ++ show sid, show p, mid $ show l] where
        mid = init . tail