module MercDB where

-- First we define the types we will be working with in a somewhat simplified way (only pkeys).
type ClienteTel = String
type Cliente = Int
type Venda = Int
type Produto = Int
data VendaProduto = VP {
    vp_venda :: Venda,
    vp_produto :: Produto,
    quant :: Int,
    price :: Float,
    value :: Float
} deriving (Show, Eq, Ord)
data Data = D {
    day :: Int,
    month :: Int,
    year :: Int
} deriving (Show, Eq)
type Cupon = Int

-- Ord for dates
instance Ord Data where
    (D d1 m1 y1) <= (D d2 m2 y2)
        | y2 > y1 = True
        | y2 < y1 = False
        | y2 == y1 && m2 > m1 = True
        | y2 == y1 && m2 < m1 = False
        | y2 == y1 && m2 == m1 && d2 >= d1 = True
        | y2 == y1 && m2 == m1 && d2 < d1 = False

-------------------------------------------- Clientes --------------------------------------------
c1tel :: ClienteTel; c1tel = "000000000"
c1 :: Cliente; c1 = 0

c2tel :: ClienteTel; c2tel = "000000001"
c2 :: Cliente; c2 = 1

c3tel :: ClienteTel; c3tel = "000000002"
c3 :: Cliente; c3 = 2

-------------------------------------------- Cupões ----------------------------------------------
cup1 :: Cupon; cup1 = 0
cup2 :: Cupon; cup2 = 1
cup3 :: Cupon; cup3 = 2

-------------------------------------------- Vendas ----------------------------------------------
v1 :: Venda; v1 = 0
v2 :: Venda; v2 = 1
v3 :: Venda; v3 = 2

-------------------------------------------- Produtos ---------------------------------------------
p1 :: Produto; p1 = 0
p2 :: Produto; p2 = 1
p3 :: Produto; p3 = 2

-------------------------------------------- VendaProdutos ----------------------------------------
vp1 :: VendaProduto; vp1 = VP v1 p1 100 10 1000
vp2 :: VendaProduto; vp2 = VP v2 p2 100 10 1000
vp3 :: VendaProduto; vp3 = VP v3 p3 100 10 1000

-------------------------------------------- Datas -------------------------------------------------
d1 :: Data; d1 = D 1 1 2020
d2 :: Data; d2 = D 2 2 2020
d3 :: Data; d3 = D 3 3 2020
