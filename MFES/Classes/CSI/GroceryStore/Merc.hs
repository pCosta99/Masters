--Translation of the grocery store exercise into haskell

module Merc where

import RelCalc
import MercDB
import Data.Monoid
--import Data.Dates

-- Define the relations present in the problem
cliente_tel :: [(ClienteTel, Cliente)]
cliente_tel = [(c1tel,c1),(c2tel,c2),(c3tel,c3)]

cliente :: [(Venda, Cliente)]
cliente = [(v1,c1),(v2,c1),(v3,c2)]

cliente_cup :: [(Cupon, Cliente)]
cliente_cup = [(cup1, c1), (cup2, c1), (cup3, c2)]

usou :: [(Cupon, Venda)]
usou = [(c1,v1),(c2,v2),(c3,v3)]

venda :: [(VendaProduto, Venda)]
venda = [(vp1, v1), (vp2, v2), (vp3, v3)]

produto :: [(VendaProduto, Produto)]
produto = [(vp1, p1), (vp2, p2), (vp3, v3)]

menor_igual :: [(Data, Data)]
menor_igual = [(d1,d2),(d2,d3),(d1,d3),(d1,d1),(d2,d2),(d3,d3)]

r_data :: [(Venda, Data)]
r_data = [(v1,d1),(v2,d2),(v3,d3)]

limite :: [(Cupon, Data)]
limite = [(c1,d1),(c2,d2),(c3,d3)]

-- Assertions
cupons_used = sse (comp usou cliente) cliente_cup

no_invalid_dates = sse (comp usou r_data) (comp limite menor_igual)

assert_all = getAll . foldMap All $ [cupons_used, no_invalid_dates]

-- Queries
tels_products_through_cupons = cliente_tel `comp` (conv cliente_cup) `comp` usou `comp` conv venda `comp` produto
