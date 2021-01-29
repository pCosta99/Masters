module Ex2 where

import Test.QuickCheck
import Control.Arrow
import Cp

f :: [Int] -> [Int]
f [] = []
f (h:t) | mod h 2 /= 0 = (h+1) : f t
        | otherwise = (h-1) : f t

g :: [Int] -> [Int]
g [] = []
g (h:t) | mod h 2 /= 0 = (h+1) : g t
        | otherwise = (h-1) : g t

numPares, numImpares :: [Int] -> Int
numPares = length . filter even
numImpares = length . filter odd

equals :: (Eq a) => (b -> (a,a)) -> b -> Bool
equals = (uncurry (==) .)

prop1 = equals $ numImpares &&& numPares . f
prop2 = equals $ numPares &&& numImpares . f
prop3 = equals $ both &&& both . f where
    both = add . (numPares &&& numImpares)
    add = uncurry (+)

ateAMutacaoVaiComOCrl [] = null $ f []
ateAMutacaoVaiComOCrl l@(x:xs) = if even x then head (f l) > x else head (f l) < x

ateAMutacaoVaiComOCrlPf = cond null (null . f) $ cond evenHead (op (>)) (op (<)) where
    head_f = head . f
    evenHead = even . head
    op cmp = uncurry cmp . (head_f &&& head)