{-# LANGUAGE ForeignFunctionInterface #-}
 
module Fib where
 
import Foreign.C.Types

fibx :: Int -> Int
fibx 0 = 1
fibx 1 = 1
fibx n = fibx (n-1) + fibx (n-2)

fib_hs :: CInt -> CInt
fib_hs = fromIntegral . fibx . fromIntegral

fibonacci :: Int -> Int
fibonacci n = fibs !! n
    where fibs = 1 : 1 : zipWith (+) fibs (tail fibs)
 
fibonacci_hs :: CInt -> CInt
fibonacci_hs = fromIntegral . fibonacci . fromIntegral


fibe :: CInt -> CInt
fibe = snd . fib2

-- fib2 :: Int -> (Int , Int)
fib2 0 = (1,1)
fib2 1 = (1,1)
fib2 n = let (nM2,nM1) =  fib2(n-1)
         in  (nM1 , nM2 + nM1)
 
foreign export ccall fibonacci_hs :: CInt -> CInt
foreign export ccall fib_hs :: CInt -> CInt
foreign export ccall fibe :: CInt -> CInt
