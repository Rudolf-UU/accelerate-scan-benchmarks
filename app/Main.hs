{-# language TypeApplications #-}
module Main (main) where

import Lib
import Data.Array.Accelerate              as A
import Data.Array.Accelerate.Interpreter
import Data.Array.Accelerate.LLVM.Native  as CPU
import Prelude (Show(..), IO, )
import qualified Prelude as Prelude
import System.IO
-- import Data.Array.Accelerate.LLVM.PTX     as GPU

main :: IO ()
main = print . run @Native $ dotp (use xs) (use ys)
    where 
        xs = fromList (Z:.10) [0..] :: Vector Float
        ys = fromList (Z:.10) [1,3..] :: Vector Float

dotp :: Acc (Vector Float) -> Acc (Vector Float) -> Acc (Scalar Float)
dotp xs ys = fold (+) 1 (zipWith (*) xs ys)

dotp2 :: Acc (Vector Int) -> Acc (Vector Int) -> Acc (Scalar Int)
dotp2 a b = fold (+) 0 $ zipWith (*) (map (+1) a) (map (`div` 2) b)
