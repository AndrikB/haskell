module Main where
import Data.Time
import Lib

f :: Double -> Double
f x = 3 * x ^ 2

main = do
     start <- getCurrentTime
     print (calculate 1 5 0.001 10 f)
     end <- getCurrentTime
     print (diffUTCTime end start)