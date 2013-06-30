module Main where

import Control.Applicative

mean' :: [Double] -> Int -> Double-> Double
mean' [] c sum = sum / (fromIntegral c)
mean' (hd : tl)  c sum = mean' tl (c+1) (hd + sum)

mean :: [Double] -> Double
mean l = mean' l 0 0.0


divergence' avg [] c sum = sum / (fromIntegral c)
divergence' avg (hd : tl) c sum = let
  dif = (avg - hd)
  in
   divergence' avg tl (c + 1) (sum + dif * dif)

-- Expectation of difference from the average
divergence :: [Double] -> Double
divergence l = let avg = mean l
               in
                divergence' avg l 0 0.0

getNumbers :: IO [Double]
getNumbers = do
  c <- getContents
  let ls = lines c
      nums = read <$> ls :: [Double]
  return nums

test :: IO ()
test = do
  putStrLn $ show (divergence [2,3,4])


main = do
  nums <- getNumbers
  putStrLn $ "avg: " ++ (show (mean nums))
  putStrLn $ "div: " ++ (show (divergence nums))
