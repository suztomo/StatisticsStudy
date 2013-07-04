module Main where

import Control.Applicative
import Statistics.Function.Basics (getNumbers, mean, variance)

-- Calculates average, variance and standard deviation
-- Example:
-- 1. get random numbers
--    perl -e 'for($i=0;$i<10000;++$i) {print(100 * rand . "\n")}' > /tmp/a
-- 2. get average and standard deviation
-- 
--    $ cat /tmp/a |divr 
--    avg: 49.98443108955174
--    variance: 836.3678591468712
--    standard deviation: 28.920025227286217
-- 3. Confirm standardization. The variance becomes close to 1 
--    $ cat /tmp/a |perl -nle 'print (($_ - 49.9844311) / 28.9200)' |divr

main = do
  nums <- getNumbers
  let m = mean nums
      var = variance nums
      stdDev = sqrt var
  sequence_ $ (\x -> putStrLn $ show $ (x - m) / stdDev) <$> nums

