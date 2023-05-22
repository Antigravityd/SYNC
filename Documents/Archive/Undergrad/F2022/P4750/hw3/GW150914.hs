-- GHC Haskell

{-# LANGUAGE BangPatterns #-}

import Prelude hiding (zip, take, (++), replicate, length, drop, map, foldl', sum)
import qualified Prelude as P (drop, take, (++))
import Data.Maybe (fromJust)
import Data.Complex ( Complex(..), magnitude, realPart)
import Data.Vector
import Statistics.Transform (fft)
import qualified Data.ByteString as BS
import Data.ByteString.Lex.Fractional (readSigned, readExponential)



rpad :: Vector e -> e -> Int -> Vector e
rpad xs with to = take to (xs ++ replicate to with)

chunksOf :: Int -> Vector e -> e -> Vector (Vector e)
chunksOf size xs padWith = let elts = length xs
                               fullChunks = elts `div` size
                               partChunks = ceiling (fromIntegral elts / fromIntegral size
                                                     - fromIntegral fullChunks)
                               in generate (fullChunks + partChunks)
                                  (\i -> if i /= fullChunks + partChunks
                                    then slice (i * size) (size) xs
                                    else if partChunks == 1
                                         then rpad (slice (fullChunks * size)
                                                    (elts - fullChunks * size) xs)
                                              padWith size
                                         else slice (i * size) (size) xs)


-- Fast/Discrete Fourier Transform: $\tilde{x}_T(f_k) = \sum_{j = -N / 2}^{N / 2}x_j e^{-2\pi i kj/N}\Delta t$
-- where $x_j(t)$ is the input time series with uniform time spacing $\Delta t$,
-- $T = N\Delta t$ is the total measurement time, $N$ is the number of points in the time series,
-- and $f_k = \frac{k}{T}$, where $0 \leq k \leq N - 1$.
-- This algorithm is $O(n^2)$, and accordingly, is impossible to run on the input data.
badFft :: Vector (Complex Double) -> Double -> Vector (Complex Double)
badFft ts delta_t = let n = length ts
                        bound = n `div` 2
                    in generate n (\k -> sum
                                    (generate n
                                     (\j  -> (ts ! j) * (delta_t :+ 0)
                                             * exp (0 :+ (-1) * 2 * pi
                                                    * fromIntegral k * fromIntegral (j - bound)
                                                    / fromIntegral n))))




-- Estimate power spectral density of time series, the "energy" carried by given frequencies:
-- $S_n(f_k) = \frac{1}{MN\Delta t}\sum_{i=0}^M|\tilde{x}_i(f_k)|^2$
-- where $M$ is the number of samples desired, and $\tilde{x}_i(f_k)$ is interpreted as
-- the FFT of the $i$th sub-series of length $T / M$ at value $f_k$.
-- Higher $M$ yields a better estimate, at the cost of frequency resolution.
psdEstimate :: Vector (Complex Double) -> Double -> Int -> Vector Double
psdEstimate ts delta_t samples = let coeff = 1 / (fromIntegral samples * fromIntegral n * delta_t)
                                     n = length ts
                                     sublength =  n `div` samples
                                     ffts = map (\chunk -> map ((*) $ delta_t :+ 0) (fft chunk))
                                       $ chunksOf sublength ts 0
                                   in generate sublength
                                      (\k -> coeff
                                        * sum (generate samples
                                               (\i -> (^2) . magnitude $ (ffts ! i) ! k)))



-- monad to read gravitational wave time series from file; they use '^#' as a comment
readData :: Int -> BS.ByteString -> (Vector (Complex Double))
readData n file = map (\x -> (fst $ fromJust (readSigned readExponential x)) :+ 0 )
                  $ fromList (P.take n (BS.split 10 file))



main :: IO ()
main = do h1str <- BS.readFile "H-H1_GWOSC_4KHZ_R1-1126257415-4096.txt"
--          l1str <- BS.readFile "L-L1_GWOSC_4KHZ_R1-1126257415-4096.txt"
          let h1 = readData 16777216 h1str
          let h1s = psdEstimate h1 (1 / 4e3) 32
          print h1s
          -- you can use "tail" to view the progress
          -- pipe stdout into tr , '\n' and redirect to file
