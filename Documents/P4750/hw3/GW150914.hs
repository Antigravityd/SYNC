-- GHC Haskell


import Safe (headMay, atMay)
import Data.Maybe (fromMaybe)
import Data.Complex ( Complex(..), magnitude)
import Data.Ix ( range )
import Data.List.Split (chunksOf)



data TimeSeries = TimeSeries {step :: Double,
                              points :: [Complex Double]}
-- ?               deriving Seq


-- Fast/Discrete Fourier Transform: $$\tilde{x}_T(f_k) = \sum_{j = -N / 2}^{N / 2}x_j e^{-2\pi i kj/N}\Delta t$$
-- where $x_j(t)$ is the input time series with uniform time spacing $\Delta t$, $T = N\Delta t$ is the total measurement time,
-- $N$ is the number of points in the time series, and $f_k = \frac{k}{T}$, where $0 \leq k \leq N - 1$.
fft :: TimeSeries -> TimeSeries
fft ts = let n = length (points ts)
             delta_t = step ts
             delta_f = 1 / delta_t
             bound = floor (fromIntegral n / 2)
         in TimeSeries delta_f
            $ map (\k -> sum $ map
                         (\j  -> (points ts !! (bound + j)) * (delta_t :+ 0)
                           * exp (0 :+ (-1.0) * 2.0 * pi * fromIntegral k * fromIntegral j / fromIntegral n))
                         $ range (-bound, bound))
            $ range (0, n - 1)


-- Estimate power spectral density of time series, the "energy" carried by given frequencies: $$S_n(f_k) = \frac{1}{MN\Delta t}\sum_{i=0}^M|\tilde{x}_i(f_k)|^2$$
-- where $M$ is the number of samples desired, and $\tilde{x}_i(f_k)$ is interpreted as the FFT of the $i$th sub-series of length $T / M$ at value $f_k$.
-- Higher $M$ yields a better estimate, at the cost of frequency resolution.
psdEstimate :: TimeSeries -> Int -> TimeSeries
psdEstimate ts samples = let n = length (points ts);
                             delta_t = step ts
                             delta_f = 1 / delta_t
                             coeff = 1 / (fromIntegral samples * fromIntegral n * delta_t)
                             subserieses = chunksOf (ceiling (fromIntegral n / fromIntegral samples)) $ points (fft ts)
                         in TimeSeries delta_f
                            $ map (\k -> coeff * (sum $ map
                                                   (\i -> (^2) . magnitude . fromMaybe 0 $ atMay (subserieses !! i) k)
                                                   $ range (0, samples - 1)) :+ 0)
                            $ range (0, length (points ts) - 1)



-- monad to read gravitational wave time series from file; they use '^#' as a comment
readData :: String -> Double -> TimeSeries
readData str step = TimeSeries step $ map (\line -> (read line :: Double) :+ 0) $ filter (\line -> headMay line /= Just '#') $ lines str


main :: IO ()
main = do -- h1str <- readFile "H-H1_GWOSC_4KHZ_R1-1126257415-4096.txt"
          l1str <- readFile "L-L1_GWOSC_4KHZ_R1-1126257415-4096.txt"
          let l1 = readData l1str (1 / 4e3)
          let l1est = points $ psdEstimate l1 2
          print l1est
