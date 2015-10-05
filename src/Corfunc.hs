module Corfunc (corfunc,fitdata) where

import Data.Complex
import Numeric.FFT

porod :: Double -> Double -> Double -> Double
porod k sigma q = (k*q**(-4))*exp (-q**2*sigma)

guinier :: Double -> Double -> Double -> Double
guinier a b q = a * exp (b*q**2)

beta :: [Double] -> [Double] -> Double
beta x y = rval x y * std y / std x

alpha :: [Double] -> [Double] -> Double
alpha x y = mean y - beta x y * mean x

mean :: [Double] -> Double
mean xs = sum xs / fromIntegral (length xs)

std :: [Double] -> Double
std xs = sqrt $ mean (map (**2) xs) - mean xs**2

rval :: [Double] -> [Double] -> Double
rval xs ys = sum (zipWith (*) xdiff ydiff) / sqrt (sum (map (**2) xdiff) * sum (map (**2) ydiff))
    where
      diff qs = map (\z -> z - mean qs) qs
      xdiff = diff xs
      ydiff = diff ys

fitguinier :: [Double] -> [Double] -> Double -> Double
fitguinier qs is = guinier a b
    where
      xs = map (**2) qs
      ys = map log is
      a = exp $ alpha xs ys
      b = beta xs ys

fitporod :: [Double] -> [Double] -> Double -> Double
fitporod qs is = porod k sigma
    where
      is' = filter (>0) is
      qs' = filterKey (>0) is qs
      xs = map (**2) qs'
      ys = map log . zipWith (*) is' . map (**4) $ qs'
      k = exp $ alpha xs ys
      sigma = 0 * beta xs ys

filterKey :: (a->Bool) -> [a] -> [b] -> [b]
filterKey f ks = map snd . filter (f.fst) . zip ks

takeKey :: (a->Bool) -> [a] -> [b] -> [b]
takeKey f ks = map snd . takeWhile (f . fst) . zip ks

dropKey :: (a->Bool) -> [a] -> [b] -> [b]
dropKey f ks = map snd . dropWhile (f . fst) . zip ks

fitdata :: [Double] -> [Double] -> Double -> Double
fitdata qs is = let d = interp qs is
                    maxq = 0.04
                    highq = dropWhile (< maxq) qs
                    highi = dropKey (< maxq) qs is
                    fitp = fitporod highq highi
                    minq = 0.0065*3
                    lowq = takeWhile (< minq) qs
                    lowi = takeKey (< minq) qs is
                    fitg = fitguinier lowq lowi
                    s1 = smooth d fitp (maxq,maximum qs)
                    s2 = smooth fitg s1 (minimum qs,minq)
                in
                  s2

-- | Smoothly transition between two functions over a range of functions
smooth :: (Double -> Double) -> (Double -> Double) -> (Double, Double) -> Double -> Double
smooth f g (start, stop) x
    | x <= start = f x
    | x >= stop = g x
    | otherwise = h*g x +(1-h)*f x
    where
      h = 1/(1+(x-stop)**2/(start-x)**2)

interpHead :: ((Double, Double), (Double, Double)) -> Double -> Double
interpHead ((x1,x2),(y1,y2)) q = (x2-q)/(x2-x1)*y1 + (q-x1)/(x2-x1)*y2

interp :: [Double] -> [Double] -> Double -> Double
interp xs ys x =
    let ranges = zip (zip xs (tail xs)) (zip ys (tail ys))
    in
      case dropWhile (\q -> snd (fst q) < x) ranges of
        [] -> last ys
        p:_ -> interpHead p x

dct :: [Double] -> [Double]
dct = map realPart . dft . map (:+ 0)

corfunc :: [Double] -> [Double] -> [(Double,Double)]
corfunc qs is = let fullData = fitdata qs is
                    factor = 16
                    deltaq = (qs!!1-head qs)/factor
                    qgrid = map (*deltaq) [0..factor*2^(floor . log . (/ deltaq) . maximum $ qs)]
                    igrid = zipWith (*) (map (**2) qgrid) $ map fullData qgrid
                    ys = dct igrid
                    xs = map (\q -> pi * q / deltaq /fromIntegral (length qgrid)) [0..fromIntegral $ length qgrid]
                in
                  zip xs ys
