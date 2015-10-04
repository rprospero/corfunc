module Main where

import Corfunc

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  let qs = [0,0.1..1]
      is = map (exp . (*(-1)) . (**2)) qs
      results = corfunc qs is
      xs = map fst results
      ys = map snd results
  print xs
  print ys
