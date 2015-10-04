{-# LANGUAGE OverloadedStrings #-}

module Main where

import Haste
import Haste.DOM
import Haste.Events
import Haste.Foreign
import Data.IORef

import Corfunc

readAsText :: JSString -> ElemID -> IO ()
readAsText = ffi "function(name,x){var r = new FileReader;r.onload=function(q){Haste[name](q.target.result)};r.readAsText(document.getElementById(x).files[0]);}"

updatePage :: IORef [(Double,Double)] -> IO ()
updatePage pref = do
  ps <- readIORef pref
  let xs = map fst ps
      ys = map snd ps
  print xs
  print ys

main :: IO ()
main = do
  Just loadPath <- elemById "loadPath"

  ps <- newIORef []

  let action = updatePage ps

  export "processDump" (processDump action ps)
  _ <- onEvent loadPath Change $ const $ readAsText "processDump" "loadPath"
  return ()

lToTup [x,y] = (x,y)

processDump :: IO () -> IORef [(Double,Double)] -> JSString -> IO ()
processDump action ds result = do
  let ps = map (lToTup . map read . words) . tail . lines . toString$result
  writeIORef ds ps
  action
