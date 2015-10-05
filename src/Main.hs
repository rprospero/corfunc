{-# LANGUAGE OverloadedStrings #-}

module Main where

import Haste
import Haste.DOM hiding (attr)
import Haste.Events
import Haste.Foreign
import Data.IORef

import Haste.D2.Scale
import Prelude hiding (log)

import Corfunc
import Graph

readAsText :: JSString -> ElemID -> IO ()
readAsText = ffi "function(name,x){var r = new FileReader;r.onload=function(q){Haste[name](q.target.result)};r.readAsText(document.getElementById(x).files[0]);}"

tupToList (a,b) = [a,b]

updatePage pref xref rref = do
  ps <- readIORef pref
  xray <- readIORef xref
  let xs = map head ps
      ys = map (!! 1) ps
      fit = zipWith (curry tupToList) xs $ map (fitdata xs ys) xs
      cs = take 20 $ corfunc xs ys

  _ <- setPoints xray [ps,fit]

  real <- readIORef rref

  _ <- setPoints real [map tupToList cs]


  return ()




main :: IO ()
main = do
  Just loadPath <- elemById "loadPath"

  ps <- newIORef []

  let margins = defaultGraphMargins

  q <- log >>= domain [1e-3, 1]
      >>= range [0,width margins]
  iq <- log >>= domain [1e-2, 1e5]
      >>= range [height margins,0]

  x <- linear >>= domain [0, 2500]
      >>= range [0,width margins]
  y <- linear >>= domain [-20, 80]
      >>= range [height margins,0]

  xref <- makeGraph margins "#xray" q iq >>= newIORef
  rref <- makeGraph margins "#real" x y >>= newIORef

  let action = updatePage ps xref rref

  export "processDump" (processDump action ps)
  _ <- onEvent loadPath Change $ const $ readAsText "processDump" "loadPath"
  return ()

processDump :: IO () -> IORef [[Double]] -> JSString -> IO ()
processDump action ds result = do
  let ps = map (map read . words) . tail . lines . toString$result
  writeIORef ds ps
  action
