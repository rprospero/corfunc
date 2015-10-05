{-# LANGUAGE OverloadedStrings #-}

module Main where

import Haste
import Haste.DOM hiding (attr)
import Haste.Events
import Haste.Foreign
import Data.IORef

import Corfunc
import Haste.D2.Scale
import Haste.D2.Select
import Haste.D2.SVG
import Haste.D2.SVG.Axis as Ax
import Prelude hiding (log)

width = 400 :: Double
height = 400 :: Double
margin = 100 :: Double


readAsText :: JSString -> ElemID -> IO ()
readAsText = ffi "function(name,x){var r = new FileReader;r.onload=function(q){Haste[name](q.target.result)};r.readAsText(document.getElementById(x).files[0]);}"

tupToList (a,b) = [a,b]

addPoints selection ds l = selectAll "path" selection >>= d3data ds >>= enter >>= append "path" >>= attr "d" l >>= attr "style" ("stroke: black; fill: none" :: String)

updatePage pref xref rref = do
  ps <- readIORef pref
  (xray,lx) <- readIORef xref
  let xs = map head ps
      ys = map (!! 1) ps
      fit = zipWith (curry tupToList) xs $ map (fitdata xs ys) xs
      cs = take 20 $ corfunc xs ys

  _ <- addPoints xray [ps,fit] lx

  _ <- selectAll "path" xray
      >>= transition
      >>= duration 10000
      >>= attr "d" lx

  (real,lr) <- readIORef rref

  _ <- addPoints real [map tupToList cs] lr

  _ <- selectAll "path" real
      >>= transition
      >>= duration 10000
      >>= attr "d" lr

  return ()

makeGraph canvas x y = do

  xAxis <- Ax.axis >>= Ax.scale x
          >>= Ax.orient Ax.Bottom
          >>= Ax.ticks 5

  yAxis <- Ax.axis >>= Ax.scale y
          >>= Ax.orient Ax.Left
          >>= Ax.ticks 5

  svg <- select canvas
        >>= attr "width" (width + margin)
        >>= attr "height" (height + margin)
        >>= append "g"
        >>= attr "transform" (translate (margin/2,margin/2))

  l <- line x y


  _ <- append "g" svg
      >>= attr "class" ("x axis" :: String)
      >>= attr "transform" (translate (0,height))
      >>= call xAxis


  _ <- append "g" svg
      >>= attr "class" ("y axis" :: String)
      >>= call yAxis

  local <- append "g" svg
      >>= attr "class" ("lines" :: String)
      >>= attr "clip-path" ("url(#clip)" :: String)

  return (local,l)


main :: IO ()
main = do
  Just loadPath <- elemById "loadPath"

  ps <- newIORef []

  q <- log >>= domain [1e-3, 1]
      >>= range [0,width]
  iq <- log >>= domain [1e-2, 1e5]
      >>= range [height,0]

  x <- linear >>= domain [0, 2500]
      >>= range [0,width]
  y <- linear >>= domain [-20, 80]
      >>= range [height,0]

  xref <- makeGraph "#xray" q iq >>= newIORef
  rref <- makeGraph "#real" x y >>= newIORef

  let action = updatePage ps xref rref

  export "processDump" (processDump action ps)
  _ <- onEvent loadPath Change $ const $ readAsText "processDump" "loadPath"
  return ()

processDump :: IO () -> IORef [[Double]] -> JSString -> IO ()
processDump action ds result = do
  let ps = map (map read . words) . tail . lines . toString$result
  writeIORef ds ps
  action
