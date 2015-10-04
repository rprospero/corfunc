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
margin = 50 :: Double


readAsText :: JSString -> ElemID -> IO ()
readAsText = ffi "function(name,x){var r = new FileReader;r.onload=function(q){Haste[name](q.target.result)};r.readAsText(document.getElementById(x).files[0]);}"

updatePage pref svgref = do
  ps <- readIORef pref
  (svg,l) <- readIORef svgref
  let xs = map head ps
      ys = map (!! 1) ps

  _ <- append "g" svg
      >>= attr "class" ("lines" :: String)
      >>= attr "clip-path" ("url(#clip)" :: String)
      >>= selectAll "path"
      >>= d3data [ps]
      >>= enter
      >>= append "path"
      >>= attr "d" l
      >>= attr "style" ("stroke: black; fill: none" :: String)

  print xs
  print ys


makeGraph = do
  x <- log >>= domain [1e-3, 1]
      >>= range [0,width]
  y <- log >>= domain [1e-2, 1e5]
      >>= range [height,0]

  xAxis <- Ax.axis >>= Ax.scale x
          >>= Ax.orient Ax.Bottom
          >>= Ax.ticks 5

  yAxis <- Ax.axis >>= Ax.scale y
          >>= Ax.orient Ax.Left
          >>= Ax.ticks 5

  l <- line x y

  svg <- select "body"
        >>= append "svg"
        >>= attr "width" (width + margin)
        >>= attr "height" (height + margin)
        >>= append "g"
        >>= attr "transform" (translate (margin/2,margin/2))


  _ <- append "g" svg
      >>= attr "class" ("x axis" :: String)
      >>= attr "transform" (translate (0,height))
      >>= call xAxis


  _ <- append "g" svg
      >>= attr "class" ("y axis" :: String)
      >>= call yAxis

  return (svg,l)


main :: IO ()
main = do
  Just loadPath <- elemById "loadPath"

  ps <- newIORef []

  (svg,l) <- makeGraph
  svgref <- newIORef (svg,l)

  let action = updatePage ps svgref

  export "processDump" (processDump action ps)
  _ <- onEvent loadPath Change $ const $ readAsText "processDump" "loadPath"
  return ()

processDump :: IO () -> IORef [[Double]] -> JSString -> IO ()
processDump action ds result = do
  let ps = map (map read . words) . tail . lines . toString$result
  writeIORef ds ps
  action
