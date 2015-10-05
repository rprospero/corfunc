module Graph (makeGraph, setPoints, defaultGraphMargins, GraphMargins(width,height,margin), updateXdomain,updateYdomain) where

import Haste.D2.Scale
import Haste.D2.Select
import Haste.D2.SVG
import Haste.D2.SVG.Axis as Ax
import Prelude hiding (log)

data Graph = Graph {gname :: String,
                    selection :: Select,
                    lineMaker :: Line,
                    xscale :: Scale,
                    yscale :: Scale,
                    xax:: Ax.Axis,
                    yax :: Ax.Axis}
data GraphMargins = GraphMargins {width :: Double,
                                  height :: Double,
                                  margin :: Double}

defaultGraphMargins :: GraphMargins
defaultGraphMargins = GraphMargins 400 400 100

makeGraph :: GraphMargins -> String -> Scale -> Scale -> IO Graph
makeGraph m name x y = do

  xAxis <- Ax.axis >>= Ax.scale x
          >>= Ax.orient Ax.Bottom
          >>= Ax.ticks 5

  yAxis <- Ax.axis >>= Ax.scale y
          >>= Ax.orient Ax.Left
          >>= Ax.ticks 5

  svg <- select name d3
        >>= attr "width" (width m + margin m)
        >>= attr "height" (height m + margin m)
        >>= append "g"
        >>= attr "transform" (translate (margin m/2,margin m/2))

  l <- line x y


  _ <- append "g" svg
      >>= attr "class" (tail name ++ "-x-axis" :: String)
      >>= attr "transform" (translate (0,height m))
      >>= call xAxis


  _ <- append "g" svg
      >>= attr "class" (tail name ++ "-y-axis" :: String)
      >>= call yAxis

  local <- append "g" svg
      >>= attr "class" ("lines" :: String)
      >>= attr "clip-path" ("url(#clip)" :: String)

  return $ Graph (tail name) local l x y xAxis yAxis

setPoints :: Graph -> [[[Double]]] -> IO ()
setPoints g ds = do
  _ <- selectAll "path" (selection g) >>= d3data ds >>= enter >>= append "path" >>= attr "d" (lineMaker g) >>= attr "style" ("stroke: black; fill: none" :: String)
  _ <- selectAll "path" (selection g) >>= transition >>= duration 2000 >>= attr "d" (lineMaker g)
  return ()

updateXdomain :: [Double] -> Graph -> IO ()
updateXdomain d g = do
  let marker = "."++gname g ++"-x-axis"
  _ <- domain d $ xscale g
  _ <- select marker d3 >>= transition >>= duration 2000 >>= call (xax g)
  return ()

updateYdomain :: [Double] -> Graph -> IO ()
updateYdomain d g = do
  let marker = "."++gname g ++"-y-axis"
  print d
  _ <- domain d $ yscale g
  _ <- select marker d3 >>= transition >>= duration 2000 >>= call (yax g)
  return ()
