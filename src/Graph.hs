module Graph (makeGraph, setPoints, defaultGraphMargins, GraphMargins(width,height,margin)) where

import Haste.D2.Scale
import Haste.D2.Select
import Haste.D2.SVG
import Haste.D2.SVG.Axis as Ax
import Prelude hiding (log)

data Graph = Graph Select Line Ax.Axis Ax.Axis
data GraphMargins = GraphMargins {width :: Double,
                                  height :: Double,
                                  margin :: Double}
defaultGraphMargins = GraphMargins 400 400 100

makeGraph :: GraphMargins -> String -> Scale -> Scale -> IO Graph
makeGraph m canvas x y = do

  xAxis <- Ax.axis >>= Ax.scale x
          >>= Ax.orient Ax.Bottom
          >>= Ax.ticks 5

  yAxis <- Ax.axis >>= Ax.scale y
          >>= Ax.orient Ax.Left
          >>= Ax.ticks 5

  svg <- select canvas
        >>= attr "width" (width m + margin m)
        >>= attr "height" (height m + margin m)
        >>= append "g"
        >>= attr "transform" (translate (margin m/2,margin m/2))

  l <- line x y


  _ <- append "g" svg
      >>= attr "class" ("x axis" :: String)
      >>= attr "transform" (translate (0,height m))
      >>= call xAxis


  _ <- append "g" svg
      >>= attr "class" ("y axis" :: String)
      >>= call yAxis

  local <- append "g" svg
      >>= attr "class" ("lines" :: String)
      >>= attr "clip-path" ("url(#clip)" :: String)

  return $ Graph local l x y

setPoints :: Graph -> [[[Double]]] -> IO ()
setPoints (Graph  selection l _ _) ds = do
  _ <- selectAll "path" selection >>= d3data ds >>= enter >>= append "path" >>= attr "d" l >>= attr "style" ("stroke: black; fill: none" :: String)
  _ <- selectAll "path" selection >>= transition >>= duration 20000 >>= attr "d" l
  return ()
