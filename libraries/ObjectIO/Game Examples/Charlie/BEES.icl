implementation module BEES

import StdEnv, StdGameDef

BeesBitmap :: GameBitmap
BeesBitmap
  = { bitmapname  = "BEES.BMP"
    , unitsize    = { w = 20, h = 20 }
    , dimensions  = (16, 1)
    , transparent = Just { x = 59, y = 19 }
    }

BeesMap :: [{#Int}]
BeesMap = [{1,2,3,2,-1,-1,-1,-1,-1},
  {-1,-1,-1,-1,-1,-1,-1,-1,-1},
  {-1,-1,-1,-1,-1,-1,-1,-1,-1},
  {-1,-1,-1,-1,-1,-1,-1,-1,-1},
  {-1,-1,-1,-1,-1,-1,-1,-1,-1}]

BeesSeq001 :: (Int, [(Int, Int)])
BeesSeq001 = (-1,[])

BeesSequences :: [(Int, [(Int, Int)])]
BeesSequences = [BeesSeq001]