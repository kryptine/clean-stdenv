implementation module WATER

import StdEnv, StdGameDef

WaterBitmap :: GameBitmap
WaterBitmap
  = { bitmapname  = "WATER.BMP"
    , unitsize    = { w = 20, h = 14 }
    , dimensions  = (16, 2)
    , transparent = Just { x = 79, y = 27 }
    }

WaterMap :: [{#Int}]
WaterMap = [{1,2,3,4,5,6,7,8,9,10,-1,-1},
  {11,12,13,14,15,16,17,18,19,20,-1,-1},
  {-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1},
  {-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1},
  {-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1}]

WaterSeq001 :: (Int, [(Int, Int)])
WaterSeq001 = (-1,[])

WaterSequences :: [(Int, [(Int, Int)])]
WaterSequences = [WaterSeq001]