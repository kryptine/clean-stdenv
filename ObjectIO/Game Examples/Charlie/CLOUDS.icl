implementation module CLOUDS

import StdEnv, StdGameDef

CloudsBitmap :: GameBitmap
CloudsBitmap
  = { bitmapname  = "CLOUDS.BMP"
    , unitsize    = { w = 40, h = 24 }
    , dimensions  = (8, 1)
    , transparent = Just { x = 159, y = 23 }
    }

CloudsMap :: [{#Int}]
CloudsMap = [{0,0,0,0,0},
  {0,0,0,0,0},
  {0,0,0,0,0},
  {0,0,0,0,0},
  {0,0,0,0,0},
  {0,0,0,0,0},
  {0,0,0,0,0}]

CloudsSeq001 :: (Int, [(Int, Int)])
CloudsSeq001 = (-1,[(1,36),(2,36)])
CloudsSeq002 :: (Int, [(Int, Int)])
CloudsSeq002 = (-2,[(3,36),(4,36)])

CloudsSequences :: [(Int, [(Int, Int)])]
CloudsSequences = [CloudsSeq001, CloudsSeq002]