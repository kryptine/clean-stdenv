implementation module STAT

import StdEnv, StdGameDef

StatusBitmap :: GameBitmap
StatusBitmap
  = { bitmapname  = "STAT.BMP"
    , unitsize    = { w = 12, h = 12 }
    , dimensions  = (26, 1)
    , transparent = Just { x = 59, y = 11 }
    }

StatusMap :: [{#Int}]
StatusMap = [{1,2,3,4,5,5,-1,-1,-1},
  {-1,-1,-1,-1,-1,-1,-1,-1,-1},
  {-1,-1,-1,-1,-1,-1,-1,-1,-1},
  {-1,-1,-1,-1,-1,-1,-1,-1,-1},
  {-1,-1,-1,-1,-1,-1,-1,-1,-1},
  {-1,-1,-1,-1,-1,-1,-1,-1,-1}]

StatusSeq001 :: (Int, [(Int, Int)])
StatusSeq001 = (-1,[])

StatusSequences :: [(Int, [(Int, Int)])]
StatusSequences = [StatusSeq001]