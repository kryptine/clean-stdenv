implementation module INFRONT

import StdEnv, StdGameDef

InFrontBitmap :: GameBitmap
InFrontBitmap
  = { bitmapname  = "INFRONT.BMP"
    , unitsize    = { w = 20, h = 16 }
    , dimensions  = (16, 1)
    , transparent = Just { x = 59, y = 15 }
    }

InFrontMap :: [{#Int}]
InFrontMap = [{1,2,3,-1,-1,-1,-1},
  {-1,-1,-1,-1,-1,-1,-1},
  {-1,-1,-1,-1,-1,-1,-1},
  {-1,-1,-1,-1,-1,-1,-1},
  {-1,-1,-1,-1,-1,-1,-1}]

InFrontSeq001 :: (Int, [(Int, Int)])
InFrontSeq001 = (-1,[])

InFrontSequences :: [(Int, [(Int, Int)])]
InFrontSequences = [InFrontSeq001]