implementation module PART

import StdEnv, StdGameDef

PartBitmap :: GameBitmap
PartBitmap
  = { bitmapname  = "PART.BMP"
    , unitsize    = { w = 12, h = 8 }
    , dimensions  = (26, 1)
    , transparent = Just { x = 11, y = 7 }
    }

PartMap :: [{#Int}]
PartMap = [{1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1},
  {-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1},
  {-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1},
  {-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1},
  {-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1},
  {-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1},
  {-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1},
  {-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1},
  {-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1}]

PartSeq001 :: (Int, [(Int, Int)])
PartSeq001 = (-1,[])

PartSequences :: [(Int, [(Int, Int)])]
PartSequences = [PartSeq001]