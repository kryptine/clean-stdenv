implementation module WORM

import StdEnv, StdGameDef

WormBitmap :: GameBitmap
WormBitmap
  = { bitmapname  = "WORM.BMP"
    , unitsize    = { w = 24, h = 24 }
    , dimensions  = (13, 1)
    , transparent = Just { x = 71, y = 23 }
    }

WormMap :: [{#Int}]
WormMap = [{1,2,3,-1,-1,-1,-1,-1},
  {-1,-1,-1,-1,-1,-1,-1,-1},
  {-1,-1,-1,-1,-1,-1,-1,-1},
  {-1,-1,-1,-1,-1,-1,-1,-1},
  {-1,-1,-1,-1,-1,-1,-1,-1},
  {-1,-1,-1,-1,-1,-1,-1,-1},
  {-1,-1,-1,-1,-1,-1,-1,-1},
  {-1,-1,-1,-1,-1,-1,-1,-1},
  {-1,-1,-1,-1,-1,-1,-1,-1}]

WormSeq001 :: (Int, [(Int, Int)])
WormSeq001 = (-1,[])

WormSequences :: [(Int, [(Int, Int)])]
WormSequences = [WormSeq001]