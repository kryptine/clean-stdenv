implementation module MC

import StdEnv, StdGameDef

MainCharBitmap :: GameBitmap
MainCharBitmap
  = { bitmapname  = "MC.BMP"
    , unitsize    = { w = 32, h = 48 }
    , dimensions  = (10, 1)
    , transparent = Just { x = 255, y = 47 }
    }

MainCharMap :: [{#Int}]
MainCharMap = [{1,2,3,4,5,6,7,8,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1},
  {-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1},
  {-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1},
  {-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1},
  {-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1},
  {-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1}]

MainCharSeq001 :: (Int, [(Int, Int)])
MainCharSeq001 = (-1,[])

MainCharSequences :: [(Int, [(Int, Int)])]
MainCharSequences = [MainCharSeq001]