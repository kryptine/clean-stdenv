implementation module CH

import StdEnv, StdGameDef

MainCharBitmap :: GameBitmap
MainCharBitmap
  = { bitmapname  = "CH.BMP"
    , unitsize    = { w = 24, h = 34 }
    , dimensions  = (13, 1)
    , transparent = Just { x = 191, y = 33 }
    }

MainCharMap :: [{#Int}]
MainCharMap = [{1,2,3,4,5,6,7,8,-1,-1,-1},
  {-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1},
  {-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1}]

MainCharSeq001 :: (Int, [(Int, Int)])
MainCharSeq001 = (-1,[])

MainCharSequences :: [(Int, [(Int, Int)])]
MainCharSequences = [MainCharSeq001]