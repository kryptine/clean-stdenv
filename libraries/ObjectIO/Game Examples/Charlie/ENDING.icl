implementation module ENDING

import StdEnv, StdGameDef

EndingBitmap :: GameBitmap
EndingBitmap
  = { bitmapname  = "ENDING.BMP"
    , unitsize    = { w = 32, h = 26 }
    , dimensions  = (10, 1)
    , transparent = Just { x = 287, y = 25 }
    }

EndingMap :: [{#Int}]
EndingMap = [{1,2,3,4,5,6,7,8,9},
  {-1,-1,-1,-1,-1,-1,-1,-1,-1},
  {-1,-1,-1,-1,-1,-1,-1,-1,-1},
  {-1,-1,-1,-1,-1,-1,-1,-1,-1}]

EndingSeq001 :: (Int, [(Int, Int)])
EndingSeq001 = (-1,[])

EndingSequences :: [(Int, [(Int, Int)])]
EndingSequences = [EndingSeq001]