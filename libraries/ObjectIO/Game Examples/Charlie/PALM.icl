implementation module PALM

import StdEnv, StdGameDef

PalmFrontBitmap :: GameBitmap
PalmFrontBitmap
  = { bitmapname  = "PALM.BMP"
    , unitsize    = { w = 20, h = 32 }
    , dimensions  = (16, 1)
    , transparent = Just { x = 179, y = 31 }
    }

PalmFrontMap :: [{#Int}]
PalmFrontMap = [{1,2,3,4,5,6,7,8,9},
  {-1,-1,-1,-1,-1,-1,-1,-1,-1}]

PalmFrontSeq001 :: (Int, [(Int, Int)])
PalmFrontSeq001 = (-1,[])

PalmFrontSequences :: [(Int, [(Int, Int)])]
PalmFrontSequences = [PalmFrontSeq001]