implementation module OBJ

import StdEnv, StdGameDef

ObjectsBitmap :: GameBitmap
ObjectsBitmap
  = { bitmapname  = "OBJ.BMP"
    , unitsize    = { w = 20, h = 16 }
    , dimensions  = (16, 2)
    , transparent = Just { x = 219, y = 31 }
    }

ObjectsMap :: [{#Int}]
ObjectsMap = [{1,2,3,4,5,6,7,8,9,10,11,12,-1},
  {13,14,15,16,17,18,19,20,21,22,23,24,-1},
  {25,26,27,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1},
  {-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1},
  {-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1},
  {-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1},
  {-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1},
  {-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1},
  {-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1}]

ObjectsSeq001 :: (Int, [(Int, Int)])
ObjectsSeq001 = (-1,[])

ObjectsSequences :: [(Int, [(Int, Int)])]
ObjectsSequences = [ObjectsSeq001]