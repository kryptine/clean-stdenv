implementation module FROGS

import StdEnv, StdGameDef

FrogsBitmap :: GameBitmap
FrogsBitmap
  = { bitmapname  = "FROGS.BMP"
    , unitsize    = { w = 20, h = 24 }
    , dimensions  = (16, 1)
    , transparent = Just { x = 139, y = 23 }
    }

FrogsMap :: [{#Int}]
FrogsMap = [{1,2,3,4,-1},
  {5,6,-1,-1,-1},
  {-1,-1,-1,7,-1},
  {-1,-1,-1,-1,-1}]

FrogsSeq001 :: (Int, [(Int, Int)])
FrogsSeq001 = (-1,[])

FrogsSequences :: [(Int, [(Int, Int)])]
FrogsSequences = [FrogsSeq001]