implementation module HEAD

import StdEnv, StdGameDef

HeadBitmap :: GameBitmap
HeadBitmap
  = { bitmapname  = "HEAD.BMP"
    , unitsize    = { w = 28, h = 28 }
    , dimensions  = (11, 1)
    , transparent = Just { x = 139, y = 27 }
    }

HeadMap :: [{#Int}]
HeadMap = [{1,2,3,4,5,-1,-1},
  {-1,-1,-1,-1,-1,-1,-1},
  {-1,-1,-1,-1,-1,-1,-1},
  {-1,-1,-1,-1,-1,-1,-1}]

HeadSeq001 :: (Int, [(Int, Int)])
HeadSeq001 = (-1,[])

HeadSequences :: [(Int, [(Int, Int)])]
HeadSequences = [HeadSeq001]