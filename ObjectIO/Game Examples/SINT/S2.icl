implementation module S2

import StdEnv, StdGameDef

Static2Bitmap :: GameBitmap
Static2Bitmap
  = { bitmapname  = "S2.BMP"
    , unitsize    = { w = 40, h = 24 }
    , dimensions  = (8, 2)
    , transparent = Just { x = 319, y = 47 }
    }

Static2Map :: [{#Int}]
Static2Map = [{0,0,0,0,0,0,0,0,0,0},
  {0,0,-1,0,-3,-1,0,0,0,0},
  {0,0,0,0,-4,0,-1,0,0,0},
  {3,0,4,5,0,4,6,7,5,8},
  {10,11,10,10,11,10,10,10,10,10},
  {10,10,10,10,10,10,10,10,10,10},
  {10,10,10,10,10,10,10,10,10,10},
  {10,10,10,10,10,10,10,10,10,10},
  {10,10,10,10,10,10,10,10,10,10},
  {10,10,10,10,10,10,10,10,10,10},
  {10,10,10,10,10,10,10,10,10,10},
  {10,10,10,10,10,10,10,10,10,10},
  {10,10,10,10,10,10,10,10,10,10}]

Static2Seq001 :: (Int, [(Int, Int)])
Static2Seq001 = (-1,[(1,36),(2,36)])
Static2Seq002 :: (Int, [(Int, Int)])
Static2Seq002 = (-2,[(9,38),(12,38)])
Static2Seq003 :: (Int, [(Int, Int)])
Static2Seq003 = (-3,[(13,48),(14,48)])
Static2Seq004 :: (Int, [(Int, Int)])
Static2Seq004 = (-4,[(15,48),(16,48)])

Static2Sequences :: [(Int, [(Int, Int)])]
Static2Sequences = [Static2Seq001, Static2Seq002, Static2Seq003, Static2Seq004]