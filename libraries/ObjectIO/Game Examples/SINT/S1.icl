implementation module S1

import StdEnv, StdGameDef

Static1Bitmap :: GameBitmap
Static1Bitmap
  = { bitmapname  = "S1.BMP"
    , unitsize    = { w = 40, h = 24 }
    , dimensions  = (8, 2)
    , transparent = Just { x = 319, y = 47 }
    }

Static1Map :: [{#Int}]
Static1Map = [{0,0,0,0,0,0,0,0,0,0},
  {0,-3,0,0,0,0,-1,0,0,0},
  {0,-4,0,0,0,0,0,-2,0,0},
  {0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,0,0},
  {5,0,6,7,0,5,8,9,5,10},
  {11,12,11,11,12,11,11,11,11,11},
  {11,11,11,11,11,11,11,11,11,11},
  {11,11,11,11,11,11,11,11,11,11},
  {11,11,11,11,11,11,11,11,11,11},
  {11,11,11,11,11,11,11,11,11,11},
  {11,11,11,11,11,11,11,11,11,11},
  {11,11,11,11,11,11,11,11,11,11}]

Static1Seq001 :: (Int, [(Int, Int)])
Static1Seq001 = (-1,[(1,36),(2,36)])
Static1Seq002 :: (Int, [(Int, Int)])
Static1Seq002 = (-2,[(3,38),(4,38)])
Static1Seq003 :: (Int, [(Int, Int)])
Static1Seq003 = (-3,[(13,48),(14,48)])
Static1Seq004 :: (Int, [(Int, Int)])
Static1Seq004 = (-4,[(15,48),(16,48)])

Static1Sequences :: [(Int, [(Int, Int)])]
Static1Sequences = [Static1Seq001, Static1Seq002, Static1Seq003, Static1Seq004]