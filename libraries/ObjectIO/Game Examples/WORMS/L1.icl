implementation module L1

import StdEnv, StdGameDef

Level1Bitmap :: GameBitmap
Level1Bitmap
  = { bitmapname  = "L1.BMP"
    , unitsize    = { w = 24, h = 24 }
    , dimensions  = (13, 2)
    , transparent = Nothing
    }

Level1Map :: [{#Int}]
Level1Map = [{1,2,3,3,4,3,5,2,5,3,5,3,4,3,5,5,3,4,5,3,2,3,5,4,3,3,2,1},
  {1,3,6,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,8,3,1},
  {1,3,9,10,11,11,10,10,11,11,10,10,11,11,10,10,11,11,10,10,11,11,10,10,
  11,12,3,1},
  {1,4,9,11,10,10,11,11,10,10,11,11,10,10,11,11,10,10,11,11,10,10,11,11,
  10,12,4,1},
  {1,3,9,11,10,10,11,11,10,10,11,11,10,10,11,11,10,10,11,11,10,10,11,11,
  10,12,3,1},
  {1,5,9,10,11,11,10,10,11,11,10,10,11,11,10,10,11,11,10,10,11,11,10,10,
  11,12,5,1},
  {1,2,9,10,11,11,10,10,11,11,10,10,11,11,10,10,11,11,10,10,11,11,10,10,
  11,12,2,1},
  {1,5,9,11,10,10,11,11,10,10,11,11,10,10,11,11,10,10,11,11,10,10,11,11,
  10,12,5,1},
  {1,3,9,11,10,10,11,11,10,10,11,11,10,10,11,11,10,10,11,11,10,10,11,11,
  10,12,3,1},
  {1,4,9,10,11,11,10,10,11,11,10,10,11,11,10,10,11,11,10,10,11,11,10,10,
  11,12,4,1},
  {1,3,9,10,11,11,10,10,11,11,10,10,11,11,10,10,11,11,10,10,11,11,10,10,
  11,12,3,1},
  {1,5,9,11,10,10,11,11,10,10,11,11,10,10,11,11,10,10,11,11,10,10,11,11,
  10,12,5,1},
  {1,2,9,11,10,10,11,11,10,10,11,11,10,10,11,11,10,10,11,11,10,10,11,11,
  10,12,4,1},
  {1,3,9,10,11,11,10,10,11,11,10,10,11,11,10,10,11,11,10,10,11,11,10,10,
  11,12,3,1},
  {1,4,9,10,11,11,10,10,11,11,10,10,11,11,10,10,11,11,10,10,11,11,10,10,
  11,12,4,1},
  {1,3,9,11,10,10,11,11,10,10,11,11,10,10,11,11,10,10,11,11,10,10,11,11,
  10,12,3,1},
  {1,4,9,11,10,10,11,11,10,10,11,11,10,10,11,11,10,10,11,11,10,10,11,11,
  10,12,4,1},
  {1,3,9,10,11,11,10,10,11,11,10,10,11,11,10,10,11,11,10,10,11,11,10,10,
  11,12,3,1},
  {1,3,13,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,
  14,15,3,1},
  {1,4,3,3,4,3,5,5,2,3,5,3,2,3,3,4,3,5,2,5,3,5,3,4,3,3,4,1}]

Level1Bounds :: [{#Int}]
Level1Bounds = [{15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,
  15,15,15,15,15,15,15,15},
  {15,15,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,15,15},
  {15,15,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,15,15},
  {15,15,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,15,15},
  {15,15,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,15,15},
  {15,15,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,15,15},
  {15,15,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,15,15},
  {15,15,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,15,15},
  {15,15,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,15,15},
  {15,15,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,15,15},
  {15,15,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,15,15},
  {15,15,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,15,15},
  {15,15,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,15,15},
  {15,15,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,15,15},
  {15,15,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,15,15},
  {15,15,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,15,15},
  {15,15,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,15,15},
  {15,15,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,15,15},
  {15,15,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,15,15},
  {15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,
  15,15,15,15}]
