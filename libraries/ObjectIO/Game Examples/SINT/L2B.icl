implementation module L2B

import StdEnv, StdGameDef

Level2bBitmap :: GameBitmap
Level2bBitmap
  = { bitmapname  = "L2B.BMP"
    , unitsize    = { w = 20, h = 20 }
    , dimensions  = (16, 7)
    , transparent = Just { x = 143, y = 119 }
    }

Level2bMap :: [{#Int}]
Level2bMap = [{-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,5,6,-3,-3,-3,-3,7,8,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,5,9,6,-3,-3,-3,10,11,12,13,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,5,9,14,15,6,-3,-3,10,-3,11,12,-3,13,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,5,14,15,14,6,-3,10,-3,-3,11,12,-3,-3,13,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,5,9,14,6,10,-3,-3,-3,16,17,-3,-3,-3,13,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,7,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,
  18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,19,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,20,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,
  21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,17,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,22,11,12,23,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,24,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,25,11,12,26,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,27,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,25,11,12,26,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,27,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,25,11,12,26,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,27,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,25,11,12,26,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,27,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,25,11,12,26,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,27,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,25,11,12,26,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,27,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,25,11,12,26,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,27,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,25,11,12,26,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,27,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,25,11,12,26,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,27,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,25,11,12,26,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,27,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,25,11,12,26,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,28,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,25,11,12,26,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,29,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,25,11,12,26,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,30,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,31,-3,-3,25,11,12,26,-3,-3,31,-3,-3,-3,
  -3,-3,-3,-3,-3,32,33,34,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,35,-3,-3,25,11,12,26,-3,-3,35,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,36,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,35,-3,-3,25,11,12,26,-3,-3,35,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,35,-3,-3,25,11,12,26,-3,-3,35,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,35,-3,-3,25,11,12,26,-3,-3,35,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,35,-3,-3,25,11,12,26,-3,-3,35,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,35,-3,-3,25,11,12,26,-3,-3,35,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,35,-3,-3,25,11,12,26,-3,-3,35,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,35,-3,-3,25,11,12,26,-3,-3,35,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,37,-3,-3,25,11,12,26,-3,-3,37,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,25,11,12,26,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,25,11,12,26,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,25,11,12,26,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,25,11,12,26,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,25,11,12,26,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,25,11,12,26,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,25,11,12,26,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,25,11,12,26,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,10,25,11,12,26,13,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,10,-3,25,11,12,26,-3,13,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,38,39,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,10,-3,-3,25,11,12,26,-3,-3,13,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,40,41,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,42,43,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,10,-3,-3,-3,25,11,12,26,-3,-3,-3,13,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,44,45,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,46,47,48,
  49,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,50,51,-3,-3,-3,-3,5,6,-3,-3,25,11,12,26,-3,-3,52,-3,13,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,53,54,50,51,-3,-3,-3,-3,-3,50,51,-3,55,42,56,57,
  -3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,58,59,-3,-3,-3,-3,5,14,6,-3,60,11,12,26,-3,-3,5,15,6,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,61,62,58,59,-3,-3,-3,-3,-3,58,59,-3,63,64,65,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,50,51,50,51,-3,-3,66,67,67,67,67,67,67,68,68,68,68,68,68,69,-3,
  -3,-3,-3,-3,-3,-3,-3,50,51,-3,40,41,-3,-3,-3,-3,40,41,40,41,-3,70,65,-3,
  -3,-3,-3,71,-3,-3,-3,-3,-3},
  {72,73,-3,58,59,58,74,75,73,76,77,77,77,78,79,79,80,80,81,82,82,82,83,52,
  84,75,73,-3,-3,-1,-1,58,59,-1,44,45,52,84,75,73,44,45,44,45,-1,85,86,52,84,
  75,73,87,-3,-1,88,75,73},
  {89,90,91,89,92,92,92,89,89,89,90,92,92,89,89,90,89,89,89,89,90,92,92,89,
  93,94,93,95,96,95,95,95,93,97,96,96,89,93,94,93,95,92,92,92,91,92,92,89,
  93,94,93,95,89,92,92,91,92},
  {98,99,100,98,99,99,99,99,99,98,99,99,99,99,98,99,99,98,99,
  98,99,99,99,99,98,99,98,99,101,99,98,101,98,101,101,101,98,101,99,
  98,101,101,101,101,98,101,99,98,101,101,101,101,98,101,101,99,98},
  {99,99,100,99,102,102,102,99,100,99,99,102,102,100,99,99,100,99,
  100,99,99,102,102,100,99,100,99,98,103,98,101,98,101,101,102,103,99,
  101,98,99,101,101,102,103,99,101,98,99,101,101,102,103,99,101,101,98,
  99},
  {100,102,99,100,98,98,98,100,99,100,102,98,98,99,100,102,99,100,99,
  100,102,98,98,99,100,101,98,99,98,101,98,101,98,102,98,98,98,102,99,
  98,102,102,98,98,98,102,99,98,102,102,98,98,98,102,102,99,98},
  {99,98,99,99,99,99,99,99,99,99,98,99,99,99,99,98,99,99,99,
  99,98,99,99,99,99,99,101,99,99,101,101,101,99,98,101,101,99,98,
  99,99,98,98,101,101,99,98,99,99,98,98,101,101,99,98,98,99,99}]

Level2bBounds :: [{#Int}]
Level2bBounds = [{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,33024,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,0,7,13,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4352,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,7,5,13,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4352,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,7,5,5,5,13,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  4352,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,7,5,5,5,13,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,33024,
  0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,7,5,5,13,0,0,61440,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
  1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,33024,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,24832,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,256,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,33024,0,0,0,0,0,0,24832,0,0,4096,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,24832,24832,0,0,0,0,33024,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,0,0,0,4864,0,0,0,0,0,0,0,0,0,0,0,0,0,0,33024,0,0,0,256,
  0,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4608,0,0,
  0,0,0,0,0,0,0,0,0,0,24832,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,256,0,0,0,0,0,0,0,256,4352,0,0,0,0,0,0,33024,0,0,0,0,0,0,0,0,0,0,
  4352,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,33024,0,0,0,0,0,0,0,4352,0,0,0,0,0,0,0,0,0,0,0,0,0,24832,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,24832,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,25088,0,24832,0,0,0,
  0,4352,0,0,0,0,0,24832,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,33024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,0,0,0,8449,0,0,0,0,0,0,0,0,8449,0,0,0,0,0,25088,0,0,7,
  5,13,0,0,24832,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,4096,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,33024,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,4096,0,
  0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,25088,0,0,0,25088,
  0,0,0,25088,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,
  0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,33024,0,0,0,0,0,0,4096,
  0,0,0,0,4096,0,25088,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,4096,0,0,24832,0,0,0,8449,0,0,0,0,0,0,0,0,8449,0,0,0,0,0,0,0,0,
  4096,0,4096,768,0,4096,0,4096,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0},
  {0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,24832,0,0,0,0,0,0,0,
  4608,0,0,0,0,0,0,0,25088,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,24832,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,256,0,0,0,0,0,0,24832,0,4096,0,0,0,0,0,0,0,0,33024,0,0,0,0,0,0,0,25088,
  25088,0,0,0,25088,25088,0,0,0,25088,25088,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0},
  {0,33024,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,0,0,24832,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,33024,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,0,0,0,0,0,24832,0,0,0,0,0,0,4096,0,0,0,33024,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,24832,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,
  0,0,4352,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,24832,24832,24832,0,0,0,0,0,0,0,
  0,0,0,0,4352,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,
  0,0,0,0,0,33024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,33024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,4352,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,33024,0,0,0,0,4096,0,0,0,
  0,0,3,9,0,0,0,0,0,0,0,0,0,4352,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4608,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  6,12,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,33024,0,0,3,9,0,256,0,0,7,13,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,
  0,0,0,3,9,3,9,0,0,0,0,0,3,9,0,0,0,0,1,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,6,12,0,33024,4352,0,7,5,13,0,0,0,0,0,0,0,7,5,13,0,256,0,0,0,0,
  0,0,0,6,12,6,12,33024,0,512,0,0,6,12,0,1,0,0,0,5120,0,0,0,0,0,0,0,0},
  {0,4352,0,3,9,3,9,0,0,3,1,1,1,1,1,1,1,1,1,1,1,1,9,0,33024,0,0,0,0,0,
  0,3,9,0,3,9,0,0,4608,0,3,9,3,9,0,0,0,0,0,0,0,65024,0,0,0,0,0},
  {0,0,0,6,12,6,12,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,23808,23808,
  6,12,23808,6,12,0,0,0,0,6,12,6,12,23808,0,0,0,0,0,0,0,0,23808,0,0,0},
  {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1},
  {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}]

Level2bSeq001 :: (Int, [(Int, Int)])
Level2bSeq001 = (-1,[(1,37),(2,37)])
Level2bSeq002 :: (Int, [(Int, Int)])
Level2bSeq002 = (-2,[(3,48),(4,48)])
Level2bSeq003 :: (Int, [(Int, Int)])
Level2bSeq003 = (-3,[])

Level2bSequences :: [(Int, [(Int, Int)])]
Level2bSequences = [Level2bSeq001, Level2bSeq002, Level2bSeq003]