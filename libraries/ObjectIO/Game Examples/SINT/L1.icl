implementation module L1

import StdEnv, StdGameDef

Level1Bitmap :: GameBitmap
Level1Bitmap
  = { bitmapname  = "L1.BMP"
    , unitsize    = { w = 20, h = 20 }
    , dimensions  = (16, 23)
    , transparent = Just { x = 259, y = 419 }
    }

Level1Map :: [{#Int}]
Level1Map = [{-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,5,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,6,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,5,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,6,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,5,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,6,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,6,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,7,-3,6,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,7,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,6,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,6,8,9,-3,10,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,11,12,6,10,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,13,14,-3,10,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,6,
  15,16,-3,10,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,6,17,18,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,19,20,6,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,21,22,-3,-3,-3,-3,-3,5,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,6,
  23,24,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,25,26,27,27,28,29,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,30,31,32,32,33,34,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,35,36,37,37,38,39,-3,-3,-3,6,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,30,31,
  32,32,33,34,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,40,41,42,43,43,44,45,46,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,40,41,43,43,43,43,45,46,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,47,48,49,49,49,49,50,51,-3,-3,6,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,52,53,54,
  55,55,54,56,57,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,40,58,59,60,61,61,62,63,64,46,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,40,58,65,60,66,66,62,67,64,
  46,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,47,68,69,70,71,71,72,73,74,51,-3,6,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,52,75,76,77,
  78,78,79,80,81,57,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,40,58,59,82,20,83,84,19,85,63,64,46,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,40,58,65,82,86,87,88,89,85,67,
  64,46,-3,-3,-3,-3,-3,-3,-3,-3,47,68,69,90,91,92,93,94,95,73,74,51,6,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,52,75,76,96,24,
  92,93,23,96,80,81,57,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,25,26,97,43,85,85,20,98,99,19,85,85,43,100,28,29,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,30,31,101,102,85,85,86,103,104,89,
  85,85,102,105,33,34,-3,-3,-3,-3,35,36,106,107,95,95,91,108,109,94,95,95,
  107,110,38,39,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,30,31,
  111,55,96,96,24,108,109,23,112,96,55,113,33,34,-3,-3,-3,-3,-3,-3,-3,114,
  115,-3,10,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,116,102,82,85,85,117,118,118,
  119,120,85,85,102,121,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,116,102,82,85,85,117,122,122,119,
  120,123,85,102,124,-3,-3,-3,-3,-3,-3,125,107,90,95,95,126,127,127,128,129,
  95,95,107,130,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  131,55,96,96,112,132,133,133,134,96,96,96,55,135,-3,-3,-3,-3,-3,-3,-3,-3,
  136,137,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,138,60,61,61,62,60,61,
  61,62,60,61,61,62,20,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,89,60,66,66,62,60,66,66,62,60,66,
  66,62,86,-3,-3,-3,-3,-3,-3,94,70,71,71,72,70,71,71,72,70,71,71,72,91,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,23,77,78,78,79,77,
  78,78,79,77,78,78,79,24,-3,-3,-3,-3,-3,-3,30,31,32,32,33,34,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,138,20,83,84,19,20,83,84,19,20,83,84,19,20,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,89,86,87,88,89,86,87,88,89,86,87,
  88,89,86,-3,-3,-3,-3,-3,-3,94,91,92,93,94,91,92,93,94,91,92,93,94,91,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,23,24,92,93,23,24,
  92,93,23,24,92,93,23,24,-3,-3,-3,-3,-3,139,140,141,142,142,141,143,144,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,145,20,83,84,19,20,83,84,19,20,83,84,19,
  146,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,147,86,87,88,89,86,87,88,89,86,
  87,88,89,146,-3,-3,-3,-3,-3,-3,148,149,92,93,148,149,92,93,148,149,92,93,
  148,149,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,150,24,
  92,93,23,24,92,93,23,24,92,93,23,151,-3,-3,-3,-3,139,152,153,154,155,155,
  156,157,158,144,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,138,20,98,99,19,20,98,99,
  19,20,98,99,19,20,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,159,160,-3,-3,89,86,103,104,89,86,103,104,89,
  86,103,104,89,86,-3,-3,-3,-3,-3,-3,94,91,108,109,94,91,108,109,94,91,108,
  109,94,161,38,39,-3,-3,-3,-3,-3,162,163,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,23,
  24,108,109,23,24,108,109,23,24,108,109,23,24,-3,-3,-3,139,152,153,164,137,
  165,166,136,164,157,158,144,-3,-3,-3,-3,-3,-3,162,163,-3,-3,-3,-3,-3,-3,
  -3,-3,162,163,-3,-3,-3,-3,-3,-3,162,163,-3,-3,-3,-3,-3,-3,-3,145,20,98,
  99,19,20,98,99,19,20,98,99,19,146,-3,-3,-3,-3,-3,-3,-3,-3,-3,162,163,
  -3,-3,-3,-3,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,167,168,-3,-3,89,117,122,122,119,117,122,122,
  119,117,122,122,119,86,-3,-3,-3,-3,-3,-3,94,126,127,127,128,126,127,127,
  128,126,127,127,128,107,49,50,51,-3,-3,-3,-3,169,170,-3,-3,159,160,-3,-3,
  -3,-3,-3,-3,23,132,133,133,134,132,133,133,134,132,133,133,134,24,-3,30,
  31,171,142,164,164,137,172,173,136,164,164,142,174,33,34,-3,-3,-3,-3,169,
  170,-3,-3,159,160,-3,-3,-3,-3,169,170,-3,-3,-3,-3,-3,-3,169,170,-3,-3,159,
  160,-3,-3,-3,145,117,118,118,119,117,118,118,119,117,118,118,119,146,-3,
  -3,-3,-3,-3,-3,-3,-3,-3,169,170,-3,-3,159,160,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,175,176,-3,-3,147,85,120,85,85,85,85,85,85,
  120,85,85,85,146,-3,-3,177,-3,-3,-3,94,95,129,95,95,95,95,95,95,129,95,
  95,95,95,95,107,110,38,39,-3,-3,178,179,-3,-3,167,168,-3,-3,-3,-3,-3,-3,
  150,96,96,96,96,96,96,96,96,96,180,96,96,151,-3,-3,181,142,182,164,164,
  183,184,184,185,164,164,164,142,186,-3,-3,-3,-3,-3,178,179,-3,-3,167,168,
  -3,-3,-3,-3,178,179,-3,-3,-3,-3,-3,-3,178,179,-3,-3,167,168,-3,-3,-3,145,
  85,187,188,85,85,85,85,85,120,85,85,85,146,-3,-3,177,-3,-3,-3,-3,-3,-3,
  178,179,-3,-3,167,168,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,189,160,167,190,-3,-3,17,60,66,66,62,85,85,120,85,
  60,66,66,62,146,-3,-3,191,-3,-3,-3,94,70,71,71,72,95,95,129,95,70,71,71,
  72,70,71,71,192,193,-3,194,163,169,195,196,-3,175,176,159,197,-3,-3,-3,
  -3,150,77,78,78,79,96,96,198,96,77,78,78,79,151,-3,-3,136,154,155,155,156,
  164,164,199,182,154,155,155,156,137,-3,-3,-3,194,163,169,195,196,-3,175,
  176,159,197,194,163,169,195,-3,-3,-3,-3,194,163,169,195,196,-3,175,176,
  159,197,-3,138,60,61,61,62,85,85,120,85,60,61,61,62,20,-3,-3,191,-3,-3,
  -3,-3,194,163,169,195,196,-3,175,176,159,197},
  {-3,-3,-3,-3,-3,-3,-3,-3,200,201,202,203,-3,89,86,87,88,89,60,66,66,62,
  86,87,88,89,86,-3,-3,191,-3,-3,-3,94,91,92,93,94,70,71,71,72,91,92,93,94,
  91,92,93,94,91,-3,-3,204,205,206,207,208,167,209,210,-3,-3,-3,-3,-3,23,
  24,92,93,23,77,78,78,79,24,92,93,23,24,-3,-3,136,137,211,212,136,154,155,
  155,156,137,211,212,136,137,-3,-3,-3,-3,204,205,206,207,208,167,209,210,
  -3,-3,204,205,213,196,-3,-3,-3,-3,204,205,206,207,208,167,209,210,-3,-3,
  145,20,83,84,19,60,61,61,62,20,83,84,19,146,-3,-3,191,-3,-3,-3,-3,-3,214,
  205,206,215,208,167,209,216,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,217,209,210,-3,89,86,87,88,89,86,218,219,220,
  86,87,88,89,86,-3,-3,191,-3,-3,-3,148,149,92,93,148,221,222,223,148,149,
  92,93,148,149,92,93,148,149,-3,-3,-3,224,225,-3,200,201,226,-3,-3,-3,-3,
  -3,-3,23,24,92,93,23,24,222,223,227,24,92,93,23,24,-3,-3,228,137,165,166,
  136,229,222,223,230,137,165,166,136,229,-3,-3,-3,231,-3,224,225,-3,200,
  201,226,-3,-3,-3,-3,224,206,207,-3,-3,-3,-3,-3,224,225,-3,200,201,226,-3,
  -3,-3,138,20,83,84,19,232,233,234,138,20,83,84,19,20,-3,-3,191,-3,-3,-3,
  -3,-3,-3,224,225,-3,235,201,226,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,217,226,-3,-3,89,86,103,104,89,86,236,237,89,
  86,103,104,89,86,-3,-3,191,-3,-3,-3,94,91,108,109,94,91,238,239,94,91,108,
  109,94,91,108,109,94,91,-3,-3,-3,224,225,-3,-3,217,226,-3,-3,-3,-3,-3,-3,
  23,24,108,109,23,24,240,241,23,24,108,109,23,24,-3,-3,136,137,172,173,136,
  137,238,239,242,137,172,173,136,137,-3,-3,-3,243,-3,224,225,-3,-3,217,226,
  -3,-3,-3,-3,224,225,-3,-3,-3,-3,-3,-3,224,225,-3,-3,217,226,-3,-3,-3,138,
  20,98,99,19,244,245,246,138,20,98,99,19,20,-3,-3,191,-3,-3,-3,-3,-3,-3,
  224,225,-3,-3,217,226,-3,-3},
  {-3,-3,-3,-3,-3,-3,-3,-3,-3,247,248,-3,-3,145,117,122,122,119,86,249,250,
  89,117,122,122,119,146,-3,-3,191,-3,-3,-3,94,126,127,127,128,91,251,252,
  94,126,127,127,128,126,127,127,253,254,-3,-3,-3,255,256,-3,-3,217,226,-3,
  -3,-3,-3,-3,-3,150,132,133,133,134,24,251,252,23,132,133,133,134,151,-3,
  -3,136,183,184,184,185,137,251,252,136,183,184,184,185,137,-3,-3,-3,243,
  -3,255,256,-3,-3,217,226,-3,-3,-3,-3,255,256,-3,-3,257,258,-3,-3,255,256,
  -3,-3,217,226,-3,-3,259,145,117,118,118,119,244,260,261,138,117,118,118,
  119,146,-3,262,263,-3,-3,-3,-3,-3,-3,255,256,264,-3,217,226,-3,-3},
  {-1,265,-3,-3,-3,-3,-3,-1,266,267,268,269,270,271,272,273,273,274,275,276,
  277,278,62,85,279,280,281,282,283,191,283,284,285,286,287,288,288,289,290,
  291,292,293,72,294,287,288,288,95,295,296,297,266,298,299,300,301,302,-3,
  267,268,-3,-3,-3,-1,303,298,304,96,305,306,307,308,309,310,311,79,96,96,
  96,151,-1,303,312,313,314,314,315,316,317,318,319,156,320,314,321,229,-3,
  -3,266,243,298,300,301,299,-3,267,268,322,266,-1,303,300,301,303,-1,243,243,
  -3,-2,300,301,299,303,267,268,303,-1,323,324,325,273,273,274,326,327,328,
  329,62,85,330,85,146,266,331,332,333,298,-3,303,303,303,300,301,334,303,
  267,268,303,299},
  {335,336,337,338,339,340,339,340,341,342,343,336,344,345,346,337,347,338,
  338,341,336,343,336,345,348,336,341,345,342,349,341,336,343,336,341,341,
  341,336,343,336,345,348,336,341,345,342,349,341,336,336,345,348,336,341,
  345,342,349,341,336,343,336,341,343,336,345,348,336,341,345,342,349,341,
  341,336,343,336,345,348,336,341,345,342,349,341,336,341,345,342,349,341,
  341,336,343,336,345,348,336,341,348,348,336,341,348,348,336,341,345,342,
  349,341,336,341,345,342,349,349,349,349,342,349,342,349,341,336,341,345,
  342,341,342,349,341,336,349,349,349,342,349,342,349,341,336,349,349,342,
  349,342,349,349,342,342,349,349,342,349,342,349,349,342,349},
  {350,351,352,350,351,351,351,353,351,352,351,351,350,351,352,352,354,355,
  352,351,352,351,351,351,351,350,352,354,355,353,351,352,351,350,356,351,
  351,352,351,351,351,351,350,352,354,355,353,351,352,351,351,351,350,352,
  354,355,353,351,352,351,350,351,351,351,351,351,350,352,354,355,353,351,
  351,352,351,351,351,351,350,352,354,355,353,351,350,352,354,355,353,351,
  351,352,351,351,351,351,350,352,354,351,350,352,354,351,357,352,354,355,
  353,351,350,352,354,355,354,354,354,354,355,353,355,353,351,350,352,354,
  355,351,351,353,351,351,352,351,351,351,353,355,353,351,350,351,358,355,
  353,355,353,351,351,351,358,358,355,353,355,353,351,351,353},
  {351,351,357,352,351,351,351,350,350,354,352,43,354,351,354,351,352,351,
  350,351,354,352,357,354,351,354,350,352,351,350,43,354,352,357,351,351,
  351,354,352,357,354,351,354,350,352,351,350,43,354,357,354,351,354,350,
  352,351,350,43,354,352,357,351,354,354,354,351,354,350,352,351,350,43,351,
  354,352,357,354,351,354,350,352,351,350,43,354,350,352,351,350,43,351,354,
  352,357,354,351,354,350,352,351,354,350,352,351,354,350,352,351,350,43,
  354,350,352,351,352,352,352,352,351,350,351,350,43,354,350,352,351,351,
  351,351,351,351,350,351,352,351,350,351,350,43,354,351,352,351,350,351,
  350,351,351,351,352,352,351,350,351,353,351,353,351},
  {351,352,351,351,351,353,351,351,352,351,351,351,351,352,352,351,351,351,
  351,351,351,351,351,352,352,351,351,351,351,351,351,351,351,351,359,351,
  351,351,351,351,352,352,351,351,351,351,351,351,351,351,352,352,351,351,
  351,351,351,351,351,351,351,351,351,351,352,352,351,351,351,351,351,351,
  351,351,351,351,352,352,351,351,351,351,351,351,351,351,351,351,351,351,
  351,351,351,351,352,352,351,351,351,352,351,351,351,352,351,357,351,351,
  351,351,351,351,351,351,351,351,351,351,351,351,351,357,351,351,351,351,
  351,351,351,351,351,43,351,351,351,350,355,351,357,351,351,352,351,351,
  351,351,357,351,351,351,351,351,351,351,351,357,353,351,353}]

Level1Bounds :: [{#Int}]
Level1Bounds = [{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4352,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,12288,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,33024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  33024,0,0,0,0,4352,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,12288,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  4352,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,12288,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,4352,0,0,0,0,0,0,0,0,33024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,10240,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,9984,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,9472,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,9728,0,0,0,0,4352,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4352,0,0,0,1,1,0,0,0,0,0,256,0,0,0,0,0,0,
  0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,33024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,1,1,0,0,0,0,0,0,33024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,33024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,33024,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,0,0,0,0,0,0,4352,0,0,0,0,0,0,0,0,0,0,0,33024,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,12288,0,0,0,0,0,0,0,0,0,0,0,0,0,0,33024,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,33024,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  128,1,1,1,1,129,0,0,0,768,0,0,33024,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,1,1,1,1,129,0,0,0,0,0,0,0,0,0,0,
  4352,0,0,0,128,1,1,1,1,129,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,128,1,1,1,1,129,0,0,0,0,0,0,4352,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,
  0,0,0,0,0,0,129,0,0,4608,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,129,0,0,0,0,0,0,0,0,0,
  0,0,0,128,0,0,0,0,0,0,129,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,128,0,0,0,0,0,0,129,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,33024,
  0,0,0,0,0,0,0,0,33024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4352,0,
  128,0,0,0,0,0,0,0,0,129,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,129,0,0,0,0,0,0,0,0,
  0,0,128,0,0,0,1024,0,0,0,0,129,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,
  0,0,0,0,128,0,0,0,0,0,0,0,0,129,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,0,0,0,
  0,0,0,0,0,0,0,129,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,129,0,0,0,0,0,0,0,
  0,128,0,0,0,0,5888,0,0,0,0,0,129,0,0,0,0,0,0,0,0,0,0,0,0,0,33024,0,0,0,
  0,0,0,4352,128,0,0,0,0,0,0,0,0,0,0,129,0,0,0,0,0,0,0,0,0,10496,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,
  1,0,0,0,0,0,0,0,0,0,0,0,0,1,129,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,0,0,0,0,128,1,0,0,0,0,0,0,0,0,0,0,0,0,1,129,0,0,0,0,128,
  1,0,0,0,0,0,0,0,0,0,0,0,0,1,129,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,1,
  0,0,0,0,0,0,0,0,0,0,0,0,1,129,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,33024,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,256,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,33024,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,33024,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4352,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,4352,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,1,1,1,1,129,0,4352,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,33024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,768,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,129,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4352,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4352,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,33024,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,4352,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,129,0,6400,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,33024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,
  0,0,0,0,0,0,0,0,0,0,0,0,1,1,129,0,4352,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,4352,0,0,0,0,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,4352,0,0,0,0,129,0,0,0,
  0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,33024,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,1,1,0,0,1,1,0,0,0,0,8192,0,0,33024,
  0,0,1,1,0,0,1,1,0,0,1,1,0,0,0,0,129,0,0,0,4352,0,0,0,0,0,0,256,0,0,0,0,
  0,0,0,1,1,0,0,1,1,0,0,1,1,0,0,0,128,1,0,0,0,0,0,0,0,0,0,0,0,0,1,129,0,0,
  0,0,0,0,0,0,0,0,4608,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,
  0,0,1,1,0,0,1,1,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,0,0,0,0,33024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,129,0,0,0,0,0,0,0,0,4608,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,
  0,4352,0,0,0,0,0,0,0,0,0,4352,0,0,0,33024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,4352,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4352,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,33024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,33024,
  0,0,0,0,0,0,0,0,0,4352,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,1,0,0,5376,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,4352,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,1,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4352,0,0,0,0,0,0,0,0,0,0,0,0,33024,0,0,
  1,0,0,1,0,0,0,1,0,0,1,0,0,0,0,0,0,0,1,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,256,4352,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4352,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,33024,0,0,0,4352,1,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,1,0,0,0,256,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,4352,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4608,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4352,0,0,0,0,0,768,0,0,0,
  0,0,0,0,0,0,4352,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,4352,4352,0,0,0,0,0,0,0,0,4352,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,
  1,1,0,0,0,0,0,0,1,1,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,6144,0,0,0,0,1,
  1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,4864,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,
  1,0,0,0,0,0,0,0,5632,0,0,0,0,0,65024,0,0,0,0,0},
  {0,0,0,61440,0,0,0,0,0,0,0,24320,24064,24064,24064,4352,4352,0,128,1,1,
  129,0,0,0,23808,0,0,0,0,0,24320,24064,24064,24064,24064,24064,0,128,1,1,
  129,0,24320,24064,24064,24064,0,0,0,0,23808,23552,0,0,0,0,0,0,0,0,0,0,0,
  0,23552,0,0,0,0,0,128,1,1,129,0,0,0,0,0,23808,0,0,0,23808,0,0,128,1,1,129,
  0,0,0,0,0,4352,0,0,0,23552,0,0,0,0,0,0,0,23808,0,0,0,0,0,0,0,0,0,23808,
  0,0,0,16384,0,0,0,0,0,0,0,0,0,0,128,1,1,129,0,0,0,0,0,23808,0,0,0,23552,
  0,0,0,0,0,0,0,0,0,0,0,0},
  {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1},
  {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}]

Level1Seq001 :: (Int, [(Int, Int)])
Level1Seq001 = (-1,[(1,37),(2,37)])
Level1Seq002 :: (Int, [(Int, Int)])
Level1Seq002 = (-2,[(3,48),(4,48)])
Level1Seq003 :: (Int, [(Int, Int)])
Level1Seq003 = (-3,[])

Level1Sequences :: [(Int, [(Int, Int)])]
Level1Sequences = [Level1Seq001, Level1Seq002, Level1Seq003]