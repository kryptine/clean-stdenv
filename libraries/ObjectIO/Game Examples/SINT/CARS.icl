implementation module CARS

import StdEnv, StdGameDef

CarsBitmap :: GameBitmap
CarsBitmap
  = { bitmapname  = "CARS.BMP"
    , unitsize    = { w = 80, h = 50 }
    , dimensions  = (4, 1)
    , transparent = Just { x = 159, y = 49 }
    }

CarsMap :: [{#Int}]
CarsMap = [{1,2,-1,-1,-1,-1,-1},
  {-1,-1,-1,-1,-1,-1,-1},
  {-1,-1,-1,-1,-1,-1,-1},
  {-1,-1,-1,-1,-1,-1,-1},
  {-1,-1,-1,-1,-1,-1,-1},
  {-1,-1,-1,-1,-1,-1,-1}]

CarsSeq001 :: (Int, [(Int, Int)])
CarsSeq001 = (-1,[])

CarsSequences :: [(Int, [(Int, Int)])]
CarsSequences = [CarsSeq001]