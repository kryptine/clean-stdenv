implementation module S1B

import StdEnv, StdGameDef

Static1bBitmap :: GameBitmap
Static1bBitmap
  = { bitmapname  = "S1B.BMP"
    , unitsize    = { w = 40, h = 24 }
    , dimensions  = (8, 2)
    , transparent = Just { x = 279, y = 47 }
    }

Static1bMap :: [{#Int}]
Static1bMap = [{0,0,0,0,0,0,0,0,0,0},
  {0,-2,0,0,0,0,-2,0,0,0},
  {0,0,-2,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0,0,0},
  {4,0,4,5,0,4,6,0,5,7},
  {9,10,9,9,11,9,9,10,9,9},
  {9,9,9,9,9,9,9,9,9,9},
  {9,9,9,9,9,9,9,9,9,9},
  {9,9,9,9,9,9,9,9,9,9},
  {9,9,9,9,9,9,9,9,9,9},
  {9,9,9,9,9,9,9,9,9,9},
  {9,9,9,9,9,9,9,9,9,9},
  {9,9,9,9,9,9,9,9,9,9}]

Static1bSeq001 :: (Int, [(Int, Int)])
Static1bSeq001 = (-1,[(1,36),(2,36)])
Static1bSeq002 :: (Int, [(Int, Int)])
Static1bSeq002 = (-2,[(3,38),(8,38)])
Static1bSeq003 :: (Int, [(Int, Int)])
Static1bSeq003 = (-3,[(12,48),(13,48)])
Static1bSeq004 :: (Int, [(Int, Int)])
Static1bSeq004 = (-4,[(14,48),(15,48)])

Static1bSequences :: [(Int, [(Int, Int)])]
Static1bSequences = [Static1bSeq001, Static1bSeq002, Static1bSeq003, Static1bSeq004]