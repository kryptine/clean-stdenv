implementation module ENEMY

import StdEnv, StdGameDef

EnemyBitmap :: GameBitmap
EnemyBitmap
  = { bitmapname  = "ENEMY.BMP"
    , unitsize    = { w = 24, h = 18 }
    , dimensions  = (13, 1)
    , transparent = Just { x = 215, y = 17 }
    }

EnemyMap :: [{#Int}]
EnemyMap = [{1,2,3,4,-1,-1},
  {5,6,7,8,9,-1},
  {-1,-1,-1,-1,-1,-1},
  {-1,-1,-1,-1,-1,-1},
  {-1,-1,-1,-1,-1,-1}]

EnemySeq001 :: (Int, [(Int, Int)])
EnemySeq001 = (-1,[])

EnemySequences :: [(Int, [(Int, Int)])]
EnemySequences = [EnemySeq001]