
definition module SintGfx

import StdEnv, StdGameDef

OBJ_START :== 0x10

L1Bitmap :: GameBitmap

L1LevelMap :: [{#Int}]
L1LevelBoundMapData :: [{#Int}]
L1LevelBoundMap :: BoundMap
L1LevelWideBoundMap :: BoundMap
L1BonusMap :: [{#Int}]
L1BonusBoundMapData :: [{#Int}]
L1BonusBoundMap :: BoundMap
L1BonusWideBoundMap :: BoundMap
L1BackGrMap :: [{#Int}]
L1BackGrBoundMapData :: [{#Int}]
L1BackGrBoundMap :: BoundMap
L1BackGrWideBoundMap :: BoundMap
L1InFrontMap :: [{#Int}]
L1InFrontBoundMapData :: [{#Int}]
L1InFrontBoundMap :: BoundMap
L1InFrontWideBoundMap :: BoundMap
L1BonusBackGrMap :: [{#Int}]
L1BonusBackGrBoundMapData :: [{#Int}]
L1BonusBackGrBoundMap :: BoundMap
L1BonusBackGrWideBoundMap :: BoundMap

L1Seq1 :: (Int, [(Int, Int)])
L1Seq2 :: (Int, [(Int, Int)])
L1Seq3 :: (Int, [(Int, Int)])
L1Seq4 :: (Int, [(Int, Int)])
L1Seq5 :: (Int, [(Int, Int)])
L1Seq6 :: (Int, [(Int, Int)])
L1Seq7 :: (Int, [(Int, Int)])

L1Sequences :: [(Int, [(Int, Int)])]

L1LevelLayer :: Layer
L1BonusLayer :: Layer
L1BackGrLayer :: Layer
L1InFrontLayer :: Layer
L1BonusBackGrLayer :: Layer

LevelSprite8 :: Sprite
LevelAnimation8 :: Sprite
InFrontSprite1 :: Sprite
InFrontAnimation1 :: Sprite
BonusSprite10 :: Sprite
BonusAnimation10 :: Sprite
L1Sprite3 :: Sprite
L1Animation3 :: Sprite
InFrontSprite2 :: Sprite
InFrontAnimation2 :: Sprite
InFrontSprite3 :: Sprite
InFrontAnimation3 :: Sprite
InFrontSprite4 :: Sprite
InFrontAnimation4 :: Sprite
L1_40x24Bitmap :: GameBitmap

L1_40x24Sky1Map :: [{#Int}]
L1_40x24Sky1BoundMapData :: [{#Int}]
L1_40x24Sky1BoundMap :: BoundMap
L1_40x24Sky1WideBoundMap :: BoundMap
L1_40x24BonusSkyMap :: [{#Int}]
L1_40x24BonusSkyBoundMapData :: [{#Int}]
L1_40x24BonusSkyBoundMap :: BoundMap
L1_40x24BonusSkyWideBoundMap :: BoundMap

L1_40x24Seq1 :: (Int, [(Int, Int)])
L1_40x24Seq2 :: (Int, [(Int, Int)])
L1_40x24Seq3 :: (Int, [(Int, Int)])
L1_40x24Seq4 :: (Int, [(Int, Int)])
L1_40x24Seq5 :: (Int, [(Int, Int)])
L1_40x24Seq6 :: (Int, [(Int, Int)])
L1_40x24Seq7 :: (Int, [(Int, Int)])
L1_40x24Seq8 :: (Int, [(Int, Int)])

L1_40x24Sequences :: [(Int, [(Int, Int)])]

L1_40x24Sky1Layer :: Layer
L1_40x24BonusSkyLayer :: Layer

Sky1Sprite2 :: Sprite
Sky1Animation2 :: Sprite
Sky1Sprite4 :: Sprite
Sky1Animation4 :: Sprite
Sky1Sprite1 :: Sprite
Sky1Animation1 :: Sprite
Sky1Sprite3 :: Sprite
Sky1Animation3 :: Sprite
L1_40x24Sprite4 :: Sprite
L1_40x24Animation4 :: Sprite
BonusSkySprite3 :: Sprite
BonusSkyAnimation3 :: Sprite
L1_40x24Sprite6 :: Sprite
L1_40x24Animation6 :: Sprite
L1_40x24Sprite7 :: Sprite
L1_40x24Animation7 :: Sprite
L1_32x32Bitmap :: GameBitmap

L1_32x32BoatsMap :: [{#Int}]
L1_32x32BoatsBoundMapData :: [{#Int}]
L1_32x32BoatsBoundMap :: BoundMap
L1_32x32BoatsWideBoundMap :: BoundMap


L1_32x32Sequences :: [(Int, [(Int, Int)])]

L1_32x32BoatsLayer :: Layer

L2Bitmap :: GameBitmap

L2LevelMap :: [{#Int}]
L2LevelBoundMapData :: [{#Int}]
L2LevelBoundMap :: BoundMap
L2LevelWideBoundMap :: BoundMap
L2BonusMap :: [{#Int}]
L2BonusBoundMapData :: [{#Int}]
L2BonusBoundMap :: BoundMap
L2BonusWideBoundMap :: BoundMap
L2BackGrMap :: [{#Int}]
L2BackGrBoundMapData :: [{#Int}]
L2BackGrBoundMap :: BoundMap
L2BackGrWideBoundMap :: BoundMap

L2Seq1 :: (Int, [(Int, Int)])
L2Seq2 :: (Int, [(Int, Int)])
L2Seq3 :: (Int, [(Int, Int)])
L2Seq4 :: (Int, [(Int, Int)])

L2Sequences :: [(Int, [(Int, Int)])]

L2LevelLayer :: Layer
L2BonusLayer :: Layer
L2BackGrLayer :: Layer

LevelSprite6 :: Sprite
LevelAnimation6 :: Sprite
LevelSprite4 :: Sprite
LevelAnimation4 :: Sprite
BonusSprite5 :: Sprite
BonusAnimation5 :: Sprite
L2Sprite3 :: Sprite
L2Animation3 :: Sprite
L2_40x24Bitmap :: GameBitmap

L2_40x24Sky2Map :: [{#Int}]
L2_40x24Sky2BoundMapData :: [{#Int}]
L2_40x24Sky2BoundMap :: BoundMap
L2_40x24Sky2WideBoundMap :: BoundMap

L2_40x24Seq1 :: (Int, [(Int, Int)])
L2_40x24Seq2 :: (Int, [(Int, Int)])
L2_40x24Seq3 :: (Int, [(Int, Int)])
L2_40x24Seq4 :: (Int, [(Int, Int)])

L2_40x24Sequences :: [(Int, [(Int, Int)])]

L2_40x24Sky2Layer :: Layer

Sky2Sprite5 :: Sprite
Sky2Animation5 :: Sprite
L2_40x24Sprite1 :: Sprite
L2_40x24Animation1 :: Sprite
Sky2Sprite2 :: Sprite
Sky2Animation2 :: Sprite
Sky2Sprite4 :: Sprite
Sky2Animation4 :: Sprite
MainCharBitmap :: GameBitmap

MainCharSintMap :: [{#Int}]
MainCharSintBoundMapData :: [{#Int}]
MainCharSintBoundMap :: BoundMap
MainCharSintWideBoundMap :: BoundMap

MainCharSeq1 :: (Int, [(Int, Int)])
MainCharSeq2 :: (Int, [(Int, Int)])
MainCharSeq3 :: (Int, [(Int, Int)])
MainCharSeq4 :: (Int, [(Int, Int)])
MainCharSeq5 :: (Int, [(Int, Int)])
MainCharSeq6 :: (Int, [(Int, Int)])
MainCharSeq7 :: (Int, [(Int, Int)])

MainCharSequences :: [(Int, [(Int, Int)])]

MainCharSintLayer :: Layer

SintSprite1 :: Sprite
SintAnimation1 :: Sprite
SintSprite2 :: Sprite
SintAnimation2 :: Sprite
SintSprite3 :: Sprite
SintAnimation3 :: Sprite
SintSprite4 :: Sprite
SintAnimation4 :: Sprite
SintSprite5 :: Sprite
SintAnimation5 :: Sprite
SintSprite6 :: Sprite
SintAnimation6 :: Sprite
SintSprite7 :: Sprite
SintAnimation7 :: Sprite
ItemsBitmap :: GameBitmap

ItemsItemMap :: [{#Int}]
ItemsItemBoundMapData :: [{#Int}]
ItemsItemBoundMap :: BoundMap
ItemsItemWideBoundMap :: BoundMap

ItemsSeq1 :: (Int, [(Int, Int)])
ItemsSeq2 :: (Int, [(Int, Int)])
ItemsSeq3 :: (Int, [(Int, Int)])
ItemsSeq4 :: (Int, [(Int, Int)])
ItemsSeq5 :: (Int, [(Int, Int)])
ItemsSeq6 :: (Int, [(Int, Int)])
ItemsSeq7 :: (Int, [(Int, Int)])

ItemsSequences :: [(Int, [(Int, Int)])]

ItemsItemLayer :: Layer

ItemSprite1 :: Sprite
ItemAnimation1 :: Sprite
ItemSprite2 :: Sprite
ItemAnimation2 :: Sprite
ItemSprite3 :: Sprite
ItemAnimation3 :: Sprite
ItemSprite4 :: Sprite
ItemAnimation4 :: Sprite
ItemSprite5 :: Sprite
ItemAnimation5 :: Sprite
ItemSprite6 :: Sprite
ItemAnimation6 :: Sprite
ItemSprite7 :: Sprite
ItemAnimation7 :: Sprite
PepernotenBitmap :: GameBitmap

PepernotenPepernootMap :: [{#Int}]
PepernotenPepernootBoundMapData :: [{#Int}]
PepernotenPepernootBoundMap :: BoundMap
PepernotenPepernootWideBoundMap :: BoundMap

PepernotenSeq1 :: (Int, [(Int, Int)])

PepernotenSequences :: [(Int, [(Int, Int)])]

PepernotenPepernootLayer :: Layer

PepernootSprite1 :: Sprite
PepernootAnimation1 :: Sprite
StatusBitmap :: GameBitmap

StatusStatusMap :: [{#Int}]
StatusStatusBoundMapData :: [{#Int}]
StatusStatusBoundMap :: BoundMap
StatusStatusWideBoundMap :: BoundMap

StatusSeq1 :: (Int, [(Int, Int)])
StatusSeq2 :: (Int, [(Int, Int)])
StatusSeq3 :: (Int, [(Int, Int)])
StatusSeq4 :: (Int, [(Int, Int)])
StatusSeq5 :: (Int, [(Int, Int)])
StatusSeq6 :: (Int, [(Int, Int)])

StatusSequences :: [(Int, [(Int, Int)])]

StatusStatusLayer :: Layer

StatusSprite1 :: Sprite
StatusAnimation1 :: Sprite
StatusSprite2 :: Sprite
StatusAnimation2 :: Sprite
StatusSprite3 :: Sprite
StatusAnimation3 :: Sprite
StatusSprite4 :: Sprite
StatusAnimation4 :: Sprite
StatusSprite5 :: Sprite
StatusAnimation5 :: Sprite
StatusSprite6 :: Sprite
StatusAnimation6 :: Sprite
SunBitmap :: GameBitmap

SunSunMap :: [{#Int}]
SunSunBoundMapData :: [{#Int}]
SunSunBoundMap :: BoundMap
SunSunWideBoundMap :: BoundMap

SunSeq1 :: (Int, [(Int, Int)])

SunSequences :: [(Int, [(Int, Int)])]

SunSunLayer :: Layer

SunSprite1 :: Sprite
SunAnimation1 :: Sprite
FadeBitmap :: GameBitmap

FadeMap1Map :: [{#Int}]
FadeMap1BoundMapData :: [{#Int}]
FadeMap1BoundMap :: BoundMap
FadeMap1WideBoundMap :: BoundMap


FadeSequences :: [(Int, [(Int, Int)])]

FadeMap1Layer :: Layer

AutoBitmap :: GameBitmap

AutoCarMap :: [{#Int}]
AutoCarBoundMapData :: [{#Int}]
AutoCarBoundMap :: BoundMap
AutoCarWideBoundMap :: BoundMap

AutoSeq1 :: (Int, [(Int, Int)])

AutoSequences :: [(Int, [(Int, Int)])]

AutoCarLayer :: Layer

CarSprite1 :: Sprite
CarAnimation1 :: Sprite
