definition module StdGame

import	StdInt, StdString
from	StdIOCommon	import ErrorReport,
							NoError, ErrorViolateDI, ErrorIdsInUse, ErrorUnknownObject, OtherError
from	StdPSt		import PSt, IOSt
import	StdGameDef
from	osgame		import GRESULT		// PA: this type should be shielded

/* predefined bounds */
BND_MAP_CODES      :==  (1 << 30)
BND_STATIC_BOUNDS  :==  (1 << 31)

/* skipmove constant */
SK_FOREVER  :==  (~1)


:: NoState
   = NoState

OpenGame :: gs (Game gs) [GameAttribute gs] !(PSt .l .p) -> (ErrorReport, !PSt .l .p)

CreateGameBitmap :: !GameBitmap !(GSt .gs) -> (!GRESULT, !GSt .gs)

CreateAnimation :: !Sprite !(GSt .gs) -> (!GRESULT, !GSt .gs)

CreateNewGameObject :: !ObjectType !SubType !Point2 !(GSt .gs) -> (!GRESULT, !GSt .gs)

:: ObjectFocus
   = { scrollleft      :: Int
     , scrollup        :: Int
     , scrollright     :: Int
     , scrolldown      :: Int
     , maxxscrollspeed :: Int
     , maxyscrollspeed :: Int
     }

instance zero ObjectFocus

CreateObjectFocus :: !ObjectFocus !(GSt .gs) -> (!GRESULT, !GSt .gs)

:: EventTarget
   = Self | AllObjects | BoundType Bounds

// modified 01/11/99
CreateUserGameEvent :: !EventType !EventPar !EventPar !EventTarget !SubType !GameTime !(GSt .gs) -> (!GRESULT, !GSt .gs)

// added 01/11/99
ANY_SUBTYPE :== (~1)

MAX_VOLUME :==  10000
MIN_VOLUME :==      0

:: Volume
   :== Int

PAN_LEFT   :== ~10000
PAN_CENTER :==      0
PAN_RIGHT  :==  10000

:: Pan
   :== Int

DEFAULT_FREQUENCY :== 0

:: Frequency
   :== Int

PlaySoundSample :: !SoundID !Volume !Pan !Frequency !GameTime !(GSt .gs) -> (!GRESULT, !GSt .gs)


GetBoundMap :: !Int !Int !(GSt .gs) -> (!GRESULT, !(!Int, !DirectionSet), !GSt .gs)
SetBoundMap :: !Int !Int (!Int, !DirectionSet) !(GSt .gs) -> (!GRESULT, !GSt .gs)


defaultInitObject :: .Size .a .Int .Point2 .GameTime !*(GSt .b) -> (!(.a,!.ObjectRec),!*GSt .b)
defaultGameObject :: .Int .Size b -> .Object *(GSt .c)
defaultObjectRec :: .Int .Point2 .Size .GameTime !*(GSt .a) -> (!.ObjectRec,!*GSt .a)

BlankScreen :: Level (GSt gs)

defaultShadow :: Int -> Shadow

defaultMovement :: Movement
defaultScrollMovement :: Int -> Movement

alignCentered :: Alignment
