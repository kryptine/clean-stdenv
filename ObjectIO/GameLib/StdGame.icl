implementation module StdGame

import	StdArray, StdBool, StdClass, StdInt, StdList, StdMisc
import	StdId
import	fixed, GameFunctions, gamehandle, gameutils, gst
from	gameobjectutils	import toBoundMapCode, fromBoundMapCode
from	StdPSt			import appPIO, accPIO
from	iostate			import setIOToolbox, getIOToolbox
import	windowcreate, windowdevice
from	windowaccess	import initWindowHandle
from	windowvalidate	import validateWindowId
from	oswindow		import OSNoWindowPtr


/* predefined bounds */
BND_MAP_CODES      :==  (1 << 30)
BND_STATIC_BOUNDS  :==  (1 << 31)

/* skipmove constant */
SK_FOREVER  :==  (~1)

:: NoState
   = NoState


OpenGame :: gs (Game gs) [GameAttribute gs] !(PSt .l .p) -> (ErrorReport, !(PSt .l .p))
OpenGame gs gdef attr ps
    #   (wId, ps)       =   accPIO openId ps
    #   size            =   findSize attr {w=320,h=240}
    #   bpp             =   findBPP attr 8
    #   (_, ps)         =   OpenGameWindow wId size bpp True ps
    #   (tb,ps)         =   accPIO getIOToolbox ps
    #   gst             =   toGSt gs tb
    #   (initLevel,gst) =   gdef.nextlevel gst
    #   (gs,tb)         =   fromGSt gst
    #   (_, tb)         =   PlayLevels initLevel gs gdef tb
    #   ps              =   appPIO (setIOToolbox tb) ps
    =   (NoError, ps)
where
    findSize :: [GameAttribute gs] Size -> Size
    findSize [] s = s
    findSize [(ScreenSize x):xs] s = x
    findSize [x:xs] s = findSize xs s
    findBPP :: [GameAttribute gs] Int -> Int
    findBPP [] s = s
    findBPP [(ColorDepth x):xs] s = x
    findBPP [x:xs] s = findBPP xs s

    // always full screen, game in a window not implemented yet
	OpenGameWindow :: !Id !Size !Int !Bool !(PSt .l .p) -> (!ErrorReport, !PSt .l .p)
	OpenGameWindow id gamewindowsize bitsperpixel fullscreen pState
		# pState				= WindowFunctions.dOpen pState
		# (isZero,pState)		= accPIO checkZeroWindowBound pState
		| isZero
			= (ErrorViolateDI,pState)
		# maybe_id				= Just id
		# (maybe_okId,ioState)	= validateWindowId maybe_id pState.io
		| isNothing maybe_okId
			= (ErrorIdsInUse,{pState & io=ioState})
		| otherwise
			# pState			= {pState & io=ioState}
			  info				= {	gamewindowDDPtr      = OSNoWindowPtr
			  					  ,	gamewindowCDepth     = bitsperpixel
			  					  ,	gamewindowSize       = gamewindowsize
			  					  ,	gamewindowFullScreen = fullscreen
			  					  }
			  okId				= fromJust maybe_okId
			# wH				= initWindowHandle "" Modeless IsGameWindow (GameWindowInfo info) [] [WindowId okId]
			# pState			= openwindow okId {wlsState=undef, wlsHandle=wH} pState
			# pState			= appPIO decreaseWindowBound pState
			= (NoError,pState)

PlayLevels :: Int gs (Game gs) !*OSToolbox -> (ErrorReport, !*OSToolbox)
PlayLevels level gs gdef tb
    |   level == 0
    =   (NoError, tb)
    #   ghnd             =  createGameHandle gdef
    #   (_, gs, tb)      =  PlayLevel level gs ghnd tb
    #   gst              =  toGSt gs tb
    #   (nextlevel, gst) =  gdef.nextlevel gst
    #   (gs,tb)          =  fromGSt gst
    =   PlayLevels nextlevel gs gdef tb

FindMaxID :: a [a] -> a  | < a
FindMaxID x [] = x
FindMaxID x [y:ys]
    |   y > x       =   FindMaxID y ys
    |   otherwise   =   FindMaxID x ys

InitLayers :: [Layer] [BID] [MAPID] !*OSToolbox -> ([BID], [MAPID], !*OSToolbox)
InitLayers [] bids mapids tb    =   (bids, mapids, tb)
InitLayers [l:ls] bids mapids tb
    #   (bids, mapids, tb)      =   InitLayer l bids mapids tb
    =   InitLayers ls bids mapids tb


MaybeSetTransparentColor :: BID (Maybe Point2) !*OSToolbox -> (GRESULT, !*OSToolbox)
MaybeSetTransparentColor _ Nothing tb = (GR_OK, tb)
MaybeSetTransparentColor bid (Just p) tb = SetTransparentColor bid p tb

MovementFunctions :: [Layer] -> [(Movement)]
MovementFunctions [] = []
MovementFunctions [l:ls] = [l.movement] ++ (MovementFunctions ls)

InitLayer :: Layer [BID] [MAPID] !*OSToolbox -> ([BID], [MAPID], !*OSToolbox)
InitLayer l bids mapids tb
    #   (newbid, tb)    =   InitGameBitmap 0 b.bitmapname (us.w * nh) (us.h * nv) us.w us.h tb  // newbid
    #   (_, tb)         =   MaybeSetTransparentColor newbid b.transparent tb
    #   tb              =   InitBlockSequences newbid l.sequences tb
    #   (_, tb)         =   InitGameLayerMap newmapid newbid l.layermap True tb     // l.tile
    =   (bids++[newbid], mapids++[newmapid], tb)
where
    b           =   l.bmp
    us          =   b.unitsize
    (nh, nv)    =   b.dimensions
    newmapid    =   ((FindMaxID 0 mapids) + 1)

InitBlockSequences :: BID [TileSequence] !*OSToolbox -> !*OSToolbox
InitBlockSequences bid [] tb = tb
InitBlockSequences bid [s:ss] tb
    #   (_, tb) =   InitBlockSequence bid s tb
    =   InitBlockSequences bid ss tb

LayersDone :: [BID] [MAPID] !*OSToolbox -> !*OSToolbox
LayersDone bids mapids tb
    #   tb      =   MapsDone mapids tb
    #   tb      =   BitmapsDone bids tb
    =   tb

MapsDone :: [MAPID] !*OSToolbox -> !*OSToolbox
MapsDone [] tb = tb
MapsDone [m:ms] tb
    #   (_, tb)     =   GameLayerMapDone m tb
    =   MapsDone ms tb

BitmapsDone :: [BID] !*OSToolbox -> !*OSToolbox
BitmapsDone [] tb = tb
BitmapsDone [b:bs] tb
    #   (_, tb)     =   GameBitmapDone b tb
    =   BitmapsDone bs tb

PlayLevel :: Int gs (GameHandle gs) !*OSToolbox -> (ErrorReport, gs,!*OSToolbox)
PlayLevel levelnumber gs gamehnd tb
    #   (_, tb)             =   SetGameBoundMap wid ht bm os stx sty  tb
    #   (_, tb)             =   MoveScreenTo curLevelHnd.initpos` tb
    #   lyrs                =   curLevelHnd.layers`
    #   (bids, mapids, tb)  =   InitLayers lyrs [] [] tb
    #   movements           =   zip2 mapids (MovementFunctions lyrs)
    #   (_, tb)             =   initsoundsamples curLevelHnd.soundsamples` tb
    #   tb                  =   maybePlayMusic curLevelHnd.music` tb
    #   gst                 =   toGSt gs tb
    #   firstlevel          =   curLevelHnd
    #   (obj, gst)          =   convertallobjsprites firstlevel.objects` gst
    #   firstlevel          =   {firstlevel & objects` = obj}
    #   curgamehnd          =   {gamehnd & levels` = [firstlevel]}
    #   (gs,tb)             =   fromGSt gst
    #   (_, tb)             =   OSGameLevelOptions fill rgb esc dbg fdin fdout tb
    #   (gs, tb)            =   RunGameEngine { scroll    = movements
                                              , gamestate = gs
                                              , gamehnd   = curgamehnd} tb
    #   tb                  =   maybeStopMusic curLevelHnd.music` tb
    #   (_, tb)             =   OSInitSoundSample (~1) "" 0 tb  // remove samples
    #   tb                  =   LayersDone bids mapids tb
    #   (_, tb)             =   ClearAllGameBitmaps tb
    =   (NoError, gs, tb)
where
    curLevelHnd         =   gamehnd.levels`!!(levelnumber-1)
    options             =   curLevelHnd.leveloptions`
    { map = bm,
      blocksize = bs,
      objstart = os,
      startobjx = stx,
      startobjy = sty}  =   curLevelHnd.boundmap`
    { w = wid, h = ht } =   bs
    maybePlayMusic :: (Maybe Music) !*OSToolbox -> !*OSToolbox
    maybePlayMusic Nothing tb = tb
    maybePlayMusic (Just m) tb
        #   (_, tb)     =   PlayMusic m.musicfile m.restart tb
        = tb
    maybeStopMusic :: (Maybe Music) !*OSToolbox -> !*OSToolbox
    maybeStopMusic Nothing tb = tb
    maybeStopMusic (Just m) tb
        |   m.continue
            = tb
        #   (_, tb)     =   StopMusic tb
        = tb
    esc = options.escquit
    dbg = options.debugscroll
    fdin = options.fadein
    fdout = options.fadeout
    rgb :: Colour
    rgb = if fill
             (fromJust options.fillbackground)
             (RGB {r= ~1, g= ~1, b= ~1})
    fill = (isJust options.fillbackground)

initsoundsamples sndlist gs
    = map2 initsoundsample sndlist gs

initsoundsample sample gs
    = OSInitSoundSample sample.soundid sample.soundfile sample.soundbuffers gs

convertallobjsprites obj gst = map2 convertobjsprites obj gst
where
    convertobjsprites obj gst
        # (sprids, gst) = convertsprites obj.sprites` gst
        = ({obj & spriteids` = sprids}, gst)

//convertsprites :: ![Sprite] !.(GSt .gs) -> (![SpriteID], !.(GSt .gs))
convertsprites spr gst
    # (idlst, gst) = map2 CreateAnimation spr gst
    # idlst        = map (~) idlst
    = (idlst, gst)


CreateGameBitmap :: !GameBitmap !(GSt .gs) -> (!GRESULT, !GSt .gs)
CreateGameBitmap bitmap=:{bitmapname, unitsize, dimensions, transparent} gst
    #   (bid, gst) = accGStTb (InitGameBitmap 0 bitmapname (w * nh) (h * nv) w h) gst
    #   (_, gst)   = accGStTb (MaybeSetTransparentColor bid transparent) gst
    =   (bid, gst)
where
    w       = unitsize.w
    h       = unitsize.h
    (nh,nv) = dimensions


CreateAnimation :: !Sprite !(GSt .gs) -> (!GRESULT, !GSt .gs)
CreateAnimation sprite=:{bitmap, sequence, loop} gst
    #   (bid, gst)   = CreateGameBitmap bitmap gst
    #   (sprid, gst) = accGStTb (InitSpriteAnimation bid sequence loop) gst
    = (~sprid, gst)

CreateNewGameObject :: !ObjectType !SubType !Point2 !(GSt .gs) -> (!GRESULT, !GSt .gs)
CreateNewGameObject ot st p gst
    =   accGStTb (InitGameObject ot st p) gst

:: ObjectFocus
   = { scrollleft      :: Int
     , scrollup        :: Int
     , scrollright     :: Int
     , scrolldown      :: Int
     , maxxscrollspeed :: Int
     , maxyscrollspeed :: Int
     }

instance zero ObjectFocus
where
    zero = { scrollleft      = 0
           , scrollup        = 0
           , scrollright     = 0
           , scrolldown      = 0
           , maxxscrollspeed = 0
           , maxyscrollspeed = 0
           }

CreateObjectFocus :: !ObjectFocus !(GSt .gs) -> (!GRESULT, !GSt .gs)
CreateObjectFocus o gst
    =   accGStTb (SetObjectFocus o.scrollleft o.scrollup o.scrollright
                      o.scrolldown o.maxxscrollspeed o.maxyscrollspeed) gst


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
PlaySoundSample id vol pan freq delay gst
    =   accGStTb (OSPlaySoundSample id (vol - 10000) pan freq delay) gst

:: EventTarget
   = Self | AllObjects | BoundType Bounds

EventTargetToInt :: EventTarget -> Int
EventTargetToInt Self = 0
EventTargetToInt AllObjects = -1
EventTargetToInt (BoundType b) = b

// modified 01/11/99
CreateUserGameEvent :: !EventType !EventPar !EventPar !EventTarget !SubType !GameTime !(GSt .gs) -> (!GRESULT, !GSt .gs)
CreateUserGameEvent ev evpar1 evpar2 dest subdest time gst
    =   accGStTb (CreateUserEvent ev evpar1 evpar2 (EventTargetToInt dest) subdest time) gst

// added 01/11/99
ANY_SUBTYPE :== (~1)

GetBoundMap :: !Int !Int !(GSt .gs) -> (!GRESULT, !(!Int, !DirectionSet), !GSt .gs)
GetBoundMap x y gst
    #   (result, gst) =   accGStTb (OSGetBoundMap x y) gst
    #   (gr, val)     =   result
    =   (gr, fromBoundMapCode val, gst)
    

SetBoundMap :: !Int !Int (!Int, !DirectionSet) !(GSt .gs) -> (!GRESULT, !GSt .gs)
SetBoundMap x y newvalue gst
    =   accGStTb (OSSetBoundMap x y (toBoundMapCode newvalue)) gst



defaultObjectOptions :: ObjectOptions
defaultObjectOptions
 = { // fixed               = False
  // , ignorestaticbounds  = False
    ignorelevelbounds   = False
  // , bounceatcollisions  = False
  // , checkmapcodes       = False
   , checkkeyboard       = False
   , allowkeyboardrepeat = False
   , static              = False
   , hdirection          = DirRight
   , vdirection          = DirDown
   , automirrorleftright = False
   , automirrorupdown    = False
   , freeze              = False
   , removemapcode       = False
   }

defaultObjectRec :: .Int .Point2 .Size .GameTime !*(GSt .a) -> (!.ObjectRec,!*GSt .a)
defaultObjectRec objsubtype position size time gs
  = ( { active          = True
      , subtype         = objsubtype
      , size            = size
      , pos             = position
      , offset          = {x = 0, y = 0}
      , currentsprite   = 1
      , displayoptions  = { blink           = False
                          , stretch         = False
                          , mirrorleftright = False
                          , mirrorupdown    = False
                          , rotation        = NoRotation }
      , ownbounds       = 0
      , bouncebounds    = 0
      , collidebounds   = 0
      , forgetdistance  = {x = 1, y = 1}
      , framecounter    = time
      , layer           = InFront
      , acceleration    = zero
      , speed           = zero
      , bounce          = {fvx = Value 0.0, fvy = Value 0.0}
      , maxspeed        = {rx = (fxr EVERYTHING), ry = (fxr EVERYTHING)}
      , slowdown        = {fvx = Value 0.0, fvy = Value 0.0}
      , skipmove        = SK_FOREVER
      , options         = defaultObjectOptions
      }
    , gs)

defaultInitObject :: .Size .a .Int .Point2 .GameTime !*(GSt .b) -> (!(.a,!.ObjectRec),!*GSt .b)
defaultInitObject size state subtype pos time gs
    # (newobjrec, gs) = defaultObjectRec subtype pos size time gs
    = ((state, newobjrec), gs)

defaultGameObject :: .Int .Size b -> .Object *(GSt .c);
defaultGameObject objtype size state
  = { objecttype = objtype
    , sprites    = []
    , init       = (defaultInitObject size state)
   // , done       = \(state, r=:{subtype}) gs -> ((objtype, subtype), gs)
    , done       = \(state, r) gs            -> gs
    , move       = \(state, r) gs            -> ((state, r), gs)
    , animation  = \(state, r) gs            -> ((state, r), gs)
    , touchbound = \(state, r) _ _ gs        -> ((state, r), gs)
    , collide    = \(state, r) _ _ _ gs      -> ((state, r), gs)
    , frametimer = \(state, r) gs            -> ((state, r), gs)
    , keydown    = \(state, r) a gs          -> ((state, r), gs)
    , keyup      = \(state, r) a gs          -> ((state, r), gs)
    , userevent  = \(state, r) a b c gs      -> ((state, r), gs)
    }


BlankScreen :: Level (GSt gs)
BlankScreen
  = { boundmap     = { map = [{0}]
                     , blocksize = {w = 10000, h = 10000}
                     , objstart  = 1
                     , startobjx = 1
                     , startobjy = 1
                     }
    , initpos      = {x = 0, y = 0}
    , layers       = []
    , objects      = []
    , music        = Nothing
    , soundsamples = []
    , leveloptions = { fillbackground = Just Black
                     , escquit        = True
                     , debugscroll    = False
                     , fadein         = False
                     , fadeout        = False
                     }
    }


defaultShadow :: Int -> Shadow
defaultShadow n =
    { shadowpos   = {x = n, y = n}
    , shadowcolor = Black
    }

defaultMovement :: Movement
defaultMovement = \p t -> p

defaultScrollMovement :: Int -> Movement
defaultScrollMovement n = \p t -> {x = p.x / n, y = p.y / n}


alignCentered :: Alignment
alignCentered = { xyfromscreencenter = (True, True)
                , xycentered = (True, True)
                }

