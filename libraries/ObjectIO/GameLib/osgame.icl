implementation module osgame

//      Version 1.0

import	StdArray, StdBool, StdChar, StdInt
import	StdMaybe
from	ospicture	import toRGBtriple
from	clCCall_12	import WinBeep
import	gameCrossCall_12, gamehandle, gameobjectutils, gst

::  OSGameData gs
    =   {   scroll    :: [(MAPID, Movement)]        // The layer movement functions
        ,   gamestate :: gs                         // The game state
        ,   gamehnd   :: GameHandle gs              // Complete game definition
        }

OSBinaryIntStr :: !Int -> {#Char}
OSBinaryIntStr x = WinBinaryIntStr x

OSBinaryBoolStr :: !Bool -> {#Char}
OSBinaryBoolStr x = WinBinaryBoolStr x

OSIntListArrayToString :: ![{#Int}] -> {#Char}
OSIntListArrayToString irs
    = IntListArrayToString "" irs
where
    IntListArrayToString :: !{#Char} ![{#Int}] -> !{#Char}
    IntListArrayToString cs [] = cs
    IntListArrayToString cs [ir:irs] = IntListArrayToString (cs+++intarrayToString ir) irs
    where
        intarrayToString :: !{#Int} -> {#Char}
        intarrayToString ir
            = encode 0 ir zeroChars
        where
            zeroChars   = createArray (4*(size ir)) zero
            encode :: !Int !{#Int} !*{#Char} -> *{#Char}
            encode i ir zeroChars
                | i<size ir
                    = encode (i+1) ir {zeroChars & [j]=toChar (maskshift 0 x),[j+1]=toChar (maskshift 8 x),[j+2]=toChar (maskshift 16 x),[j+3]=toChar (maskshift 24 x)}
                | otherwise
                    = zeroChars
            where
                x   = ir.[i]
                j   = 4*i
                maskshift :: !Int !Int -> Int
                maskshift nrbits x = (x>>nrbits) bitand 0xFF

OSInitGameBitmap :: !BID !{#Char} !Int !Int !Int !Int !*OSToolbox -> (!GRESULT,!*OSToolbox)
OSInitGameBitmap id filename w h blockwidth blockheight tb
    = WinInitGameBitmap id filename w h blockwidth blockheight tb

OSGameBitmapDone :: !BID !*OSToolbox -> (!GRESULT,!*OSToolbox)
OSGameBitmapDone id tb
    = WinGameBitmapDone id tb

OSClearAllGameBitmaps :: !*OSToolbox -> (!GRESULT,!*OSToolbox)
OSClearAllGameBitmaps tb
    = WinClearAllGameBitmaps tb

OSSetTransparentColor :: !BID !Int !Int !*OSToolbox -> (!GRESULT,!*OSToolbox)
OSSetTransparentColor id x y tb
    = WinSetTransparentColor id x y tb

OSInitBlockSequence :: !BID !SEQID !{#Char} !*OSToolbox -> (!GRESULT,!*OSToolbox)
OSInitBlockSequence bid seqid seq tb
    = WinInitBlockSequence bid seqid seq tb

OSInitGameLayerMap :: !MAPID !BID !{#Char} !Int !Int !Bool !*OSToolbox -> (!GRESULT,!*OSToolbox)
OSInitGameLayerMap mapid bid map w h tile tb
    = WinInitGameLayerMap mapid bid map w h tile tb

OSGameLayerMapDone :: !MAPID !*OSToolbox -> (!GRESULT,!*OSToolbox)
OSGameLayerMapDone mapid tb
    = WinGameLayerMapDone mapid tb

OSRunGameEngine :: !(OSGameData gs) !*OSToolbox -> (gs,!*OSToolbox)
OSRunGameEngine gd tb
    # (gd,tb)   = WinRunGameEngine handleGameEvents gd 0 0 0 tb
    = (gd.gamestate,tb)

OSSetGameBoundMap :: !Int !Int !{#Char} !Int !Int !Int !Int !Int !*OSToolbox -> (!GRESULT,!*OSToolbox)
OSSetGameBoundMap w h map mw mh objstart startobjx startobjy tb
    = WinSetGameBoundMap w h map mw mh objstart startobjx startobjy tb

OSMoveScreenTo :: !Int !Int !*OSToolbox -> (!GRESULT, !*OSToolbox)
OSMoveScreenTo x y tb
    = WinMoveScreenTo x y tb

OSInitSpriteAnimation :: !BID !{#Char} !Bool !*OSToolbox -> (!GRESULT,!*OSToolbox)
OSInitSpriteAnimation bid seq loop tb
    = WinInitSpriteAnimation bid seq loop tb

OSInitGameObject :: !ObjectType !SubType !Point2 !*OSToolbox -> (!GRESULT, !*OSToolbox)
OSInitGameObject ot st p tb
    = WinInitGameObject ot st p.x p.y tb

OSSetObjectFocus :: !Int !Int !Int !Int !Int !Int !*OSToolbox -> (!GRESULT, !*OSToolbox)
OSSetObjectFocus x1 y1 x2 y2 maxxv maxyv tb
    = WinSetObjectFocus x1 y1 x2 y2 maxxv maxyv tb

// modified 01/11/99
OSCreateUserEvent :: !Int !Int !Int !Int !Int !Int !*OSToolbox -> (!GRESULT, !*OSToolbox)
OSCreateUserEvent ev evpar1 evpar2 dest subdest time tb
    = WinCreateUserEvent ev evpar1 evpar2 dest subdest time tb

OSShowStatistic :: !Int !Int !{#Char} !Int !Colour !{#Char} !Int !Bool !Bool  !Bool !Int !Int !Colour !Int !*OSToolbox -> (!GRESULT, !*OSToolbox)
OSShowStatistic x y format value color font size bold italic shadow sx sy scolor options tb
    = WinShowStatistic x y format value colorRGB font size bold italic shadow sx sy scolorRGB options tb
where
    colorRGB  = toRGBtriple color
    scolorRGB = toRGBtriple scolor

OSPlayMusic :: !{#Char} !Bool !*OSToolbox -> (!GRESULT, !*OSToolbox)
OSPlayMusic midifile restart tb
    = WinPlayMusic midifile restart tb

OSStopMusic :: !*OSToolbox -> (!GRESULT, !*OSToolbox)
OSStopMusic tb
    = WinStopMusic tb


OSGameLevelOptions :: !Bool !Colour !Bool !Bool !Bool !Bool !*OSToolbox -> (!GRESULT, !*OSToolbox)
OSGameLevelOptions fill rgb esc debug fadein fadeout tb
    = WinGameLevelOptions (if fill (toRGBtriple rgb) (~1,~1,~1)) esc debug fadein fadeout tb

OSInitSoundSample :: !Int !{#Char} !Int !*OSToolbox -> (!GRESULT, !*OSToolbox)
OSInitSoundSample id name buffers tb
    = WinInitSoundSample id name buffers tb

OSPlaySoundSample :: !Int !Int !Int !Int !Int !*OSToolbox -> (!GRESULT, !*OSToolbox)
OSPlaySoundSample id vol pan freq delay tb
    = WinPlaySoundSample id vol pan freq delay tb

OSGetBoundMap :: !Int !Int !*OSToolbox -> (!(!Int, !GRESULT), !*OSToolbox)
OSGetBoundMap x y tb
    # (value, gresult, tb) = WinGetBoundMap x y tb
    = ((value, gresult), tb)

OSSetBoundMap :: !Int !Int !Int !*OSToolbox -> (!GRESULT, !*OSToolbox)
OSSetBoundMap x y newvalue tb
    = WinSetBoundMap x y newvalue tb

handleGameEvents :: !CrossCallInfo !(OSGameData gs) !*OSToolbox -> (!CrossCallInfo,!OSGameData gs,!*OSToolbox)
handleGameEvents fromOSCci=:{ccMsg=CcWmGAMEKEYBOARD,p1=key,p2=x,p3=y} state tb
    = (Return2Cci x` y`,state,tb)
where
    (x`,y`) = case key of
                GK_LEFT     -> (x-1, y)
                GK_RIGHT    -> (x+1, y)
                GK_UP       -> (x, y-1)
                GK_DOWN     -> (x, y+1)
                otherwise   -> (x, y)

handleGameEvents fromOSCci=:{ccMsg=CcWmCHECKQUIT} state tb
    #   quitfunction        =   state.gamehnd.quitlevel`
    #   gst                 =   toGSt state.gamestate tb
    #   (quit, gst)         =   quitfunction gst
    #   (newstate, tb)      =   fromGSt gst
    =   (Return1Cci (toInt quit), {state & gamestate = newstate}, tb)

handleGameEvents fromOSCci=:{ccMsg=CcWmSCROLL,p1=id,p2=x,p3=y,p4=t} state tb
    = (Return2Cci x` y`, state, tb)
where
    {x = x`, y = y`}    =   f (MakePoint x y) t
    f                   =   FindMovement (id, state.scroll)
	
	FindMovement :: !(!MAPID, ![(MAPID, Movement)]) -> Movement
	FindMovement (mapid, [(id, mv): rest])
	    | id == mapid   =   mv
	    | otherwise     =   FindMovement (mapid, rest)

handleGameEvents fromOSCci=:{ccMsg=CcWmINITOBJECT, p1=objtype, p2=subtype, p3=id, p4=x, p5=y, p6=time} state tb
    # (state,tb)        =   initialiseGameObject objtype subtype id {x=x,y=y} time state tb
    = (Return0Cci, state, tb)

handleGameEvents fromOSCci=:{ccMsg=CcWmOBJECTDONE, p1=objtype, p2=id} state tb
    # (state, tb) = doneGameObject objtype id state tb
    = (Return0Cci, state, tb)

handleGameEvents fromOSCci=:{ccMsg=CcWmMOVEOBJECT, p1=objtype, p2=id} state tb
    # (state,tb)        =   moveGameObject objtype id state tb
    = (Return0Cci, state, tb)

handleGameEvents fromOSCci=:{ccMsg=CcWmTOUCHBOUND, p1=objtype, p2=id, p3=dir, p4=mapcode} state tb
    # (state,tb)        =   touchBound objtype id dir mapcode state tb
    = (Return0Cci, state, tb)

handleGameEvents fromOSCci=:{ccMsg=CcWmCOLLISION, p1=ot1, p2=id1, p3=ot2, p4=id2, p5=dir} state tb
    # (state,tb)        =   handleCollision ot1 id1 ot2 id2 dir state tb
    = (Return0Cci, state, tb)

handleGameEvents fromOSCci=:{ccMsg=CcWmANIMATION, p1=objtype, p2=id} state tb
    # (state,tb)        =   handleAnimationEvent objtype id state tb
    = (Return0Cci, state, tb)

handleGameEvents fromOSCci=:{ccMsg=CcWmUSEREVENT, p1=objtype, p2=id, p3=ev, p4=par1, p5=par2} state tb
    # (state,tb)        =   handleUserEvent objtype id ev par1 par2 state tb
    = (Return0Cci, state, tb)

handleGameEvents fromOSCci=:{ccMsg=CcWmOBJECTTIMER, p1=objtype, p2=id} state tb
    # (state,tb)        =   handleTimerEvent objtype id state tb
    = (Return0Cci, state, tb)

handleGameEvents fromOSCci=:{ccMsg=CcWmOBJECTKEYDOWN, p1=objtype, p2=id, p3=key} state tb
    # (state,tb)        =   handleKeyDown objtype id key state tb
    = (Return0Cci, state, tb)

handleGameEvents fromOSCci=:{ccMsg=CcWmOBJECTKEYUP, p1=objtype, p2=id, p3=key} state tb
    # (state,tb)        =   handleKeyUp objtype id key state tb
    = (Return0Cci, state, tb)

handleGameEvents fromOSCci=:{ccMsg=CcWmSTATISTICS} state tb
    #   gst             =   toGSt state.gamestate tb
    #   (statlist, gst) =   state.gamehnd.statistics` gst
    #   (newstate, tb)  =   fromGSt gst
    #   tb              =   showall statlist tb
    = (Return0Cci, {state & gamestate = newstate}, tb)
where
    showall :: [Statistic] !*OSToolbox -> *OSToolbox
    showall [] tb = tb
    showall [x:xs] tb
        # tb = showstat x tb
        = showall xs tb
    where
        showstat :: Statistic !*OSToolbox -> *OSToolbox
        showstat s tb
            | isJust s.shadow
                # (_,tb) = OSShowStatistic s.position.x s.position.y s.format (mi s.value)
                            s.color s.style.fontname s.style.fontsize s.style.bold
                            s.style.italic True sh.shadowpos.x sh.shadowpos.y
                            sh.shadowcolor (alint s.alignment) tb
                = tb
            # (_,tb) = OSShowStatistic s.position.x s.position.y s.format (mi s.value)
                            s.color s.style.fontname s.style.fontsize s.style.bold
                            s.style.italic False 0 0 Black (alint s.alignment) tb
            = tb
        where
            mi :: (Maybe Int) -> Int
            mi Nothing  = NOTHING
            mi (Just x) = x
            sh = fromJust s.shadow
            alint :: Alignment -> Int
            alint al = CompressBools (False, False, False, False, sy, sx, cy, cx)
              where
                (cx,cy) = al.xycentered
                (sx,sy) = al.xyfromscreencenter

handleGameEvents fromOSCci state tb
    = (Return0Cci,state,WinBeep tb)


MakePoint :: !Int !Int -> Point2
MakePoint a b = {x = a, y = b}



handleUserEvent :: !Int !InstanceID !Int !Int !Int !(OSGameData gs) !*OSToolbox -> (OSGameData gs, !*OSToolbox)
handleUserEvent objtype id ev par1 par2 data tb
    #   maybefound              =   getobject objtype data.gamehnd
    |   isJust maybefound
        #   obj                 =   fromJust maybefound
        #   (_, _, objrec, tb)  =   GetObjectRec id tb
        #   gst                 =   toGSt data.gamestate tb
        #   (obj,objrec,gst)    =   doUserEvent id obj objrec ev par1 par2 gst
        #   data                =   {data & gamehnd = putobject obj data.gamehnd}
        #   (newstate, tb)      =   fromGSt gst
        #   (_, tb)             =   SetObjectRec id objtype objrec obj.spriteids` tb
        =   ({data & gamestate=newstate}, tb)
    |   otherwise
        =   (data, tb)
where
    doUserEvent :: !InstanceID !.(ObjectHandle .a) !ObjectRec !Int !Int !Int !.a -> (.ObjectHandle .a,ObjectRec,.a);
    doUserEvent id obj=:{instances`,userevent`} objrec ev par1 par2 gst
        #   maybestate          =   findinstance instances` id
        |   isNothing maybestate
            =   (obj,objrec,gst)
        #   state               =   fromJust maybestate
        #   ((s,objrec),gst)    =   userevent` (state,objrec) ev par1 par2 gst
        #   newinstances        =   updateinstance id s instances`
        #   obj                 =   {obj & instances`=newinstances}
        =   (obj,objrec,gst)
//      =   ({obj & instances`=newinstances},objrec,gst)



handleAnimationEvent :: !Int !InstanceID !(OSGameData gs) !*OSToolbox -> (OSGameData gs, !*OSToolbox)
handleAnimationEvent objtype id data tb
    #   maybefound              =   getobject objtype data.gamehnd
    |   isJust maybefound
        #   obj                 =   fromJust maybefound
        #   (_, _, objrec, tb)  =   GetObjectRec id tb
        #   gst                 =   toGSt data.gamestate tb
        #   (obj,objrec,gst)    =   doAnimation id obj objrec gst
        #   data                =   {data & gamehnd = putobject obj data.gamehnd}
        #   (newstate, tb)      =   fromGSt gst
        #   (_, tb)             =   SetObjectRec id objtype objrec obj.spriteids` tb
        =   ({data & gamestate=newstate}, tb)
    |   otherwise
        =   (data, tb)
where
    doAnimation :: !InstanceID !.(ObjectHandle .a) !ObjectRec !.a -> (.ObjectHandle .a,ObjectRec,.a);
    doAnimation id obj=:{instances`,animation`} objrec gst
        #   maybestate          =   findinstance instances` id
        |   isNothing maybestate
            =   (obj,objrec,gst)
        #   state               =   fromJust maybestate
        #   ((s,objrec),gst)    =   animation` (state,objrec) gst
        #   newinstances        =   updateinstance id s instances`
        #   obj                 =   {obj & instances`=newinstances}
        =   (obj,objrec,gst)
//      =   ({obj & instances`=newinstances},objrec,gst)



handleTimerEvent :: !Int !InstanceID !(OSGameData gs) !*OSToolbox -> (OSGameData gs, !*OSToolbox)
handleTimerEvent objtype id data tb
    #   maybefound              =   getobject objtype data.gamehnd
    |   isJust maybefound
        #   obj                 =   fromJust maybefound
        #   (_, _, objrec, tb)  =   GetObjectRec id tb
        #   gst                 =   toGSt data.gamestate tb
        #   (obj,objrec,gst)    =   doTimer id obj objrec gst
        #   data                =   {data & gamehnd = putobject obj data.gamehnd}
        #   (newstate, tb)      =   fromGSt gst
        #   (_, tb)             =   SetObjectRec id objtype objrec obj.spriteids` tb
        =   ({data & gamestate=newstate}, tb)
    |   otherwise
        =   (data, tb)
where
    doTimer :: !InstanceID !.(ObjectHandle .a) !ObjectRec !.a -> (.ObjectHandle .a,ObjectRec,.a);
    doTimer id obj=:{instances`,frametimer`} objrec gst
        #   maybestate          =   findinstance instances` id
        |   isNothing maybestate
            =   (obj,objrec,gst)
        #   state               =   fromJust maybestate
        #   ((s,objrec),gst)    =   frametimer` (state,objrec) gst
        #   newinstances        =   updateinstance id s instances`
        #   obj                 =   {obj & instances`=newinstances}
        =   (obj,objrec,gst)
//      =   ({obj & instances`=newinstances},objrec,gst)



moveGameObject :: !Int !InstanceID !(OSGameData gs) !*OSToolbox -> (OSGameData gs, !*OSToolbox)
moveGameObject objtype id data tb
    #   maybefound              =   getobject objtype data.gamehnd
    |   isJust maybefound
        #   obj                 =   fromJust maybefound
        #   (_, _, objrec, tb)  =   GetObjectRec id tb
        #   gst                 =   toGSt data.gamestate tb
        #   (obj,objrec,gst)    =   doMove id obj objrec gst
        #   data                =   {data & gamehnd = putobject obj data.gamehnd}
        #   (newstate, tb)      =   fromGSt gst
        #   (_, tb)             =   SetObjectRec id objtype objrec obj.spriteids` tb
        =   ({data & gamestate=newstate}, tb)
    |   otherwise
        =   (data, tb)
where
    doMove :: !InstanceID !.(ObjectHandle .a) !ObjectRec !.a -> (.ObjectHandle .a,ObjectRec,.a);
    doMove id obj=:{instances`,move`} objrec gst
        #   maybestate          =   findinstance instances` id
        |   isNothing maybestate
            =   (obj,objrec,gst)
        #   state               =   fromJust maybestate
        #   ((s,objrec),gst)    =   move` (state,objrec) gst
        #   newinstances        =   updateinstance id s instances`
        #   obj                 =   {obj & instances`=newinstances}
        =   (obj,objrec,gst)
//      =   ({obj & instances`=newinstances},objrec,gst)


touchBound :: !Int !InstanceID !Int !Int !(OSGameData gs) !*OSToolbox -> (OSGameData gs, !*OSToolbox)
touchBound objtype id dir mapcode data tb
    #   directions              =   makeDirectionSet dir
    #   maybefound              =   getobject objtype data.gamehnd
    |   isJust maybefound
        #   obj                 =   fromJust maybefound
        #   (_, _, objrec, tb)  =   GetObjectRec id tb
        #   gst                 =   toGSt data.gamestate tb
        #   (obj,objrec,gst)    =   doTouchBound id obj objrec directions mapcode gst
        #   data                =   {data & gamehnd = putobject obj data.gamehnd}
        #   (newstate, tb)      =   fromGSt gst
        #   (_, tb)             =   SetObjectRec id objtype objrec obj.spriteids` tb
        =   ({data & gamestate=newstate}, tb)
    |   otherwise
        =   (data, tb)
where
    doTouchBound :: !InstanceID !.(ObjectHandle .a) !ObjectRec !DirectionSet !Int !.a -> (.ObjectHandle .a,ObjectRec,.a);
    doTouchBound id obj=:{instances`,touchbound`} objrec bounds mapcode gst
        #   maybestate          =   findinstance instances` id
        |   isNothing maybestate
            =   (obj,objrec,gst)
        #   state               =   fromJust maybestate
        #   ((s,objrec),gst)    =   touchbound` (state,objrec) bounds mapcode gst
        #   newinstances        =   updateinstance id s instances`
        #   obj                 =   {obj & instances`=newinstances}
        =   (obj,objrec,gst)



initialiseGameObject :: !Int !Int !InstanceID !Point2 !Int !(OSGameData gs) !*OSToolbox
                                                    -> (OSGameData gs, !*OSToolbox)
initialiseGameObject objtype subtype id p time data tb
    #   maybefound              =   getobject objtype data.gamehnd
    |   isJust maybefound
        #   obj                 =   fromJust maybefound
        #   gst                 =   toGSt data.gamestate tb
        #   (obj,objrec,gst)    =   doInit id subtype p time obj gst
        #   data                =   {data & gamehnd = putobject obj data.gamehnd}
        #   (newstate, tb)      =   fromGSt gst
        #   (_, tb)             =   SetObjectRec id objtype objrec obj.spriteids` tb
        =   ({data & gamestate=newstate}, tb)
    |   otherwise
        =   (data, tb)
where
    doInit :: !InstanceID !Int !Point2 !GameTime !.(ObjectHandle .a) !.a -> (.ObjectHandle .a,ObjectRec,.a);
    doInit id subtype p time obj=:{instances`,init`} gst
        #   ((state,objrec), gst)       =   init` subtype p time gst
        #   newinstances                =   [(id,state):instances`]
        =   ({obj & instances`=newinstances},objrec,gst)


doneGameObject :: !Int !InstanceID !(OSGameData gs) !*OSToolbox -> (OSGameData gs, !*OSToolbox)
doneGameObject objtype id data tb
    #   maybefound              =   getobject objtype data.gamehnd
    |   isJust maybefound
        #   obj                 =   fromJust maybefound
        #   (_, _, objrec, tb)  =   GetObjectRec id tb
        #   gst                 =   toGSt data.gamestate tb
        #   (obj,gst)           =   doDone id obj objrec gst
        #   data                =   {data & gamehnd = putobject obj data.gamehnd}
        #   (newstate, tb)      =   fromGSt gst
        #   (_, tb)             =   SetObjectRec id objtype objrec obj.spriteids` tb
        =   ({data & gamestate=newstate}, tb)
    |   otherwise
        =   (data, tb)
where
    doDone :: !InstanceID !.(ObjectHandle .a) !ObjectRec !.a -> (.ObjectHandle .a,.a);
    doDone id obj=:{instances`,done`} objrec gst
        #   maybestate          =   findinstance instances` id
        |   isNothing maybestate
            =   (obj, gst)
        #   state               =   fromJust maybestate
        #   gst                 =   done` (state,objrec) gst
        #   newinstances        =   removeinstance id instances`
        #   obj                 =   {obj & instances`=newinstances}
        =   (obj, gst)
//      =   ({obj & instances`=newinstances}, gst)



handleCollision :: !Int !InstanceID !Int !InstanceID !Int !(OSGameData gs) !*OSToolbox -> (OSGameData gs, !*OSToolbox)
handleCollision ot1 id1 ot2 id2 dir data tb
    #   directions              =   makeDirectionSet dir
    #   maybefound              =   getobject ot1 data.gamehnd
    |   isJust maybefound
        #   obj                 =   fromJust maybefound
        #   (_, _, objrec2, tb) =   GetObjectRec id2 tb
        #   (_, _, objrec1, tb) =   GetObjectRec id1 tb
        #   gst                 =   toGSt data.gamestate tb
        #   (obj,objrec1,gst)   =   doCollision id1 obj objrec1 directions ot2 objrec2 gst
        #   data                =   {data & gamehnd = putobject obj data.gamehnd}
        #   (newstate, tb)      =   fromGSt gst
        #   (_, tb)             =   SetObjectRec id1 ot1 objrec1 obj.spriteids` tb
        =   ({data & gamestate=newstate}, tb)
    |   otherwise
        =   (data, tb)
where
    doCollision :: !InstanceID !.(ObjectHandle .a) !ObjectRec !DirectionSet !Int !ObjectRec !.a -> (.ObjectHandle .a,ObjectRec,.a);
    doCollision id obj=:{instances`,collide`} objrec1 bounds ot2 objrec2 gst
        #   maybestate          =   findinstance instances` id
        |   isNothing maybestate
            =   (obj,objrec1,gst)
        #   state               =   fromJust maybestate
        #   ((s,objrec1),gst)   =   collide` (state,objrec1) bounds ot2 objrec2 gst
        #   newinstances        =   updateinstance id s instances`
        #   obj                 =   {obj & instances`=newinstances}
        =   (obj,objrec1,gst)
//      =   ({obj & instances`=newinstances},objrec1,gst)



handleKeyDown :: !Int !InstanceID !Int !(OSGameData gs) !*OSToolbox -> (OSGameData gs, !*OSToolbox)
handleKeyDown objtype id key data tb
    #   maybefound              =   getobject objtype data.gamehnd
    |   isJust maybefound
        #   obj                 =   fromJust maybefound
        #   (_, _, objrec, tb)  =   GetObjectRec id tb
        #   gst                 =   toGSt data.gamestate tb
        #   (obj,objrec,gst)    =   doKeyDown id obj objrec key gst
        #   data                =   {data & gamehnd = putobject obj data.gamehnd}
        #   (newstate, tb)      =   fromGSt gst
        #   (_, tb)             =   SetObjectRec id objtype objrec obj.spriteids` tb
        =   ({data & gamestate=newstate}, tb)
    |   otherwise
        =   (data, tb)
where
    doKeyDown :: !InstanceID !.(ObjectHandle .a) !ObjectRec !Int !.a -> (.ObjectHandle .a,ObjectRec,.a);
    doKeyDown id obj=:{instances`,keydown`} objrec key gst
        #   maybestate          =   findinstance instances` id
        |   isNothing maybestate
            =   (obj,objrec,gst)
        #   state               =   fromJust maybestate
        #   ((s,objrec),gst)    =   keydown` (state,objrec) key gst
        #   newinstances        =   updateinstance id s instances`
        #   obj                 =   {obj & instances`=newinstances}
        =   (obj,objrec,gst)
//      =   ({obj & instances`=newinstances},objrec,gst)


handleKeyUp :: !Int !InstanceID !Int !(OSGameData gs) !*OSToolbox -> (OSGameData gs, !*OSToolbox)
handleKeyUp objtype id key data tb
    #   maybefound              =   getobject objtype data.gamehnd
    |   isJust maybefound
        #   obj                 =   fromJust maybefound
        #   (_, _, objrec, tb)  =   GetObjectRec id tb
        #   gst                 =   toGSt data.gamestate tb
        #   (obj,objrec,gst)    =   doKeyUp id obj objrec key gst
        #   data                =   {data & gamehnd = putobject obj data.gamehnd}
        #   (newstate, tb)      =   fromGSt gst
        #   (_, tb)             =   SetObjectRec id objtype objrec obj.spriteids` tb
        =   ({data & gamestate=newstate}, tb)
    |   otherwise
        =   (data, tb)
where
    doKeyUp :: !InstanceID !.(ObjectHandle .a) !ObjectRec !Int !.a -> (.ObjectHandle .a,ObjectRec,.a);
    doKeyUp id obj=:{instances`,keyup`} objrec key gst
        #   maybestate          =   findinstance instances` id
        |   isNothing maybestate
            =   (obj,objrec,gst)
        #   state               =   fromJust maybestate
        #   ((s,objrec),gst)    =   keyup` (state,objrec) key gst
        #   newinstances        =   updateinstance id s instances`
        #   obj                 =   {obj & instances`=newinstances}
        =   (obj,objrec,gst)
//      =   ({obj & instances`=newinstances},objrec,gst)


