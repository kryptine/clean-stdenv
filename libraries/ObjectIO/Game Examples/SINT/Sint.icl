module Sint

/*
    Sint Nicolaas

    Written by Mike Wiering (mike.wiering@cs.kun.nl)
    Katholieke Universiteit Nijmegen
    
    For more information, see: http://www.cs.kun.nl/is/sint/
    
    
    Compile with 3200K heap space (Project, Application options)
*/

import StdEnv, StdIO
import StdGameDef, StdGame, StdGSt
import notes
import osgame, gst
import gameobjectutils
import Random
/* import ArgEnv */

/* import generated code */

import OBJ      // id: Objects
import PPN      // id: Pepernoot
import MC       // id: MainChar
import STAT     // id: Status
import SUN      // id: SunRise
import CARS     // id: Cars
import FADE     // id: Fade

import L1       // id: Level1
import B1       // id: BackGr1
import S1       // id: Static1
import F1       // id: ForeGr1
import L1FRONT  // id: Level1Front

import L1B      // id: Level1b
import B1B      // id: BackGr1b
import S1B      // id: Static1b

import L2       // id: Level2
import B2       // id: BackGr2
import S2       // id: Static2

import L2B      // id: Level2b


VERSION :== "Sint '99 - Versie 1.0"


/* layers */
LYR_BACKGROUND   :==   1
LYR_MIDGROUND    :==   2
LYR_FOREGROUND   :==   3
LYR_KADO         :==   8
LYR_PLAYER       :==   9
LYR_INFRONT      :==  10
LYR_GLITTER      :==  20
LYR_STATUS       :==  30

/* main character defaults */
DEFAULT_LIVES :== 3
DEFAULT_TIME  :== 250
BONUS_TIME    :== 125
TODO          :== (5 * (5 + 1))

BONUS_PPN     :== 50
EXTRA_PPN     :== 15

/* scrolling text positions */
START_MSG     :== 340
STOP_MSG      :== -340

/* level result codes */
EC_NONE       :==  0
EC_SUCCESS    :==  1
EC_FAILURE    :==  2
EC_QUIT       :==  3

FIRST_TIME    :== -1000

SHOW_PERS_HIS :== -2000
SHOW_HIS      :== -2001

FPS           :== 70
FADE          :== True
ESC_QUIT      :== True

CARDWARE      :== False


/* high scores */

HIGH_SCORES_FILE :== "SINT99.HIS"

::  HiS
    = { name    :: !String
      , hiscore :: !Int
      }

::  CodedHiS
    = { codedhiscore :: !String    /* binary representation of name and score */
      }

emptyhis = { codedhiscore = "" }

MAX_NUMBER_LEN :== 16
MAX_NAME_LEN   :== 32

GRONDTAL  :==  10


DecodeHiS :: CodedHiS -> [HiS]
DecodeHiS chis
    = decodehis ([c \\ c <-: chis.codedhiscore] ++ ['\n']) "" 0 0
where
    decodehis :: [Char] !String !Int !Int -> [HiS]
    decodehis [] _ _ _ = []
    decodehis [c:cs] s i n
        | n < MAX_NUMBER_LEN                 = decodehis cs s ((i * GRONDTAL) + (toInt c)) (n + 1)
        | n < MAX_NAME_LEN + MAX_NUMBER_LEN  = decodehis cs (s +++ (if (toInt c == 0) "" (toString c))) i (n + 1)
        | otherwise                          = [{name = s, hiscore = i}: decodehis [c:cs] "" 0 0] 

EncodeHiS :: [HiS] -> CodedHiS
EncodeHiS his
    # chis = codehis his
    = { codedhiscore = {c \\ c <- chis}}
where
    codehis :: [HiS] -> [Char]
    codehis [] = []
    codehis [h:hs] = (codeh h) ++ (codehis hs)

    codeh h =: {name, hiscore} = (reverse (addzero (codehiscore hiscore) MAX_NUMBER_LEN)) ++
                                 (addzero [c \\ c <-: name] MAX_NAME_LEN)

    codehiscore :: Int -> [Char]
    codehiscore 0 = []
    codehiscore score
        = [toChar (score rem GRONDTAL)] ++ codehiscore (score / GRONDTAL)

    addspaces :: [Char] Int -> [Char]
    addspaces c n
        | length c < n = (addspaces c (n - 1)) ++ [' ']
        | otherwise    = c

    addzero :: [Char] Int -> [Char]
    addzero c n
        | length c < n = (addzero c (n - 1)) ++ [toChar 0]
        | otherwise    = c


ReadScores :: !String !*Files -> (!(!*File, !CodedHiS), !*Files)
ReadScores filename files
    # (exists, file, files) = fopen fpath FReadText files
    | exists
        # (codedhis, file) = ReadHiS file
        = ((file, codedhis), files)
    # (_, file, files) = fopen fpath FWriteText files
    | otherwise
        = ((file, emptyhis), files)
where
    fpath = filename

    ReadHiS :: !*File -> (!CodedHiS, !*File)
    ReadHiS file
        | sfend file
            = (emptyhis, file)
        # (codedhi, file) = freadline file
        = ({codedhiscore = codedhi}, file)

WriteScores :: !*File !CodedHiS !*Files -> *Files
WriteScores file codedhis files
    # (ok, file) = freopen file FWriteText
    | not ok
        = snd (fclose file files)
    # file = WriteHiS codedhis file    
    = snd (fclose file files)
where
    WriteHiS :: !CodedHiS !*File -> *File
    WriteHiS codedhis file
        = file <<< codedhis.codedhiscore <<< '\n'


FindScore :: String CodedHiS -> Int
FindScore s codedhis
    = findscore s (DecodeHiS codedhis)
where
    findscore :: String [HiS] -> Int
    findscore _ [] = 0
    findscore s [h:hs]
        | h.name == s   = h.hiscore
        | otherwise     = findscore s hs

UpdateScore gst
    # personalscore = FindScore gst.player gst.codedhis
    | (gst.score == 0) || (gst.player == "")
        = {gst & pershiscore = personalscore, rank = 10000}
    # hislist = DecodeHiS gst.codedhis
    # (hislist, newrank) = addscoretolist hislist {name = gst.player, hiscore = gst.score} 1
    # gst = {gst & codedhis = EncodeHiS hislist}
    = {gst & pershiscore = personalscore, rank = newrank}
where
    addscoretolist :: [HiS] HiS Int -> ([HiS], Int)
    addscoretolist [] h n = ([h], n)
    addscoretolist [x:xs] h n
        | h.hiscore > x.hiscore = ([h:[x:xs]], n)
        | otherwise
             # (newxs, newn) = addscoretolist xs h (n + 1)
             = ([x:newxs], newn)


/* the game state definition */
::  GameState
    = { curlevel    :: !Int         /* 1 = title, 2 = level1, 3 = bonus1, 4 = level2, etc */
      , maxlevel    :: !Int
      , titlescreen :: !Bool
      , quit        :: !Bool
      , statusline  :: !Bool
      , exitcode    :: !Int
      , lives       :: !Int
      , ppn         :: !Int         /* pepernoten */
      , time        :: !Int
      , score       :: !Int
      , gameover    :: !Bool
      , bonus       :: !Bool
      , bonusmsg    :: !Int         /* current position of scrolling message */
      , morningmsg  :: !Int
      , readymsg    :: !Int
      , notreadymsg :: !Int
      , player      :: !String      /* player's name */
      , codedhis    :: !CodedHiS
      , pershiscore :: !Int
      , rank        :: !Int
      , cursor      :: !Int
      , randseed    :: !RandomSeed
      }

initialGameState = { curlevel    = 0
                   , maxlevel    = 5
                   , titlescreen = True
                   , quit        = False
                   , statusline  = True
                   , exitcode    = EC_NONE
                   , lives       = DEFAULT_LIVES
                   , ppn         = 0
                   , time        = FIRST_TIME
                   , score       = 0
                   , gameover    = False
                   , bonus       = False
                   , bonusmsg    = STOP_MSG
                   , morningmsg  = STOP_MSG
                   , readymsg    = STOP_MSG
                   , notreadymsg = STOP_MSG
                   , player      = ""
                   , codedhis    = emptyhis
                   , pershiscore = 0
                   , rank        = 0
                   , cursor      = 0
                   , randseed    = nullRandomSeed
                   }

/* ---------- main program: load game definition and run the game! ---------- */

Start world
    # (randomSeed, world) = getNewRandomSeed world
    # initgs = {initialGameState & randseed = randomSeed}
    = startIO SDI initgs init [ProcessClose closeProcess] world
where
    init ps=:{ls=initgs}
        # ((hisfile, codedhis), ps) = accFiles (ReadScores HIGH_SCORES_FILE) ps
        # gs = {initgs & codedhis = codedhis}
        # (finalgamestate, _, ps) = openGame gs SintGame [ColorDepth depth, ScreenSize {w=320,h=200}] ps
        # ps = appFiles (WriteScores hisfile finalgamestate.codedhis) ps
        = closeProcess ps

    depth = 16
/*
    depth = if ((parameter "/8") || (parameter "-8")) 8 16

    parameter arg
        = checkpar arg getCommandLine 0
    where
        checkpar arg cmdline n
            | n >= (size cmdline) = False
            | cmdline.[n] == arg  = True
            | otherwise           = (checkpar arg cmdline (n + 1)) 
*/    

/* ---------- the complete game definition starts here ---------- */

SintGame :: (Game GameState)
SintGame =
    { levels = [ TitleScreen
               , Level1
               , Level1b
               , Level2
               , Level2b

               ]
    , quitlevel = accGSt QuitFunction
    , nextlevel = accGSt NextLevelFunction
    , textitems = accGSt GameTexts
    }


/* if the quit function returns true, the game engine quits the level */

QuitFunction :: GameState -> (Bool, GameState)
QuitFunction gst
    = (gst.quit, {gst & quit = False})


/* function that returns the next level to run, 0 = end game */

NextLevelFunction :: GameState -> (Int, GameState)
NextLevelFunction gst =: {curlevel, maxlevel, exitcode, lives, gameover, bonus}
    | exitcode == EC_QUIT
        = (0, gst)
    | (curlevel == 0) || gameover
        = title gst
    | curlevel == 1  // title screen
        = (next, resetgame gst)
    | exitcode == EC_FAILURE
        | lives > 0
            = (curlevel, {(resetlevel gst) & lives = lives - 1})
        = title gst
    | exitcode == EC_SUCCESS
        | not (normallevel curlevel)
            # (lev, gst) = nextlevel
            = (lev, {gst & bonus = False})
        = nextlevel
    = title gst
where
    resetgame gst
        = { resetlevel gst 
          & titlescreen = False
          , statusline = True
          , lives = DEFAULT_LIVES
          , score = 0
          , curlevel = next
          }

    resetlevel gst
        = { gst
          & ppn = 0
          , time = DEFAULT_TIME
          , gameover = False
          , bonus = False
          }

    title gst
        # gst = UpdateScore gst
        # gst = if (gst.time == FIRST_TIME) gst {gst & time = SHOW_PERS_HIS}
        = (1, gsttitle gst)
    where
        gsttitle gst
            = { gst 
              & titlescreen = True
              , statusline = False
              , gameover = False
              , curlevel = 1
              }

    nextlevel = if (next > maxlevel)
                     (title gst)
                     (next, { gst 
                            & curlevel = next
                            , time = if (normallevel next) DEFAULT_TIME BONUS_TIME
                            , bonusmsg    = STOP_MSG
                            , morningmsg  = STOP_MSG
                            , readymsg    = STOP_MSG
                            , notreadymsg = STOP_MSG
                            })

    next
        | (normallevel curlevel)
            | bonus
                = curlevel + 1  // play bonus level
            = curlevel + 2      // next (normal) level
        = curlevel + 1          // coming from a bonus level, play next level

    normallevel n = ((n rem 2) == 0)


/* function that returns text to be displayed */

GameTexts :: GameState -> ([GameText], GameState)
GameTexts gst
    | gst.statusline
        # (msg, gst) = if (gst.morningmsg > STOP_MSG) 
                           ([Morning gst.morningmsg], {gst & morningmsg = gst.morningmsg - 2})
                           ([], gst)
        # (msg, gst) = if (gst.bonusmsg > STOP_MSG) 
                           ([Bonus gst.bonusmsg], {gst & bonusmsg = gst.bonusmsg - 2})
                           (msg, gst)
        # (msg, gst) = if (gst.readymsg > STOP_MSG) 
                           ([Ready gst.readymsg], {gst & readymsg = gst.readymsg - 2})
                           (msg, gst)
        # (msg, gst) = if (gst.notreadymsg > STOP_MSG) 
                           ([NotReady gst.notreadymsg], {gst & notreadymsg = gst.notreadymsg - 2})
                           (msg, gst)
        = ([ Lives    gst.lives
           , TimeLeft (if (gst.time < 0) 0 gst.time)
           , PPN      gst.ppn
           , Score    gst.score
           ] 
           ++ (if gst.gameover [GameOver] [])
           ++ (if (gst.time < 0) [TimeUp] [])
           ++ msg
           , gst)
    | gst.time == FIRST_TIME
        # gst = {gst & cursor = gst.cursor + 1}
        = ([Version, TypeNaam, Naam (" " +++ gst.player +++ (if ((gst.cursor / 10) rem 2 == 0) " " "|"))], gst)
    | gst.time == FIRST_TIME - 1
        # gst = {gst & cursor = gst.cursor + 1}
        = ([Version, TypeNaam, Naam gst.player, Pers (FindScore gst.player gst.codedhis)], gst)
    | gst.time == SHOW_PERS_HIS
        = ((PersHiScores (DecodeHiS gst.codedhis) gst.score gst.pershiscore gst.player gst.rank), gst)
    | gst.time == SHOW_HIS
        = ((HiScores (DecodeHiS gst.codedhis)), gst)
    = ([], gst)


/* ---------- bounds and event codes ---------- */

/* bounds: groups of objects that collide in the same way */

BND_MAIN_CHARACTER     :==  (1 <<  0)
BND_POWER_UP           :==  (1 <<  1)
BND_KADO               :==  (1 <<  2)
BND_ENEMY              :==  (1 <<  3)
BND_BLOCKS             :==  (1 <<  4)
BND_CHIMNEY            :==  (1 <<  5)
BND_ENDING             :==  (1 <<  6)
BND_STAT               :==  (1 <<  7)
BND_HURT               :==  (1 <<  8)


/* predefined bounds
BND_MAP_CODES          :==  (1 << 30)  // codes defined in EDLEV
BND_STATIC_BOUNDS      :==  (1 << 31)  // bounds defined in EDLEV
*/


/* event codes objects can send to each other (use createUserGameEvent) */

EV_QUIT_LEVEL          :==    1
EV_GAME_OVER           :==    2
EV_TIME_UP             :==    3

EV_READY               :==    5
EV_KADO_DROPPED        :==    6
EV_PEPERNOOT           :==    7
EV_ALL_DONE            :==    8

EV_STOP_BLINKING       :==   10
EV_STOP_MOVING         :==   11

EV_COUNT_SCORE         :==   14
EV_PPN_SCORE           :==   15
EV_ADD_BIRD_SCORE      :==   16

EV_SUNRISE             :==   18

EV_HEALTH              :==   20
EV_TIMER               :==   21

EV_POS                 :==  100




/* ---------- objects ---------- */

/* object codes (must correspond with level map) */

OBJ_AUTOINIT           :==     0

OBJ_START              :==  0x10

OBJ_STATIC_PEPERNOOT   :==  0x10
OBJ_FALLING_PEPERNOOT  :==  0x11
OBJ_LETTER             :==  0x12
OBJ_HEART              :==  0x13
OBJ_LIFE               :==  0x14

OBJ_FALLING_KADO1      :==  0x15
OBJ_FALLING_KADO2      :==  0x16
OBJ_FALLING_KADO3      :==  0x17
OBJ_FALLING_KADO4      :==  0x18
OBJ_FALLING_KADO5      :==  0x19

OBJ_BOUNCEBLOCK        :==  0x20
OBJ_TRAMP              :==  0x21

OBJ_CHIMNEY1           :==  0x25
OBJ_CHIMNEY2           :==  0x26
OBJ_CHIMNEY3           :==  0x27
OBJ_CHIMNEY4           :==  0x28
OBJ_CHIMNEY5           :==  0x29

OBJ_ANTENNE            :==  0x30

OBJ_CAR                :==  0x40

OBJ_FRONT              :==  0x5F

OBJ_FADE               :==  0x60

OBJ_BIRD               :==  0x81



OBJ_MAIN_CHAR          :==  0xF0

OBJ_ENDING             :==  0xFE


OBJ_KADO1              :== 0x101
OBJ_KADO2              :== 0x102
OBJ_KADO3              :== 0x103
OBJ_KADO4              :== 0x104
OBJ_KADO5              :== 0x105

OBJ_FLITS              :== 0x110
OBJ_STAT               :== 0x111

OBJ_SUN                :== 0x115


// default block size

W :== 20
H :== 20

BLOCK_SIZE :== {w = W, h = H}
ITEM_SIZE  :== {w = W, h = 16}

NEVER_FORGET :== {x = 10000, y = 10000}


GameObjectList = [ AutoInitObject
                 , StatHeartObject

                 , StaticPepernoot
                 , FallingPepernoot
                 , FallingLetter
                 , HeartObject
                 , LifeObject
                 
                 , Kado1
                 , Kado2
                 , Kado3
                 , Kado4
                 , Kado5
                 
                 , KadoObject 1
                 , KadoObject 2
                 , KadoObject 3
                 , KadoObject 4
                 , KadoObject 5

                 , ChimneyObject 1
                 , ChimneyObject 2
                 , ChimneyObject 3
                 , ChimneyObject 4
                 , ChimneyObject 5
                                  
                 , BounceBlockObject
                 , TrampObject

                 , BirdObject

                 , FadeObject 0
                 , FadeObject 1
                 , FadeObject 2
                 , FadeObject 3
                 , FadeObject 4
                 , FadeObject 5
                 , FadeObject 6
                 , FadeObject 7
                 , FadeObject 8
                 , FadeObject 9
                 
                 , AntenneObject
                 
                 , FlitsObject

                 , SunObject
                 
                 , EndingObject

                 , CarObject
                 
                 , MainCharObject
                 ] 


/* ---------- autoinit object ---------- */

/*
   this object is automatically initialized when the level starts
*/

STY :== 183
STS :== -5

AutoInitObject
    # obj = defaultGameObject OBJ_AUTOINIT size Void
    # obj = { obj
            & init  = (newinit size Void)
            }
    = obj
where
    size = {w = 1, h = 1}
    newinit size state subcode pos time gs
        # gs = setexitcode EC_NONE gs
        # (_, gs) = createNewGameObject OBJ_STAT  1       {x = 181-4, y = STY} gs
        # (_, gs) = createNewGameObject OBJ_STAT  2       {x = 193-4, y = STY} gs
        # (_, gs) = createNewGameObject OBJ_STAT  3       {x = 205-4, y = STY} gs
        # (_, gs) = createNewGameObject OBJ_STAT ST_X     {x =  29, y = STY} gs
        # (_, gs) = createNewGameObject OBJ_STAT ST_COLON {x = 257, y = STY} gs
        # (_, gs) = createNewGameObject OBJ_STAT ST_TIME  {x =  85, y = STY} gs
        # (_, gs) = createUserGameEvent EV_TIMER
                        0 0 (BoundType BND_STAT) ST_TIME (FPS) gs
        # (_, gs) = createNewGameObject OBJ_STAT ST_COLON {x =  83, y = STY} gs
        # (_, gs) = createNewGameObject OBJ_STAT ST_PPN   {x = 133-4, y = STY} gs
        # (_, gs) = createNewGameObject OBJ_STAT ST_X     {x = 142-4, y = STY} gs

        # (objrec, gs) = defaultObjectRec subcode pos size time gs
        # objrec = {objrec & active = False}
        # (bonus, gs) = getbonus gs
        | bonus
            = {st=state, or=objrec, gs = msgbonus gs}
        = {st=state, or=objrec, gs=gs}



/* ---------- chimney objects ---------- */

ChimneyObject n
    # obj = defaultGameObject (OBJ_CHIMNEY1 + n - 1) ITEM_SIZE Void
    # obj = { obj
            & sprites = [ ChimneySprite 0, ChimneySprite 2, ChimneySprite 3
                        , ChimneySprite 4, ChimneySprite 5, ChimneySprite 6 
                        , ChimneySprite (m + 1), ChimneySprite (m + 2), ChimneySprite (m + 3)
                        , ChimneySprite (m + 4), ChimneySprite (m + 5), ChimneySprite (m + 6) 
                        ]
            , init      = (newinit ITEM_SIZE Void)
            , userevent = newuserevent
            }
    = obj
where
    m = n * 6
    
    newinit size state subcode pos time gs
        # pos = {x = pos.x + W / 2, y = pos.y + H / 2}
        # (objrec, gs) = defaultObjectRec subcode pos size time gs
        # objrec = { objrec
                   & layer          = AtLayer LYR_FOREGROUND
                   , currentsprite  = 12
                   , ownbounds      = BND_CHIMNEY
                   , offset         = {x = 0, y = BLOCK_SIZE.h} 
                   , forgetdistance = NEVER_FORGET
                   }
        = {st=state, or=objrec, gs=gs}
            
    newuserevent ev evpar1 evpar2 {st, or, gs}
        | (ev == EV_PEPERNOOT) && (evpar1 == n)
            = {st=st, or = {or & currentsprite = or.currentsprite - 1}, gs = addscore 100 gs}
        | (ev == EV_KADO_DROPPED) && (evpar1 == n)
            = {st=st, or = {or & currentsprite = or.currentsprite - 6}, gs = addscore 500 gs}
        = {st=st, or=or, gs=gs}


/* ---------- Fading block ---------- */

FadeObject n
    # obj = defaultGameObject (OBJ_FADE + n) size Void
    # obj = { obj
            & sprites   = [FadeSprite1 n, FadeSprite2 n]
            , init      = (newinit size Void)
            , collide   = newcollide
            , animation = killobject
            }
    = obj  
where
    size    = {w = 20, h = 14}

    newinit size state subcode pos time gs
        # (objrec, gs) = defaultObjectRec subcode pos size time gs
        # objrec = { objrec
                   & ownbounds      = BND_STATIC_BOUNDS
                   , collidebounds  = BND_MAIN_CHARACTER
                   , forgetdistance = {x = 10, y = 12}
                   , layer          = AtLayer LYR_FOREGROUND
                   } 
        = {st=state, or=objrec, gs=gs}

    newcollide bnds objtype objrec {st, or, gs}
        # or = {or & collidebounds = 0
                   , currentsprite = 2
                   , options.removemapcode = True
                   }
        = {st=st, or=or, gs = addscore 10 gs}


/* ---------- Antenne ---------- */

AntenneObject
    # obj = defaultGameObject OBJ_ANTENNE size Void
    # obj = { obj
            & init    = (newinit size Void)
            }
    = obj  
where
    size    = {w = 20, h = 16}

    newinit size state subcode pos time gs
        # (objrec, gs) = defaultObjectRec subcode pos size time gs
        # objrec = { objrec
                   & ownbounds      = BND_HURT + BND_STATIC_BOUNDS
                   , subcode        = fromDirectionSet {left = False, right = False,
                                                        top = True, bottom = True}
                   } 
        = {st=state, or=objrec, gs=gs}


/* ---------- in front ---------- */

Level1FrontObj = [ L1FrontObject 1
                 , L1FrontObject 2
                 , L1FrontObject 3
                 , L1FrontObject 4
                 ]


L1FrontObject n
    # obj = defaultGameObject (OBJ_FRONT - n + 1) BLOCK_SIZE Void
    # obj = { obj
            & sprites = [L1FrontObjectSprite n]
            , init = (newinit BLOCK_SIZE Void)
            }
    = obj
where
    newinit size state subcode pos time gs
        # (objrec, gs) = defaultObjectRec subcode pos size time gs
        # objrec = { objrec & layer = AtLayer LYR_INFRONT }
        = {st=state, or=objrec, gs=gs}
        

ChimneyList = [ OBJ_CHIMNEY1
              , OBJ_CHIMNEY2
              , OBJ_CHIMNEY3
              , OBJ_CHIMNEY4
              , OBJ_CHIMNEY5
              ]
              

KadoList = [ OBJ_FALLING_KADO1
           , OBJ_FALLING_KADO2
           , OBJ_FALLING_KADO3
           , OBJ_FALLING_KADO4
           , OBJ_FALLING_KADO5
           ]


/* ---------- Sun ---------- */

SunObject
    # obj = defaultGameObject OBJ_SUN size Void
    # obj = { obj
            & sprites = [SunSprite]
            , init = (newinit size Void)
            , userevent = newuserevent
            }
    = obj
where
    size = {w = 40, h = 32}

    newinit size state subcode pos time gs
        # (objrec, gs) = defaultObjectRec subcode pos size time gs
        # objrec = { objrec & layer = AtLayer LYR_BACKGROUND
                            , options = { objrec.options & static = True }
                            , subcode = 1
                   }
//        # (rnd, gs) = IRRnd 5 gs
//        # (_, gs) = playSoundSample SND_COCK DefaultVolume PAN_CENTER 
//                       (getnotefreq (90 + rnd)) 0 gs
        # (_, gs) = createUserGameEvent EV_SUNRISE 0 0 Self ANY_SUBTYPE 3 gs
        = {st=state, or=objrec, gs=gs}

    newuserevent ev _ _ {st, or, gs}
        | ev == EV_SUNRISE
            # or = {or & pos = {x = or.pos.x, y = or.pos.y - 1}}
            | or.pos.y > 2
                # (_, gs) = createUserGameEvent EV_SUNRISE 0 0 Self ANY_SUBTYPE 1 gs
                = {st=st, or=or, gs=gs}
            = {st=st, or=or, gs=gs}
        = {st=st, or=or, gs=gs}

/* ---------- items ---------- */

StaticPepernoot  = StaticGameItem  OBJ_STATIC_PEPERNOOT  PepernootSprite  {w = 14, h = 12}
FallingPepernoot = FallingGameItem OBJ_FALLING_PEPERNOOT PepernootSprite  {w = 14, h = 12}

FallingLetter = FallingGameItem OBJ_LETTER (LetterSprite 0) ITEM_SIZE
HeartObject   = FallingGameItem OBJ_HEART  HeartSprite ITEM_SIZE
LifeObject    = FallingGameItem OBJ_LIFE   LifeSprite  ITEM_SIZE

Kado1 = FallingGameItem OBJ_FALLING_KADO1 (KadoSprite 1) ITEM_SIZE
Kado2 = FallingGameItem OBJ_FALLING_KADO2 (KadoSprite 2) ITEM_SIZE
Kado3 = FallingGameItem OBJ_FALLING_KADO3 (KadoSprite 3) ITEM_SIZE
Kado4 = FallingGameItem OBJ_FALLING_KADO4 (KadoSprite 4) ITEM_SIZE
Kado5 = FallingGameItem OBJ_FALLING_KADO5 (KadoSprite 5) ITEM_SIZE



FallingGameItem objecttype sprite size
    # obj = StaticGameItem objecttype sprite size
    # obj = { obj
            & init = (newinit size Void)
            }
    | (objecttype == OBJ_LETTER)
        = {obj & sprites = [LetterSprite 0, LetterSprite 1, LetterSprite 2, LetterSprite 3, 
                                GlitterSprite]}
    = obj
where
    newinit size state subcode pos time gs
        # oldpos = pos
        # pos = {pos & x = if (subcode == 1) (pos.x + W / 2) (pos.x)}
        # (objrec, gs) = defaultObjectRec subcode pos size time gs
        # objrec = { objrec
                   & acceleration   = {rx = 0.0, ry = 1.0 / 16.0}
                   , slowdown       = {fvx = Factor (1.0 / 24.0), fvy = Value 0.0}
                   , bounce         = {fvx = Value 0.0, fvy = Factor (3.0 / 5.0)}
                   , ownbounds      = BND_POWER_UP
                   , bouncebounds   = BND_STATIC_BOUNDS
                   , collidebounds  = BND_MAIN_CHARACTER + BND_ENEMY
                   , layer          = AtLayer LYR_FOREGROUND
                   , forgetdistance = {x = 8, y = 10}
                   }
        | (objecttype == OBJ_LETTER)
            = {st=state, or = {objrec & currentsprite = subcode + 1, pos = oldpos}, gs=gs}
        | (isMember objecttype [OBJ_FALLING_PEPERNOOT, OBJ_STATIC_PEPERNOOT])
            = {st=state, or = {objrec & pos.x = objrec.pos.x + (20 - 14) / 2}, gs=gs}
        | (objecttype == OBJ_LIFE)
            # (ppn, gs) = getppn gs
            | ppn == BONUS_PPN
                = {st=state, or=objrec, gs=gs}
            = killobject {st=state, or=objrec, gs=gs}
        = {st=state, or=objrec, gs=gs}


StaticGameItem objecttype sprite size
    # obj = defaultGameObject objecttype size Void
    # obj = { obj
            & sprites   = [sprite, GlitterSprite]
            , init      = (newinit size Void)
            , animation = killobject
            , collide   = newcollide
            }
    = obj
where
    newinit size state subcode pos time gs
        # (dx, gs) = 
            case subcode of
                0         -> (0, gs)
                1         -> (W / 2, gs)
                otherwise -> (IRRnd (W * subcode) gs)
        # pos = {pos & x = pos.x + dx}
        # (objrec, gs) = defaultObjectRec subcode pos size time gs
        # objrec = { objrec
                   & pos            = { x = pos.x + ((BLOCK_SIZE.w - size.w) / 2)
                                      , y = pos.y + (BLOCK_SIZE.h - size.h)
                                      }
                   , ownbounds      = BND_POWER_UP
                   , bouncebounds   = BND_STATIC_BOUNDS
                   , collidebounds  = BND_MAIN_CHARACTER
                   , layer          = AtLayer LYR_FOREGROUND
                   , forgetdistance = NEVER_FORGET
                   }
        = {st=state, or=objrec, gs=gs}
        
    newcollide bnds objtype objrec {st, or, gs}
        | objtype == OBJ_BIRD
            | (isMember objecttype ([OBJ_FALLING_PEPERNOOT, OBJ_LETTER, OBJ_HEART, OBJ_LIFE] ++ KadoList) &&
               (objrec.speed.ry < 0.0)) 
                # or = {or & speed = {objrec.speed & ry = objrec.speed.ry}
                           , pos.x = objrec.pos.x + (objrec.size.w - or.size.w) / 2}
                = {st=st, or=or, gs=gs}
            = {st=st, or=or, gs=gs}
        | objtype == OBJ_MAIN_CHAR
            # points = itemscore objecttype
            # gs = addscore points gs
            # gs = itemsound objecttype gs
            # or = { or 
                   & currentsprite         = if (objecttype == OBJ_LETTER) 5 2
                   , options.removemapcode = True
                   , layer                 = AtLayer LYR_GLITTER
                   , ownbounds             = 0
                   , collidebounds         = 0
                   , offset                = { x = (size.w - ITEM_SIZE.w) / 2
                                             , y = (size.h - ITEM_SIZE.h) / 2
                                             }
                   }
            = {st=st, or=or, gs=gs}
        = {st=st, or=or, gs=gs}
    where
        itemscore tp
            | isMember tp [OBJ_STATIC_PEPERNOOT, OBJ_FALLING_PEPERNOOT]
                = 50
            | isMember tp KadoList
                = 250
            | isMember tp [OBJ_HEART]
                = 500
            | isMember tp [OBJ_LIFE]
                = 750
            = 150
                
        itemsound tp gs
            # (pan, gs) = RandomPan gs
            # vol = HighVolume
            | isMember tp [OBJ_STATIC_PEPERNOOT, OBJ_FALLING_PEPERNOOT]
                # instr = SND_PPN
                # vol = DefaultVolume
                # (_, gs) = playSoundSample instr vol pan (getnotefreq 69) 0 gs
                # (_, gs) = playSoundSample instr vol pan (getnotefreq 80) 3 gs
                = gs
            | isMember tp KadoList
                # instr = SND_FLUTE
                # (_, gs) = playSoundSample instr vol pan (getnotefreq 104) 0 gs
                # (_, gs) = playSoundSample instr vol pan (getnotefreq 108) 2 gs
                # (_, gs) = playSoundSample instr vol pan (getnotefreq 111) 4 gs
                = gs
            | isMember tp [OBJ_HEART]
                # ins = SND_XYLOFOON
                
                /* Sinterklaas Kapoentje... */
                # gs = PlaySong ins vol pan 106 0 [(7,16),(7,8),(9,16),(9,8),(7,24),(4,24)] gs
                # gs = PlaySong ins vol pan 106 0 [(4,16),(4,8),(5,16),(5,8),(4,24),(0,24)] gs
                = gs
            | isMember tp [OBJ_LIFE]
                = AddLife gs
            # instr = SND_XYLOFOON
            # (_, gs) = playSoundSample instr vol pan (getnotefreq 127) 0 gs
            # (_, gs) = playSoundSample instr vol pan (getnotefreq 130) 3 gs
            # (_, gs) = playSoundSample instr vol pan (getnotefreq 134) 3 gs
            = gs


killobject {st, or, gs}
    = {st = st, or = {or & active = False}, gs = gs}

AddLife gs
    # (pan, gs) = RandomPan gs
    # vol = HighVolume
    # ins = SND_FLUTE
    # gs = PlaySong ins vol pan 100 0 [(-4,5),(0,5),(3,5),(8,5),(12,5),(15,5),(20,5)] gs
    = inclives gs


Beep gs
    # (_, gs) = playSoundSample SND_PPN DefaultVolume PAN_CENTER (getnotefreq 69) 0 gs
    = gs

PlaySong instr vol pan base delay [] gs = gs
PlaySong instr vol pan base delay [(note,duration):ls] gs
    # (_, gs) = playSoundSample instr vol pan (getnotefreq (base + note)) delay gs
    = PlaySong instr vol pan base (delay + duration) ls gs


/* ---------- textitems objects ---------- */

ST_X        :==  10
ST_COLON    :==  11
ST_PPN      :==  12
ST_TIME     :==  13

:: StatState
   = { skyr  :: !Int
     , skyg  :: !Int
     , skyb  :: !Int
     , addr  :: !Int
     , addg  :: !Int
     , addb  :: !Int
     }

StatHeartObject
    # obj = defaultGameObject OBJ_STAT size state
    # obj = { obj
            & sprites   = [StatusSprite 1, StatusSprite 2, StatusSprite 3,
                           StatusSprite 4, StatusSprite 5, StatusSprite 6]
            , init      = (newinit size state)
            , userevent = newuserevent
            }
    = obj
where
    size    = {w = 12, h = 12}

    state   = { skyr = 0
              , skyg = 0
              , skyb = 0
              , addr = 2
              , addg = 2
              , addb = 2
              }

    newinit size state subcode pos time gs
        # (objrec, gs) = defaultObjectRec subcode pos size time gs
        # objrec = { objrec
                   & options = {objrec.options & static = True}
                   , layer = AtLayer LYR_STATUS
                   , currentsprite = if (subcode >= ST_X) (subcode - 7) 2
                   , ownbounds = BND_STAT
                   }
        # (lev, gs) = getlevel gs
        # rgb = BackGroundColor lev
        # state = 
            {state & skyr = (fromRGB rgb).r
                   , skyg = (fromRGB rgb).g
                   , skyb = (fromRGB rgb).b
            }
        = {st=state, or=objrec, gs=gs}
    where
        fromRGB (RGB rgb) = rgb

    newuserevent ev evpar1 evpar2 {st, or, gs}
        | (ev == EV_POS) && (or.subcode == ST_COLON) && (or.pos.x < 160)
            # or = {or & pos.x = or.pos.x + evpar1}
            = {st=st, or=or, gs=gs}
        | (ev == EV_TIMER) && (or.subcode == ST_TIME)
            # (_, gs) = createUserGameEvent EV_TIMER
                        0 0 (BoundType BND_STAT) ST_TIME (FPS) gs
            # (st, gs) = tick st gs
            = {st=st, or=or, gs=gs}
        | (ev == EV_HEALTH) && (or.subcode < 10)
            # or = {or & currentsprite = if (evpar1 < or.subcode) 1 2}
            = {st=st, or=or, gs=gs}
        = {st=st, or=or, gs=gs}


/* ---------- ending of the level ---------- */

EndingObject
    # obj = defaultGameObject OBJ_ENDING size Void
    # obj = { obj
            & sprites   = []
            , init      = (newinit size Void)
            , collide   = newcollide
            , userevent = newuserevent
            }
    = obj
where
    size    = {w = 6 * 20, h = 40}

    newinit size state subcode pos time gs
        # pos = {pos & y = pos.y + 20}
        # (objrec, gs) = defaultObjectRec subcode pos size time gs
        # objrec = { objrec
                   & layer          = AtLayer LYR_FOREGROUND
                   , ownbounds      = BND_ENDING
                   , collidebounds  = 0
                   , currentsprite  = 0
                   , forgetdistance = {x = 12, y = 12}

                   }
        = {st=state, or=objrec, gs=gs}

    newcollide bnds othertype otherobjrec {st, or, gs}
        = {st=st, or = {or & collidebounds = 0}, gs=gs}

    newuserevent ev evpar1 evpar2 {st, or, gs}
        | ev == EV_ALL_DONE
            = {st=st, or = {or & collidebounds = BND_MAIN_CHARACTER}, gs=gs}
        = {st=st, or=or, gs=gs}


/* ---------- car ---------- */

:: CarState
   = { sound :: Int
     , count :: Int
     }

CarObject
    # obj = defaultGameObject OBJ_CAR size state
    # obj = { obj
            & sprites   = [CarSprite]
            , init      = (newinit size state)
            , move      = newmove
            }
    = obj
where
    state   = { sound = 0, count = 0 }
    size    = {w = 80 - 8, h = 50 - 20}

    newinit size state subcode pos time gs
        # pos = {pos & y = pos.y - size.h + BLOCK_SIZE.h}
        # (objrec, gs) = defaultObjectRec subcode pos size time gs
        # objrec = { objrec
                   & layer                 = AtLayer LYR_FOREGROUND
                   , ownbounds             = BND_HURT
                   , collidebounds         = 0
                   , options.removemapcode = True
                   , speed                 = {rx = -4.0, ry = 0.0}
                   , skipmove              = 0
                   , subcode               = fromDirectionSet {left = True, 
                                                right = True, top = True, bottom = True}
                   , offset                = {x = -5, y = -20}
                   }
        # (_, gs) = playSoundSample SND_FLUTE2 HighVolume PAN_RIGHT 56000 0 gs
        # (_, gs) = playSoundSample SND_FLUTE2 HighVolume PAN_RIGHT 56000 6 gs
        # (_, gs) = playSoundSample SND_FLUTE2 HighVolume PAN_RIGHT 56000 1 gs
        # (_, gs) = playSoundSample SND_FLUTE2 HighVolume PAN_RIGHT 56000 7 gs
        # (_, gs) = playSoundSample SND_FLUTE2 HighVolume PAN_RIGHT 56000 8 gs
        = {st=state, or=objrec, gs=gs}

    newmove {st, or, gs}
        # (freq, gs) = IRRnd 60 gs
        # st = {st & sound = st.sound + freq + 25, count = st.count + 1}
        # freq = freq + st.sound
        # x = ((80 - st.count) - 40)
        # (rnd, gs) = IRRnd ((abs x) + 10) gs
        | (abs rnd) < 2
            # (_, gs) = playSoundSample SND_PLOF LowVolume (x * PAN_RIGHT / 45)
                              (5500 + freq) 13 gs
            = {st=st, or=or, gs=gs}
        | freq > 1000
            = {st = {st & sound = 0}, or=or, gs=gs}
        = {st=st, or=or, gs=gs}


/* ---------- flits ---------- */

FlitsObject
    # obj = defaultGameObject OBJ_FLITS size Void
    # obj = { obj 
            & sprites   = [FlitsSprite]
            , init      = (newinit size Void)
            , animation = killobject
            }
    = obj
where
    size    = {w = 24, h = 20}

    newinit size state subcode pos time gs
        # (objrec, gs) = defaultObjectRec subcode pos size time gs
        # objrec = { objrec
                   & layer         = AtLayer LYR_INFRONT
                   }
        = {st=state, or=objrec, gs=gs}



/* ---------- flying enemies ---------- */

BirdObject = FlyingObject OBJ_BIRD [BirdSprite 0, BirdSprite 1]

FlyingObject objtype sprlist
    # obj = defaultGameObject objtype size Void
    # obj = { obj
            & sprites = sprlist
            , init    = (newinit size Void)
            , collide = newcollide
            , move    = newmove
            }
    = obj  
where
    size    = {w = 20, h = 16}

    newinit size state subcode pos time gs
        # (objrec, gs) = defaultObjectRec subcode pos size time gs
        # objrec = { objrec
                   & offset         = {x = 0, y = -3}
                   , speed          = {rx = -0.5, ry = 0.0}
                   , bounce         = {fvx = Factor 1.0, fvy = Factor 1.0} 
                   , layer          = AtLayer (LYR_FOREGROUND + 1)
                   , options        = { objrec.options
                                      & automirrorleftright = True
                                      }
                   , ownbounds      = BND_ENEMY
                   , bouncebounds   = BND_STATIC_BOUNDS + BND_ENEMY + BND_MAP_CODES
                   , collidebounds  = BND_MAIN_CHARACTER
                   , currentsprite  = (1 + subcode)
                   , forgetdistance = {x = 6, y = 4}
                   , skipmove       = 0
                   } 
        = {st=state, or=objrec, gs=gs}

    newcollide bnds othertype otherobjrec {st, or, gs}
        | ((othertype == OBJ_MAIN_CHAR) && (bnds.bottom))
            # (freq, gs) = IRRnd 1000 gs
            # (_, gs) = playSoundSample SND_BIRD LowVolume PAN_CENTER 
                            ((getnotefreq 100) + freq) 0 gs
            # (_, gs) = playSoundSample SND_FLUTE LowVolume PAN_CENTER
                                          (90000 + freq * 2) 1 gs
            # (_, gs) = playSoundSample SND_FLUTE LowVolume PAN_CENTER
                                          (45000 + freq) 0 gs
            = {st=st, or = kill or, gs=gs}
        = {st=st, or=or, gs=gs}

    newmove {st, or, gs}
        # (turn, gs) = IRnd 30 gs
        # (xadd, gs) = RRnd 0.05 gs
        # (yadd, gs) = RRnd 0.085 gs
        # (skmv, gs) = IRnd 25 gs
        # rxv = (if (turn == 1) (~or.speed.rx) (or.speed.rx)) + xadd
        # ryv = or.speed.ry + yadd + 0.005
        # or = {or & skipmove = skmv, speed = {rx=rxv, ry=ryv}}
        = {st=st, or=or, gs=gs}



kill :: GameObjectRec -> GameObjectRec
kill or =
    {or & displayoptions.mirrorupdown = True
        , acceleration = {rx = 0.0, ry = 1.0 / 16.0}
        , speed = {rx = ~(or.speed.rx / 2.0), ry = -3.0}
        , ownbounds = 0
        , bouncebounds = 0
        , collidebounds = 0
        , forgetdistance = {x = 1, y = 1}
        , layer = InFront
        , options.removemapcode = True
        }



/* ---------- tramp / bounce block ---------- */

TrampObject = BounceObject OBJ_TRAMP False 4 (-2) 4

BounceBlockObject = BounceObject OBJ_BOUNCEBLOCK True 1 4 (ITEM_SIZE.h)

BounceObject objnr isLamp base rely hght
    # obj = defaultGameObject objnr size Void
    # obj = { obj
            & sprites   = [ {(LampSprite 0) & loop = False},
                            {(LampSprite 1) & loop = False},
                              LampSprite 2,
                              TrampSprite
                          ]
            , init      = (newinit size Void)
            , collide   = newcollide
            , animation = newanimation
            }
    = obj
where
    size    = {w = ITEM_SIZE.w, h = hght}

    newinit size state subcode pos time gs
        # pos = {pos & y = pos.y + rely}
        # (time, gs) = GetTime gs
        # (bonus, gs) = getbonus gs
        # (objrec, gs) = defaultObjectRec subcode pos size time gs
        # objrec = { objrec
                   & layer           = AtLayer LYR_FOREGROUND
                   , speed           = {rx = ~(toReal subcode), ry = 0.0}
                   , bounce          = {fvx = Factor 1.0, fvy = Value 0.0}
                   , forgetdistance  = {x = 5 + 15 * subcode, y = 5} 
                   , ownbounds       = BND_BLOCKS
                   , collidebounds   = BND_MAIN_CHARACTER
                   , bouncebounds    = BND_STATIC_BOUNDS
                   , currentsprite   = if (isLamp) (if ((not bonus) && (time < 30)) 3 1) base
                   }
        = {st=state, or=objrec, gs=gs}

    newcollide bnds othertype otherobjrec {st, or, gs}
        | othertype == OBJ_MAIN_CHAR
            | (bnds.top || bnds.bottom)
                = {st=st, or = {or & offset.y = (if bnds.top 4 (-4))
                                   , currentsprite = if isLamp 2 base}, gs=gs}
            = {st=st, or=or, gs=gs}
        = {st=st, or=or, gs=gs}

    newanimation {st, or, gs}
        | (or.offset.y == 0)
            = {st=st, or = {or & currentsprite = base}, gs=gs}
        # or = {or & offset.y = ~(decr or.offset.y)}
        | not isLamp
            = {st=st, or=or, gs=gs}
        # (time, gs) = GetTime gs
        # (bonus, gs) = getbonus gs
        | (or.currentsprite == 1) && (time < 30) && (time > 0) && (not bonus)
            = {st=st, or = {or & currentsprite = 3}, gs=gs}
        = {st=st, or=or, gs=gs}
    where
        decr x
            | x < 0     = x + 1
            | otherwise = x - 1 


/* ---------- trail ---------- */

KadoObject n
    # obj = defaultGameObject (OBJ_KADO1 + n - 1) size Void
    # obj = { obj
            & sprites    = [KadoSprite n]
            , init       = (newinit size Void)
            , userevent  = newuserevent
            }
    = obj
where
    size = ITEM_SIZE
    
    newinit size state subcode pos time gs
        # (objrec, gs) = defaultObjectRec subcode pos size time gs
        # objrec = { objrec
                   & ownbounds      = BND_KADO
                   , bouncebounds   = 0
                   , collidebounds  = 0
                   , layer          = AtLayer (LYR_KADO - subcode)
                   , forgetdistance = {x = 10, y = 10}
                   }
        = {st=state, or=objrec, gs=gs}

    newuserevent ev evpar1 evpar2 {st, or, gs}
        | ev == (EV_POS + or.subcode)
            = {st=st, or = {or & pos = {x = evpar1, y = evpar2}}, gs=gs}
        | ev == EV_KADO_DROPPED
            | or.subcode == 0
                = {st=st, or = {or & active = False}, gs=gs}
            = {st=st, or = {or & subcode = or.subcode - 1, 
                          layer = AtLayer ((getlayer or.layer) + 1)}, gs=gs}
        = {st=st, or=or, gs=gs}

getlayer :: LayerPosition -> Int
getlayer InFront     = 0
getlayer (AtLayer n) = n


/* ---------- main character ---------- */

MC_IDLE  :==  1
MC_WALK  :==  2
MC_JUMP  :==  3
MC_FALL  :==  4
MC_FFST  :==  5

MC_TURN  :==  6
MC_DEAD  :==  7

TRAILXMIN   :== -20 - 4
TRAILXMAX   :==  32 - 4

TRAILTURNSPEED :==   2




:: MainCharState
   = { action      :: !Int
     , keyLeft     :: !Bool
     , keyRight    :: !Bool
     , lastpos1    :: !Point2
     , lastpos2    :: !Point2
     , lastspeed1  :: !RealXY
     , lastspeed2  :: !RealXY
     , lasthdir    :: !HDirection
     , turning     :: !Bool
     , traildelta  :: !Int
     , trail       :: [!Int]
     , lastypos    :: !Int
     , readytodrop :: !Bool
     , pepernoten  :: !Int
     , letters     :: !Int
     , health      :: !Int
     , enemynote   :: !Int
     , todo        :: !Int
     , extra       :: !Int
     , birdskilled :: !Int
     }

MainCharObject
    # obj = defaultGameObject OBJ_MAIN_CHAR size newstate
    # obj = { obj
            & sprites    = [ SintIdleSprite
                           , SintWalkSprite
                           , SintJumpSprite
                           , SintFallSprite
                           , SintFFstSprite

                           , SintTurnSprite
                           , SintDeadSprite
                           ]
            , init       = (newinit size newstate)
            , move       = newmove
            , animation  = newanimation
            , keydown    = newkeydown
            , keyup      = newkeyup
            , collide    = newcollide
            , userevent  = newuserevent
            }
    = obj
where
    size     = {w = 24, h = 38}
    defofs   = {x = -4, y = -9}

    ac = 1.0 / 5.0
    
    defmaxspeed = {rx = 2.0, ry = 6.0}
    
    newstate = { action       = MC_IDLE
               , keyLeft      = False
               , keyRight     = False
               , lastpos1     = zero
               , lastpos2     = zero
               , lastspeed1   = zero
               , lastspeed2   = zero
               , lasthdir     = DirLeft
               , turning      = False
               , traildelta   = 0
               , trail        = []
               , lastypos     = 0
               , readytodrop  = True
               , pepernoten   = 0
               , letters      = 0
               , health       = 3
               , enemynote    = 0
               , todo         = 0
               , extra        = 0
               , birdskilled  = 0
               }

    newinit size state subcode pos time gs
        # pos = {x = pos.x + W / 2, y = pos.y + H - size.h}
        # (objrec, gs) = defaultObjectRec subcode pos size time gs
        # objrec = { objrec
                   & offset         = defofs
                   , acceleration   = {rx = 0.0, ry = 1.0 / 12.0}
                   , bounce         = {fvx = Value 0.0, fvy = Factor (1.0 / 16.0)} 
                   , maxspeed       = defmaxspeed
                   , slowdown       = {fvx = Factor (1.0 / 16.0), fvy = Value 0.0}
                   , layer          = AtLayer LYR_PLAYER
                   , options        = { objrec.options
                                      & checkkeyboard       = True
                                      , allowkeyboardrepeat = False
                                      , automirrorleftright = True
                                      }
                   , ownbounds      = BND_MAIN_CHARACTER
                   , bouncebounds   = BND_STATIC_BOUNDS
                   , collidebounds  = BND_ENEMY + BND_POWER_UP + BND_BLOCKS + BND_CHIMNEY
                                         + BND_ENDING + BND_HURT
                   , forgetdistance = NEVER_FORGET
                   , skipmove       = 0
                   }
        # (_, gs)  = createObjectFocus
                         { scrollleft      = 132
                         , scrollup        =  40
                         , scrollright     = 132
                         , scrolldown      =  42
                         , maxxscrollspeed =   2
                         , maxyscrollspeed =   3
                         } gs
        # (bonus, gs) = getbonus gs
        # state = { state & todo = if bonus 0 TODO
                          , extra = if bonus (-1) EXTRA_PPN
                  }
        = {st=state, or=objrec, gs=gs}


    newmove {st, or, gs}
        # newy     = (st.lastypos + or.pos.y + 1) / 2
        # st       = {st & traildelta = newdelta st.traildelta or.options.hdirection
                         , lastypos = newy}
        # trailpos = {x = or.pos.x + st.traildelta, y = newy + or.size.h - ITEM_SIZE.h - 1}
        # gs       = broadcastposition (length st.trail) trailpos.x trailpos.y 
                        (st.trail ++ [0]) gs
        | st.turning
            | (same st.lasthdir or.options.hdirection)
                = {st=st, or=or, gs=gs}
            = {st = {st & turning = False}, or = {or & currentsprite = MC_TURN}, gs=gs}
        = {st=st, or=or, gs=gs}
    where
        broadcastposition :: !Int !Int !Int [!Int] !(GSt gs) -> (GSt gs)
        broadcastposition n x y [] gs = gs
        broadcastposition n x y [l:ls] gs
            # (_, gs) = createUserGameEvent (EV_POS + n)
                        x y (BoundType BND_KADO) n (n * 11 + 4) gs
            = broadcastposition (n - 1) x y ls gs 

        same DirRight DirRight = True
        same DirLeft  DirLeft  = True
        same _ _               = False

        newdelta x DirLeft
            | x < TRAILXMAX    = x + TRAILTURNSPEED
            | otherwise        = TRAILXMAX
        newdelta x DirRight
            | x > TRAILXMIN    = x - TRAILTURNSPEED
            | otherwise        = TRAILXMIN

    newanimation {st, or, gs}
        # act = st.action
        # xstuck = ((or.pos.x == st.lastpos1.x) &&
                    (or.pos.x == st.lastpos2.x) &&
                    ((toInt or.speed.rx) == 0)
                   )
        # ystuck = ((or.pos.y == st.lastpos1.y) &&
                    (or.pos.y == st.lastpos2.y) &&
                    ((toInt or.speed.ry) == 0)
                   )
        # oldact = act
        # act = case act of
            MC_IDLE   -> if (toInt (or.speed.rx / 1.25) == 0)
                             (if (or.speed.ry > sp) MC_FALL MC_IDLE)
                             (if (or.speed.ry > sp) MC_FALL MC_WALK)
            MC_WALK   -> if (toInt (or.speed.rx / 1.25) == 0)
                             (if (or.speed.ry > sp) MC_FALL MC_IDLE)
                             (if (or.speed.ry > sp) MC_FALL MC_WALK)
            MC_JUMP   -> if (or.speed.ry > sp) MC_FALL
                              (if (xstuck && ystuck && (or.speed.ry == st.lastspeed2.ry))
                                  MC_IDLE
                                  MC_JUMP)
            MC_FALL   -> if (or.speed.ry > sp) MC_FALL MC_WALK
            otherwise -> MC_IDLE
        # st = {st & lastpos2 = st.lastpos1, lastspeed2 = st.lastspeed1}
        # st = {st & lastpos1 = or.pos, lastspeed1 = or.speed}
        # newsprite = if (or.speed.ry > 3.0) MC_FFST act
//        # newsprite = if (st.turning && (toInt or.speed.ry) == 0) MC_TURN act

        # st = {st & action = act}
        # or = {or & currentsprite = newsprite}
        | (act == MC_WALK) && (oldact == MC_FALL)
            # (_, gs) = playSoundSample SND_PLOF LowVolume
                            PAN_CENTER DEFAULT_FREQUENCY 0 gs
            # st = {st & enemynote = 0}
            | st.lastspeed2.ry == defmaxspeed.ry
                = hurt {st=st, or = {or & speed = zero}, gs=gs}
            = {st=st, or=or, gs=gs}
        = {st=st, or=or, gs=gs}
    where
        sp = 1.0 / 4.0
        
    newkeydown key {st, or, gs}
        # action = st.action
        | key == GK_LEFT
            # st = {st & keyLeft = True}
            # newaction = if (action == MC_IDLE) MC_WALK action
            # st = {st & action = newaction}
            # or = {or & acceleration.rx = or.acceleration.rx - ac, currentsprite = newaction}
            | (newaction == MC_WALK) && (or.speed.rx > 0.0)
                = {st = {st & turning = True, lasthdir = DirRight}, or=or, gs=gs}
            = {st=st, or=or, gs=gs}
        | key == GK_RIGHT
            # st = {st & keyRight = True}
            # newaction = if (action == MC_IDLE) MC_WALK action
            # st = {st & action = newaction}
            # or = {or & acceleration.rx = or.acceleration.rx + ac, currentsprite = newaction}
            | (newaction == MC_WALK) && (or.speed.rx < 0.0)
                = {st = {st & turning = True, lasthdir = DirLeft}, or=or, gs=gs}
            = {st=st, or=or, gs=gs}
        | key == GK_SPACE
            | (isMember action [MC_IDLE, MC_WALK, MC_TURN])
                # (_, gs) = playSoundSample SND_JUMP DefaultVolume
                                PAN_CENTER DEFAULT_FREQUENCY 0 gs
                # act = action
                // *** play sound
                # st = {st & action = MC_JUMP}
                # or = {or & speed = jumpspeed or.speed
                                   , currentsprite = MC_JUMP
                                   , maxspeed = defmaxspeed
                                   }
                = {st=st, or=or, gs=gs}
            = {st=st, or=or, gs=gs} 
        | otherwise = {st=st, or=or, gs=gs}
    where
        jumpspeed sp=:{rx, ry} = {rx = rx, ry = ry - 3.25 - (abs rx) / 3.0}

    newkeyup key {st, or, gs}
        # action = st.action
        | (key == GK_LEFT) && (st.keyLeft)
            # st = {st & keyLeft = False}
            = {st=st, or = {or & acceleration.rx = or.acceleration.rx + ac}, gs=gs}
        | (key == GK_RIGHT) && (st.keyRight)
            # st = {st & keyRight = False}
            = {st=st, or = {or & acceleration.rx = or.acceleration.rx - ac}, gs=gs}
        | (key == GK_SPACE) && (st.action == MC_JUMP) && (or.speed.ry < (-0.5))
            = {st=st, or = {or & speed = {or.speed & ry = or.speed.ry * 3.0 / 4.0}}, gs=gs}
        | otherwise
            = {st=st, or=or, gs=gs}


    newcollide bnds othertype otherobjrec {st, or, gs}
        | (isMember othertype ChimneyList) && (or.speed.ry >= 0.0) && 
                (or.pos.y + or.size.h <= otherobjrec.pos.y + otherobjrec.size.h / 2 + 4) &&
                (st.readytodrop)
            # chimneytype = othertype - OBJ_CHIMNEY1 + 1
            # first = st.trail ++ [(-1)]
            | (first!!0 == chimneytype)
                # instr       = SND_FLUTE
                # vol         = HighVolume
                # (_, gs)     = playSoundSample instr vol PAN_CENTER (getnotefreq 100)  0 gs
                # (_, gs)     = playSoundSample instr vol PAN_CENTER (getnotefreq 101)  8 gs  
                # (_, gs)     = playSoundSample instr vol PAN_CENTER (getnotefreq 100) 16 gs
                # (_, gs)     = playSoundSample instr vol PAN_CENTER (getnotefreq  96)  0 gs  
                # (_, gs)     = playSoundSample instr vol PAN_CENTER (getnotefreq  98)  8 gs  
                # (_, gs)     = playSoundSample instr vol PAN_CENTER (getnotefreq  96) 16 gs
                # (_, gs)     = createUserGameEvent EV_READY 0 0 Self ANY_SUBTYPE (32) gs
                # (_, gs)     = createUserGameEvent EV_KADO_DROPPED chimneytype 0
                                    (BoundType (BND_KADO + BND_CHIMNEY)) ANY_SUBTYPE 0 gs
                # st          = {st & trail = drop 1 st.trail, readytodrop = False}
                = checkalldone {st=st, or=or, gs=gs}
            | (not (isMember otherobjrec.currentsprite [1, 7])) && (st.pepernoten > 0)
                # instr       = SND_FLUTE
                # vol         = HighVolume
                # (_, gs)     = playSoundSample instr vol PAN_CENTER (getnotefreq 112)  0 gs  
                # (_, gs)     = playSoundSample instr vol PAN_CENTER (getnotefreq 113)  8 gs  
                # (_, gs)     = playSoundSample instr vol PAN_CENTER (getnotefreq 112) 16 gs
                # (_, gs)     = playSoundSample instr vol PAN_CENTER (getnotefreq 108)  0 gs  
                # (_, gs)     = playSoundSample instr vol PAN_CENTER (getnotefreq 110)  8 gs  
                # (_, gs)     = playSoundSample instr vol PAN_CENTER (getnotefreq 108) 16 gs
                # (_, gs)     = createUserGameEvent EV_READY 0 0 Self ANY_SUBTYPE (32) gs
                # (_, gs)     = createUserGameEvent EV_PEPERNOOT chimneytype 0
                                    (BoundType BND_CHIMNEY) ANY_SUBTYPE 0 gs
                # st          = {st & pepernoten = st.pepernoten - 1, readytodrop = False}
                # gs          = setppn st.pepernoten gs
                = checkalldone {st=st, or=or, gs=gs}
            = {st=st, or=or, gs=gs}
        | othertype == OBJ_LETTER
            # st = {st & letters = st.letters + 1}
            | st.letters == 4
                # (_, gs) = createUserGameEvent EV_HEALTH 3 0 
                              (BoundType BND_STAT) ANY_SUBTYPE 1 gs
                = {st = {st & health = 3}, or = or, gs = AddLife gs}
            = {st=st, or=or, gs=gs}
        | othertype == OBJ_BOUNCEBLOCK
            | bnds.top
                # (_, gs) = playSoundSample SND_WJUMP DefaultVolume
                              PAN_CENTER 40000 0 gs
                = {st = {st & action = MC_JUMP}, 
                   or = {or & speed.ry = ~(abs or.speed.ry) * 2.0 - 4.25, currentsprite = MC_JUMP}, gs=gs}
            | bnds.bottom
                # (_, gs) = playSoundSample SND_WJUMP DefaultVolume
                              PAN_CENTER 35000 0 gs
                = {st = {st & action = MC_FALL}, 
                   or = {or & speed.ry = or.speed.ry + 2.5, currentsprite = MC_FALL}, gs = gs}
            = {st=st, or=or, gs=gs}
        | othertype == OBJ_TRAMP
            | bnds.top
                # (_, gs) = playSoundSample SND_WJUMP DefaultVolume
                              PAN_CENTER 70000 0 gs
                = {st = {st & action = MC_JUMP}, 
                   or = {or & speed.ry = ~(abs or.speed.ry) * 1.5 - 2.0, currentsprite = MC_JUMP}, gs = gs}
            = {st=st, or=or, gs=gs}
        | othertype == OBJ_HEART
            | not (st.health < 3)
                = {st=st, or=or, gs=gs}
            # st = {st & health = st.health + 1}
            # (_, gs) = createUserGameEvent EV_HEALTH st.health 0
                          (BoundType BND_STAT) ANY_SUBTYPE 1 gs
            = {st=st, or=or, gs=gs}
        | isMember othertype [OBJ_FALLING_PEPERNOOT, OBJ_STATIC_PEPERNOOT]
            # st = {st & pepernoten = st.pepernoten + 1}
            = {st=st, or=or, gs = setppn st.pepernoten gs}
        | isMember othertype KadoList
            # kadotype = othertype - OBJ_FALLING_KADO1 + 1
            # (_, gs)  = createNewGameObject (OBJ_KADO1 + kadotype - 1)
                          (length st.trail)
                          {x = or.pos.x + st.traildelta, 
                           y = or.pos.y + or.size.h - ITEM_SIZE.h - 1} gs
            # st = {st & trail = st.trail ++ [kadotype]}
            = {st=st, or=or, gs=gs}

        | othertype == OBJ_ENDING
            | st.todo == 0
                # or = {or & acceleration.rx = ac
                           , collidebounds = BND_POWER_UP
                           , options = {or.options & checkkeyboard = False
                                                   , ignorelevelbounds = True}}
                # (_, gs) = createUserGameEvent EV_STOP_MOVING 0 0 Self ANY_SUBTYPE 125 gs
                # (_, gs) = createObjectFocus {zero & scrollright = 160
                                                    , maxxscrollspeed = 1} gs
                # gs      = setexitcode EC_SUCCESS gs
                # (t, gs) = GetTime gs
                # t       = t * 3
                # p       = st.pepernoten * 10
                # (_, gs) = createUserGameEvent EV_COUNT_SCORE
                                0 0 Self ANY_SUBTYPE 350 gs
                # (_, gs) = createUserGameEvent EV_PPN_SCORE
                                0 0 Self ANY_SUBTYPE (500 + t) gs
                # (_, gs) = createUserGameEvent EV_ADD_BIRD_SCORE
                                0 0 Self ANY_SUBTYPE (600 + t + p) gs
                # (_, gs) = createUserGameEvent EV_QUIT_LEVEL
                                EC_SUCCESS 0 Self ANY_SUBTYPE (850 + t + p) gs
                # gs = dagsint SND_FLUTE 0 gs
                = {st=st, or=or, gs=gs}
            = {st=st, or=or, gs = msgnotready gs}
        | othertype == OBJ_HEART
            | not (st.health < 3)
                = {st=st, or=or, gs=gs}
            # st = {st & health = st.health + 1}
            # (_, gs) = createUserGameEvent EV_HEALTH st.health 0
                          (BoundType BND_STAT) ANY_SUBTYPE 1 gs
            = {st=st, or=or, gs=gs}
        | (not (otherobjrec.ownbounds bitand BND_HURT == 0))
            | or.displayoptions.blink
                = {st=st, or=or, gs=gs}
            | (not (otherobjrec.subcode bitand (fromDirectionSet bnds) == 0))
                = hurt {st=st, or=or, gs=gs}
            = {st=st, or=or, gs=gs}
        | bnds.top
            | (not (otherobjrec.ownbounds bitand BND_ENEMY == 0))
                # st = {st & birdskilled = st.birdskilled + 1}
                # gs = addscore (-500) gs
                # (_, gs) = playSoundSample SND_HIT HighVolume
                              PAN_CENTER (getnotefreq
                              (MIDDLE_C + 76 + 2 * st.enemynote)) 0 gs
                # st = {st & enemynote = st.enemynote + 1}
                # pos = {x = (or.pos.x + otherobjrec.pos.x) / 2, y = or.pos.y + 20} 
                # (_, gs) = createNewGameObject OBJ_FLITS 0 pos gs
                # st = {st & action = MC_JUMP}
                # or = {or & speed.ry = -2.5, currentsprite = MC_JUMP}
                = {st=st, or=or, gs=gs}
            = {st=st, or=or, gs=gs}

        | (not (otherobjrec.ownbounds bitand BND_ENEMY == 0))
            = hurt {st=st, or=or, gs=gs}
        = {st=st, or=or, gs=gs}
    where
        checkalldone {st, or, gs}
            # st = {st & todo = st.todo - 1}
            | st.todo == 0
                # (_, gs) = createUserGameEvent EV_ALL_DONE 0 0 
                                  (BoundType BND_ENDING) ANY_SUBTYPE 0 gs
                # gs = dagsint SND_XYLOFOON 25 gs
                = {st=st, or=or, gs = msgready gs}
            = {st=st, or=or, gs=gs}
            
        dagsint instr delay gs
            # vol = HighVolume
            # pan = PAN_CENTER
            
            /* Dag Sinterklaasje... */
            # gs = PlaySong instr vol pan 106 delay
                     [(7,24),(4,16),(2,8),(0,24),(7,24),(9,24),(5,24),(7,24),(4,24)] gs
            # gs = PlaySong instr vol pan 106 delay
                     [(4,24),(0,16),(-5,8),(-8,24),(4,24),(5,24),(2,24),(4,24),(0,24)] gs
            = gs

    newuserevent ev evpar1 evpar2 {st, or, gs}
        | ev == EV_STOP_BLINKING
            = {st=st, or = {or & displayoptions.blink = False}, gs=gs}
        | ev == EV_READY
            = {st = {st & readytodrop = True}, or=or, gs=gs}
        | ev == EV_QUIT_LEVEL
            # gs = setexitcode evpar1 gs
            # gs = quitlevel gs
            = {st=st, or=or, gs=gs}
        | ev == EV_GAME_OVER
            = {st = st, or = or, gs = setgameover gs}
        | ev == EV_TIME_UP
            # st = {st & health = 0}
            = hurt {st=st, or = {or & displayoptions = {or.displayoptions & blink = False}}, gs = gs}
        | ev == EV_STOP_MOVING
            = {st=st, or = {or & speed = zero, acceleration = zero}, gs=gs}
        | ev == EV_ADD_BIRD_SCORE
            | st.birdskilled == 0
                # (_, gs) = playSoundSample SND_PPN DefaultVolume
                            PAN_CENTER (getnotefreq 80) 0 gs
                # (_, gs) = playSoundSample SND_PPN DefaultVolume
                            PAN_CENTER (getnotefreq 99) 1 gs
                = {st=st, or=or, gs = addscore 10000 gs}
            = {st=st, or=or, gs=gs}
        | ev == EV_COUNT_SCORE
            # (t, gs) = GetTime gs
            | t > 0
                # (_, gs) = createUserGameEvent EV_COUNT_SCORE
                                0 0 Self ANY_SUBTYPE 3 gs
                # (_, gs) = dectime gs
                | (or.framecounter rem 1) == 0
                    # (_, gs) = playSoundSample SND_XYLOFOON DefaultVolume
                                  PAN_CENTER (getnotefreq 120) 0 gs
                    = {st=st, or=or, gs = addscore 50 gs}
                = {st=st, or=or, gs=gs}
            = {st=st, or=or, gs=gs}
        | ev == EV_PPN_SCORE
            # (bonus, gs) = getbonus gs
            | st.pepernoten > 0
                # (_, gs) = createUserGameEvent EV_PPN_SCORE
                                0 0 Self ANY_SUBTYPE 10 gs
                # st = {st & pepernoten = st.pepernoten - 1}
                # gs = setppn st.pepernoten gs
                # (_, gs) = playSoundSample SND_FLUTE DefaultVolume
                                  PAN_CENTER (getnotefreq 122) 0 gs
                = {st = {st & extra = st.extra - 1}, or = or, gs = addscore (if bonus 50 250) gs}
            | st.extra == 0
                # (_, gs) = playSoundSample SND_PPN DefaultVolume PAN_CENTER (getnotefreq 80) 10 gs
                # (_, gs) = playSoundSample SND_PPN DefaultVolume PAN_CENTER (getnotefreq 82) 13 gs
                # (_, gs) = playSoundSample SND_PPN DefaultVolume PAN_CENTER (getnotefreq 84) 16 gs
                # (_, gs) = playSoundSample SND_PPN DefaultVolume PAN_CENTER (getnotefreq 85) 19 gs
                # (_, gs) = playSoundSample SND_PPN DefaultVolume PAN_CENTER (getnotefreq 87) 22 gs
                = {st = st, or = or, gs = setbonus True gs}
            = {st=st, or=or, gs=gs}
        = {st=st, or=or, gs=gs}

    hurt {st, or, gs}
        | or.displayoptions.blink
            = {st=st, or=or, gs=gs}
        # (ec, gs) = getexitcode gs
        | isMember ec [EC_SUCCESS, EC_FAILURE]  // player not active anymore?
            = {st=st, or=or, gs=gs}
        # st = {st & health = st.health - 1}
        # (_, gs) = createUserGameEvent EV_HEALTH st.health 0 
                      (BoundType BND_STAT) ANY_SUBTYPE 1 gs
        # (_, gs) = playSoundSample SND_AU HighVolume PAN_CENTER 52000  0 gs
        # (_, gs) = playSoundSample SND_AU HighVolume PAN_CENTER 50000  4 gs
        # (_, gs) = playSoundSample SND_AU HighVolume PAN_CENTER 48000  7 gs
        # (_, gs) = playSoundSample SND_AU HighVolume PAN_CENTER 42000  9 gs
        # (_, gs) = playSoundSample SND_AU HighVolume PAN_CENTER 33000 10 gs
        | st.health == (-1)
            # st = {st & action = MC_DEAD}
            # or = {or & currentsprite = MC_DEAD
                       , speed = {rx = 0.0, ry = -2.25}
                       , acceleration = {rx = 0.0, ry = 1.0 / 24.0}
                       , ownbounds = 0
                       , collidebounds = 0
                       , bouncebounds = 0
                       , layer = InFront
                       , options = {or.options & checkkeyboard = False
                                               , ignorelevelbounds = True}}
            # (_, gs) = createUserGameEvent EV_STOP_MOVING 0 0 Self ANY_SUBTYPE 500 gs
            # (_, gs) = createObjectFocus zero gs
            # (bonus, gs) = getbonus gs
            # resultcode  = if bonus EC_SUCCESS EC_FAILURE   // can't die in a bonus level
            # gs      = setexitcode resultcode gs
            # (_, gs) = createUserGameEvent EV_QUIT_LEVEL resultcode
                                    0 Self ANY_SUBTYPE 600 gs
            # (rnd, gs) = IRRnd 3 gs
            # (_, gs) = playSoundSample SND_WATER LowVolume PAN_CENTER 
                           (getnotefreq (102 + rnd)) 250 gs
            # (l, gs) = getlives gs
            | l == 0
                # (_, gs) = createUserGameEvent EV_GAME_OVER 0 0 Self ANY_SUBTYPE 350 gs
                = {st=st, or=or, gs=gs}
            = {st=st, or=or, gs=gs}
        # (_, gs) = createUserGameEvent EV_STOP_BLINKING 0 0 Self ANY_SUBTYPE 225 gs
        # or = {or & displayoptions.blink = True}
        = {st=st, or=or, gs=gs}



/* ---------- useful functions for objects ---------- */

/* quit the level */

quitlevel gs
    = appGSt setQuit gs
where
    setQuit :: GameState -> GameState
    setQuit gst = {gst & quit = True}


/* set exit code for the level */
setexitcode newcode gs = appGSt (setgstexitcode newcode) gs
where
    setgstexitcode :: Int GameState -> GameState
    setgstexitcode c gst = {gst & exitcode = c}

/* get exitcode */
getexitcode gs = accGSt getgstexitcode gs
where
    getgstexitcode :: GameState -> (Int, GameState)
    getgstexitcode gst = (gst.exitcode, gst)


/* bonus functions */
setbonus b gs = appGSt (setgstbonus b) gs
setgstbonus :: Bool GameState -> GameState
setgstbonus b gst = {gst & bonus = b}

/* get bonus */
getbonus gs = accGSt getgstbonus gs
where
    getgstbonus :: GameState -> (Bool, GameState)
    getgstbonus gst = (gst.bonus, gst)



/* increment the number of lives */
inclives gs = appGSt incgstlives gs
where
    incgstlives :: GameState -> GameState
    incgstlives gst = {gst & lives = gst.lives + 1}

/* get number of lives */
getlives gs = accGSt getgstlives gs
where
    getgstlives :: GameState -> (Int, GameState)
    getgstlives gst = (gst.lives, gst)

/* get number of lives */
GetTime gs = accGSt getgsttime gs
where
    getgsttime :: GameState -> (Int, GameState)
    getgsttime gst = (gst.time, gst)


/* get current level number */
getlevel gs = accGSt getgstlevel gs
where
    getgstlevel :: GameState -> (Int, GameState)
    getgstlevel gst = (gst.curlevel, gst)


/* set background color */
SetBackGroundColor rgb gs
    = accGStTb (OSGameLevelOptions True rgb ESC_QUIT False FADE FADE) gs

/* time tick */
tick st gs
    # (ec, gs) = getexitcode gs
    | isMember ec [EC_SUCCESS, EC_FAILURE]  // player not active anymore?
        = (st, gs)
    # (time, gs) = dectime gs
    # (bonus, gs) = getbonus gs
    | bonus
        | time == 0
            # (_, gs) = createUserGameEvent EV_TIME_UP 0 0
                        (BoundType BND_MAIN_CHARACTER) ANY_SUBTYPE 0 gs
            # (_, gs) = dectime gs
            = (st, gs)
        = (st, gs)
    | time == 60  // de ochtend breekt aan...
        # (rnd, gs) = IRRnd 4 gs
        # (_, gs) = playSoundSample SND_COCK HighVolume PAN_CENTER 
                       (getnotefreq (90 + rnd)) (25 + rnd)  gs
        = (st, msgmorning gs)
    | time > 60
        = (st, gs)
    # light = 60 - time
    # (_, gs) = SetBackGroundColor (RGB {r = crgb (st.skyr + light * st.addr), 
                                         g = crgb (st.skyg + light * st.addg),
                                         b = crgb (st.skyb + light * st.addb)}) gs
    | time == 4
        # (_, gs) = playSoundSample SND_GRIEG HighVolume PAN_CENTER 
                       DEFAULT_FREQUENCY 0 gs
        = (st, gs)
    | time == 2
        # (_, gs) = createNewGameObject OBJ_SUN 0 {x = 144, y = 112} gs
        = (st, gs)
    | time == 0
        # (_, gs) = createUserGameEvent EV_TIME_UP 0 0
                        (BoundType BND_MAIN_CHARACTER) ANY_SUBTYPE 0 gs
        # (_, gs) = dectime gs 
        = (st, gs)
    = (st, gs)

dectime gs 
    # (time, gs) = accGSt (decgsttime 1) gs
    | time == 99  // shift position of ":"
        # (_, gs) = createUserGameEvent EV_POS 2 0 (BoundType BND_STAT) ST_COLON 0 gs
        = (time, gs)
    | time == 9
        # (_, gs) = createUserGameEvent EV_POS 1 0 (BoundType BND_STAT) ST_COLON 0 gs
        = (time, gs)
    = (time, gs)
where
    decgsttime :: Int GameState -> (Int, GameState)
    decgsttime n gst
        # gst = {gst & time = gst.time - n}
        = (gst.time, gst)


setppn n gs = appGSt (setgstppn n) gs
where
    setgstppn :: Int GameState -> GameState
    setgstppn n gst = {gst & ppn = n}

getppn gs = accGSt getgstppn gs
where
    getgstppn :: GameState -> (Int, GameState)
    getgstppn gst = (gst.ppn, gst)



setplayer s gs = appGSt (setgstplayer s) gs
where
    setgstplayer :: String GameState -> GameState
    setgstplayer s gst = {gst & player = s}

getplayer gs = accGSt getgstplayer gs
where
    getgstplayer :: GameState -> (String, GameState)
    getgstplayer gst = (gst.player, gst)



/* add a value to the score */
addscore points gs = appGSt (addgstscore points) gs
where
    addgstscore :: Int GameState -> GameState
    addgstscore points gst
        | gst.score + points < 0
            = {gst & score = 0}
        = {gst & score = gst.score + points}


/* gameover functions */
setgameover gs = appGSt setgstgameover gs
setgstgameover :: GameState -> GameState
setgstgameover gst = {gst & gameover = True}

/* start messages */

msgmorning gs = appGSt gstmsgmorning gs
gstmsgmorning :: GameState -> GameState
gstmsgmorning gst = {gst & morningmsg = START_MSG}

msgbonus gs = appGSt gstmsgbonus gs
gstmsgbonus :: GameState -> GameState
gstmsgbonus gst = {gst & bonusmsg = START_MSG}

msgready gs = appGSt gstmsgready gs
gstmsgready :: GameState -> GameState
gstmsgready gst = {gst & readymsg = START_MSG}

msgnotready gs = appGSt gstmsgnotready gs
gstmsgnotready :: GameState -> GameState
gstmsgnotready gst
    | gst.notreadymsg == STOP_MSG
        = {gst & notreadymsg = START_MSG}
    = gst


/* clip RGB */
crgb :: Int -> Int
crgb n
    | n < 0     = 0
    | n > 255   = 255
    | otherwise = n


/* ---------- definitions of the levels ---------- */


/* ---------- Title screen ---------- */

TitleScreen
  = { blankScreen & layers = [TitleLayer]
                  , objects = [AutoMenuObject]
                  , soundsamples = MenuSoundSampleList
                  , leveloptions.escquit = False
                  , leveloptions.fillbackground = Nothing
                  , music = Just TitleMusic
    }


TitleMusic =
    { musicfile = "sint1.mid"
    , restart   = True
    , continue  = False
    }


TitleLayer
  = { bmp       = TitleBitmap
    , layermap  = [{1}]
    , sequences = []
    , movement  = defaultMovement
    }

TitleBitmap
  = { bitmapname  = "TITEL.BMP"
    , unitsize    = {w = 320, h = 200}
    , dimensions  = (1, 1)
    , transparent = Nothing
    }



/* ---------- Menu object ---------- */

MAX_INPUT_LEN :== 21

:: MenuState
   = { selected :: Int   /* menu options: min ... max */
     , min      :: Int
     , max      :: Int
     , typing   :: Bool
     , story    :: Bool
     , quitting :: Bool
     , naam     :: String
     , phis     :: Bool
     }

AutoMenuObject
    # obj = defaultGameObject OBJ_AUTOINIT size newstate
    # obj = { obj
            & sprites = [Menu1Sprite, Menu2Sprite, Menu3Sprite, 
                         Info1Sprite, Info2Sprite, Info3Sprite,
                         Story0Sprite, 
                         Story1Sprite, Story2Sprite, Story3Sprite, 
                         StoryVSprite,
                         StorySprite]
            , init = (newinit size newstate)
            , keydown = newkeydown
            }
    = obj
where
    size = { w = 120, h = 100 }
    newstate = {selected = 1, min = 1, max = 3, story = False, typing = False, quitting = False, naam = "", phis = False}

    newinit size state subcode pos time gs
        # (objrec, gs) = defaultObjectRec subcode pos size time gs
        # (t, gs) = GetTime gs
        # objrec = {objrec & pos = {x = 100, y = 50}
                           , offset = if (t == FIRST_TIME) bigofs zero
                           , options = {objrec.options & static = True
                                                       , checkkeyboard = True
                                                      // , allowkeyboardrepeat = True
                                                       }
                   }
        | (t == FIRST_TIME)
            # gs = selectsound gs
            # state = {state & story = True, typing = True}
            # objrec = {objrec & currentsprite = 11}
            = {st=state, or=objrec, gs=gs}
        | (t == SHOW_PERS_HIS)
            # gs = selectsound gs
            # state = {state & story = True, phis = True}
            # objrec = {objrec & currentsprite = 12, offset = bigofs}
            = {st=state, or=objrec, gs=gs}
        = {st=state, or=objrec, gs=gs}

    newkeydown k {st, or, gs}
        | st.story
            | st.typing && (not (k == GK_RETURN))
                # namelist = [c \\ c <-: st.naam]
                | (k == 8) && (length namelist > 0)  /* backspace */
                    # st = {st & naam = {s \\ s <- (init namelist)}}
                    = {st=st, or=or, gs = setplayer st.naam gs}
                | (k >= (toInt ' ')) && (k <= (toInt 'Z')) && (length namelist < MAX_INPUT_LEN)
                    # st = {st & naam = st.naam +++ (toString (toChar k))}
                    = {st=st, or=or, gs = setplayer st.naam gs}
                = {st=st, or=or, gs=gs}
            # (_, gs) = playSoundSample SND_FLUTE DefaultVolume PAN_CENTER (getnotefreq 103) 0 gs
            | st.typing && (k == GK_RETURN)
                | st.naam == ""
                    = {st=st, or=or, gs=gs}
                # gs = selectsound gs
                # (_, gs) = dectime gs
                # st = {st & typing = False}
                = {st=st, or=or, gs=gs}
            | st.quitting
                # or = {or & currentsprite = 0, offset = zero}
                = {st=st, or=or, gs = quitlevel gs}
            # (_, gs) = dectime gs
            | st.phis
                # st = {st & phis = False}
                = {st=st, or=or, gs=gs}
            # (_, gs) = dectime gs
            # st = {st & story = False}
            # or = {or & currentsprite = st.selected, offset = zero}
            = {st=st, or=or, gs=gs}
        | k == GK_DOWN
            # (_, gs) = playSoundSample SND_XYLOFOON LowVolume PAN_CENTER (getnotefreq 112) 0 gs
            | st.selected >= st.max
                # st = {st & selected      = st.min}
                # or = {or & currentsprite = st.min}
                = {st=st, or=or, gs=gs}
            # st = {st & selected = st.selected + 1}
            # or = {or & currentsprite = st.selected}
            = {st=st, or=or, gs=gs}
        | k == GK_UP
            # (_, gs) = playSoundSample SND_XYLOFOON VeryLowVolume PAN_CENTER (getnotefreq 112) 0 gs
            | st.selected <= st.min
                # st = {st & selected      = st.max}
                # or = {or & currentsprite = st.max}
                = {st=st, or=or, gs=gs}
            # st = {st & selected = st.selected - 1}
            # or = {or & currentsprite = st.selected}
            = {st=st, or=or, gs=gs}
        | (k == GK_SPACE) || (k == GK_RETURN)
            # gs = selectsound gs

            | st.selected == 1  // Start
                # gs = setexitcode EC_SUCCESS gs
                = {st=st, or=or, gs=quitlevel gs}

            | st.selected == 2  // Informatie
                = {st = {st & selected = 4, min = 4, max = 6},  or = {or & currentsprite = 4}, gs=gs}

            # st = {st & story = True}
            # or = {or & offset = bigofs}

            | st.selected == 3  // Einde
                # gs = setexitcode EC_QUIT gs
                | CARDWARE == True
                    = {st = {st & quitting = True}, or = {or & currentsprite = 7}, gs = gs}
                = {st = {st & quitting = False}, or = {or & currentsprite = 0}, gs = quitlevel gs}

            | st.selected == 4  // Dit spel...
                = {st=st, or = {or & currentsprite = 8}, gs=gs}

            | st.selected == 5  // KUN Informatica
                = {st=st, or = {or & currentsprite = 9}, gs=gs}

            | st.selected == 6  // Clean
                = {st=st, or = {or & currentsprite = 10}, gs=gs}

            # or = {or & offset = zero}
            # st = {st & story = False}
            = {st=st, or=or, gs = quitlevel gs}
        | (k == GK_ESCAPE)
            # (_, gs) = playSoundSample SND_FLUTE DefaultVolume PAN_CENTER (getnotefreq 103) 0 gs
            | isMember st.selected [4,5,6]
                # st = {st & selected = 2, min = 1, max = 3}
                # or = {or & currentsprite = 2}            
                = {st=st, or=or, gs=gs}
            # st = {st & story = True}
            # or = {or & offset = bigofs}
            # gs = setexitcode EC_QUIT gs
            | CARDWARE == True
                = {st = {st & quitting = True}, or = {or & currentsprite = 7}, gs = gs}
            = {st = {st & quitting = False}, or = {or & currentsprite = 0}, gs = quitlevel gs}
        = {st=st, or=or, gs=gs}

    selectsound gs
        # (_, gs) = playSoundSample SND_FLUTE DefaultVolume PAN_CENTER (getnotefreq 112) 0 gs
        # (_, gs) = playSoundSample SND_FLUTE DefaultVolume PAN_CENTER (getnotefreq 115) 1 gs
        # (_, gs) = playSoundSample SND_FLUTE DefaultVolume PAN_CENTER (getnotefreq 120) 2 gs
        = gs

    bigofs = {x = -84, y = 6}

Menu1Sprite = { bitmap = Menu1Bitmap, sequence = [(1, 10000)], loop = True }
Menu2Sprite = { bitmap = Menu2Bitmap, sequence = [(1, 10000)], loop = True }
Menu3Sprite = { bitmap = Menu3Bitmap, sequence = [(1, 10000)], loop = True }

Menu1Bitmap = { bitmapname = "MENU1.BMP", unitsize = {w = 120, h = 100}, dimensions = (1, 1), transparent = Nothing }
Menu2Bitmap = { bitmapname = "MENU2.BMP", unitsize = {w = 120, h = 100}, dimensions = (1, 1), transparent = Nothing }
Menu3Bitmap = { bitmapname = "MENU3.BMP", unitsize = {w = 120, h = 100}, dimensions = (1, 1), transparent = Nothing }

Info1Sprite = { bitmap = Info1Bitmap, sequence = [(1, 10000)], loop = True }
Info2Sprite = { bitmap = Info2Bitmap, sequence = [(1, 10000)], loop = True }
Info3Sprite = { bitmap = Info3Bitmap, sequence = [(1, 10000)], loop = True }

Info1Bitmap = { bitmapname = "INFO1.BMP", unitsize = {w = 120, h = 100}, dimensions = (1, 1), transparent = Nothing }
Info2Bitmap = { bitmapname = "INFO2.BMP", unitsize = {w = 120, h = 100}, dimensions = (1, 1), transparent = Nothing }
Info3Bitmap = { bitmapname = "INFO3.BMP", unitsize = {w = 120, h = 100}, dimensions = (1, 1), transparent = Nothing }

StoryVSprite = { bitmap = StoryVBitmap, sequence = [(1, 10000)], loop = True }
Story0Sprite = { bitmap = Story0Bitmap, sequence = [(1, 10000)], loop = True }
Story1Sprite = { bitmap = Story1Bitmap, sequence = [(1, 10000)], loop = True }
Story2Sprite = { bitmap = Story2Bitmap, sequence = [(1, 10000)], loop = True }
Story3Sprite = { bitmap = Story3Bitmap, sequence = [(1, 10000)], loop = True }

StoryVBitmap = { bitmapname = "STORYV.BMP", unitsize = {w = 286, h = 130}, dimensions = (1, 1), transparent = Nothing }
Story0Bitmap = { bitmapname = "STORY0.BMP", unitsize = {w = 286, h = 130}, dimensions = (1, 1), transparent = Nothing }
Story1Bitmap = { bitmapname = "STORY1.BMP", unitsize = {w = 286, h = 130}, dimensions = (1, 1), transparent = Nothing }
Story2Bitmap = { bitmapname = "STORY2.BMP", unitsize = {w = 286, h = 130}, dimensions = (1, 1), transparent = Nothing }
Story3Bitmap = { bitmapname = "STORY3.BMP", unitsize = {w = 286, h = 130}, dimensions = (1, 1), transparent = Nothing }

StorySprite = { bitmap = StoryBitmap, sequence = [(1, 10000)], loop = True }

StoryBitmap = { bitmapname = "STORY.BMP", unitsize = {w = 286, h = 130}, dimensions = (1, 1), transparent = Nothing }


/* ---------- Level1 ---------- */

Level1
  = { boundmap     = { map = Level1Bounds  /* bounds and map codes defined in EDLEV */
                     , blocksize = Level1Layer1.bmp.unitsize
                     , objstart  = OBJ_START
                     , startobjx = 1
                     , startobjy = 1
                     }
    , initpos      = { x = 0, y = 23 * 20 } 
    , layers       = [ Static1Layer1
                     , BackGr1Layer1
                     , Level1Layer1
                     , DummyLayer
                     , DummyLayer
                     , DummyLayer
                     , DummyLayer
                     , DummyLayer
                     , DummyLayer
                     , DummyLayer
                     , ForeGr1Layer1
    
                     ]
    , objects      = GameObjectList ++ Level1FrontObj
    , music        = Nothing
	, soundsamples = GameSoundSampleList
    , leveloptions = { fillbackground = Just (BackGroundColor (1 * 2))
                     , escquit        = ESC_QUIT
                     , debugscroll    = False
                     , fadein         = FADE
                     , fadeout        = FADE
                     }
    }


Level1Layer1 =
    { bmp       = Level1Bitmap
    , layermap  = Level1Map
    , sequences = Level1Sequences
    , movement  = defaultMovement
    }

BackGr1Layer1 =
    { bmp       = BackGr1Bitmap
    , layermap  = BackGr1Map
    , sequences = BackGr1Sequences
    , movement  = defaultScrollMovement 3
    }

Static1Layer1 =
    { bmp       = Static1Bitmap
    , layermap  = Static1Map
    , sequences = Static1Sequences
    , movement  = \p t -> {x=20, y=12}
    }

ForeGr1Layer1 =
    { bmp       = ForeGr1Bitmap
    , layermap  = ForeGr1Map
    , sequences = ForeGr1Sequences
    , movement  = \p t -> {x = p.x * 5 / 4, y = p.y * 4 / 3 + (wave t)}
    }
where
    wave :: Int -> Int
    wave t =
        case (t / 20) rem 4 of
            0 ->  0
            1 -> -1
            2 ->  0
            3 ->  1
            


DummyLayer = 
    { bmp       = DummyBitmap
    , layermap  = [{0}]
    , sequences = []
    , movement  = \p t -> zero
    }

DummyBitmap 
  = { bitmapname  = "DUMMY.BMP"
    , unitsize    = {w = 320, h = 200}
    , dimensions  = (1, 1)
    , transparent = Just {x = 0, y = 0}
    }


/* ---------- Level1b ---------- */

Level1b
  = { boundmap     = { map = Level1bBounds  /* bounds and map codes defined in EDLEV */
                     , blocksize = Level1bLayer1.bmp.unitsize
                     , objstart  = OBJ_START
                     , startobjx = 1
                     , startobjy = 1
                     }
    , initpos      = { x = 0, y = 23 * 20 } 
    , layers       = [ Static1bLayer1
                     , BackGr1bLayer1
                     , Level1bLayer1

                     ]
    , objects      = GameObjectList ++ Level1FrontObj
    , music        = Nothing
	, soundsamples = GameSoundSampleList
    , leveloptions = { fillbackground = Just (BackGroundColor (1 * 2 + 1))
                     , escquit        = ESC_QUIT
                     , debugscroll    = False
                     , fadein         = FADE
                     , fadeout        = FADE
                     }
    }

Level1bLayer1 =
    { bmp       = Level1bBitmap
    , layermap  = Level1bMap
    , sequences = Level1bSequences
    , movement  = defaultMovement
    }

BackGr1bLayer1 =
    { bmp       = BackGr1bBitmap
    , layermap  = BackGr1bMap
    , sequences = BackGr1bSequences
    , movement  = defaultScrollMovement 3
    }

Static1bLayer1 =
    { bmp       = Static1bBitmap
    , layermap  = Static1bMap
    , sequences = Static1bSequences
    , movement  = \p t -> {x=20, y=12}
    }



/* ---------- Level2 ---------- */

Level2
  = { boundmap     = { map = Level2Bounds  /* bounds and map codes defined in EDLEV */
                     , blocksize = Level2Layer1.bmp.unitsize
                     , objstart  = OBJ_START
                     , startobjx = 1
                     , startobjy = 1
                     }
    , initpos      = { x = 0, y = 50 * 20 } 
    , layers       = [ Static2Layer1
                     , BackGr2Layer1
                     , Level2Layer1
    
                     ]
    , objects      = GameObjectList ++ Level1FrontObj
    , music        = Nothing
	, soundsamples = GameSoundSampleList
    , leveloptions = { fillbackground = Just (BackGroundColor (2 * 2))
                     , escquit        = ESC_QUIT
                     , debugscroll    = False
                     , fadein         = FADE
                     , fadeout        = FADE
                     }
    }

Level2Layer1 =
    { bmp       = Level2Bitmap
    , layermap  = Level2Map
    , sequences = Level2Sequences
    , movement  = defaultMovement
    }

BackGr2Layer1 =
    { bmp       = BackGr2Bitmap
    , layermap  = BackGr2Map
    , sequences = BackGr2Sequences
    , movement  = defaultScrollMovement 3
    }

Static2Layer1 =
    { bmp       = Static2Bitmap
    , layermap  = Static2Map
    , sequences = Static2Sequences
    , movement  = \p t -> {x=20, y=12}
    }


/* ---------- Level2b ---------- */

Level2b
  = { boundmap     = { map = Level2bBounds  /* bounds and map codes defined in EDLEV */
                     , blocksize = Level2bLayer1.bmp.unitsize
                     , objstart  = OBJ_START
                     , startobjx = 1
                     , startobjy = 1
                     }
    , initpos      = { x = 10 * 20, y = 8 * 20 } 
    , layers       = [ Static2Layer1
                     , BackGr2Layer1
                     , Level2bLayer1
    
                     ]
    , objects      = GameObjectList ++ Level1FrontObj
    , music        = Nothing
	, soundsamples = GameSoundSampleList
    , leveloptions = { fillbackground = Just (BackGroundColor (2 * 2 + 1))
                     , escquit        = ESC_QUIT
                     , debugscroll    = False
                     , fadein         = FADE
                     , fadeout        = FADE
                     }
    }

Level2bLayer1 =
    { bmp       = Level2bBitmap
    , layermap  = Level2bMap
    , sequences = Level2bSequences
    , movement  = defaultMovement
    }

/*
BackGr2Layer1 =
    { bmp       = BackGr2Bitmap
    , layermap  = BackGr2Map
    , sequences = BackGr2Sequences
    , movement  = defaultScrollMovement 3
    }

Static2Layer1 =
    { bmp       = Static2Bitmap
    , layermap  = Static2Map
    , sequences = Static2Sequences
    , movement  = \p t -> {x=20, y=12}
    }
*/

/* ---------------------------- */


BackGroundColor level = 
    case level of
        2           -> RGB {r= 30, g= 60, b=116}
        3           -> RGB {r= 60, g= 92, b=100}
        4           -> RGB {r=164, g= 32, b= 24}
        5           -> RGB {r=172, g= 40, b= 36}
        otherwise   -> RGB {r=  0, g=  0, b=  0}

/* ---------- sounds ---------- */

HighVolume    = (9 * MAX_VOLUME / 10)
DefaultVolume = (8 * MAX_VOLUME /  9)
LowVolume     = (7 * MAX_VOLUME /  8)
VeryLowVolume = (5 * MAX_VOLUME /  6)

RandomPan gs = IRRnd (PAN_RIGHT * 2 / 3) gs

SND_JUMP            :==   1
SND_PPN             :==   2
SND_PLOF            :==   3
SND_FLUTE           :==   4
SND_FLUTE2          :==   5
SND_XYLOFOON        :==   6
SND_CLARINET        :==   7
SND_AU              :==   8
SND_WJUMP           :==   9
SND_HIT             :==  10
SND_COCK            :==  11
SND_BIRD            :==  12
SND_WATER           :==  13
SND_GRIEG           :==  14


GameSoundSampleList = 
  [ { soundid = SND_JUMP        , soundfile = "JUMP.WAV"     , soundbuffers =  2 }
  , { soundid = SND_PPN         , soundfile = "COIN.WAV"     , soundbuffers = 15 }
  , { soundid = SND_PLOF        , soundfile = "PLOF.WAV"     , soundbuffers =  6 }
  , { soundid = SND_FLUTE       , soundfile = "FLUTE.WAV"    , soundbuffers = 15 } 
  , { soundid = SND_FLUTE2      , soundfile = "FLUTE2.WAV"   , soundbuffers = 12 } 
  , { soundid = SND_XYLOFOON    , soundfile = "XYLOFOON.WAV" , soundbuffers = 10 }
  , { soundid = SND_CLARINET    , soundfile = "CLARINET.WAV" , soundbuffers = 10 }
  , { soundid = SND_AU          , soundfile = "AU.WAV"       , soundbuffers =  5 }
  , { soundid = SND_WJUMP       , soundfile = "WJUMP.WAV"    , soundbuffers =  2 }
  , { soundid = SND_HIT         , soundfile = "HIT.WAV"      , soundbuffers =  5 }
  , { soundid = SND_COCK        , soundfile = "COCK.WAV"     , soundbuffers =  1 }
  , { soundid = SND_BIRD        , soundfile = "BIRD.WAV"     , soundbuffers =  2 }
  , { soundid = SND_WATER       , soundfile = "WATER.WAV"    , soundbuffers =  1 }
  , { soundid = SND_GRIEG       , soundfile = "GRIEG.WAV"    , soundbuffers =  1 }
  ]


MenuSoundSampleList =
  [ { soundid = SND_PLOF        , soundfile = "PLOF.WAV"     , soundbuffers =  6 }
  , { soundid = SND_FLUTE       , soundfile = "FLUTE.WAV"    , soundbuffers = 15 } 
  , { soundid = SND_FLUTE2      , soundfile = "FLUTE2.WAV"   , soundbuffers = 12 } 
  , { soundid = SND_XYLOFOON    , soundfile = "XYLOFOON.WAV" , soundbuffers = 10 }
  , { soundid = SND_CLARINET    , soundfile = "CLARINET.WAV" , soundbuffers = 10 }
  ]


/* ---------- random functions ---------- */

/* get random integer value 0..n */
IRnd n gs
    # (rnd, gs) = rand gs
    = (rnd rem n, gs)

/* get random integer value -n..n */
IRRnd n gs
    # (rnd1, gs) = rand gs
    # (rnd2, gs) = rand gs
    = ((rnd1 rem n) - (rnd2 rem n), gs)

/* get random Real value -n..n */
RRnd n gs
    # (rnd1, gs) = rand gs
    # (rnd2, gs) = rand gs
    = (n * (((toReal rnd1) / max) - ((toReal rnd2) / max)), gs)
where
    max = (toReal MAX_RAND)

gsrand :: GameState -> (Int, GameState)
gsrand gs=:{randseed}
    # (x, newrandseed) = random randseed
    = (x, {gs & randseed=newrandseed})

rand gst = accGSt gsrand gst

MAX_RAND = 65535

/* ---------- sprites ---------- */

PepernootSprite :: Sprite
PepernootSprite
  = { bitmap    = PepernootBitmap
    , sequence  = BitmapSequence 1 2 30
    , loop      = True
    }

KadoSprite color    = ObjectSprite (BitmapSequence (2 * color - 1) 2 (25 + 2 * color))
ChimneySprite n     = ObjectSprite [(25 + n, 1000)] 
LetterSprite letter = ObjectSprite [(11 + letter, 1000)] // 0="S", 1="I", 2="N", 3="T"
HeartSprite         = ObjectSprite (BitmapSequence 62 2 50)
LifeSprite          = ObjectSprite (BitmapSequence 63 6 0)
GlitterSprite       = { (ObjectSprite (BitmapSequence 15 4 10)) & loop = False }
LampSprite 0        = ObjectSprite [(23, 650)]  // 0 = on
LampSprite 1        = ObjectSprite [(24, 1), (23, 0), (24,0)]  // 1 = blink
LampSprite 2        = ObjectSprite [(24, 650)]  // 2 = off
BirdSprite n        = ObjectSprite (BitmapSequence (69 + 2 * n) 2 40)
FlitsSprite         = { ObjectSprite (BitmapSequence 73 5 8) & loop = False }
TrampSprite         = { ObjectSprite [(78, 1), (79, 1)] & loop = False }

ObjectSprite seq
  = { bitmap    = ObjectsBitmap
    , sequence  = seq
    , loop      = True
    } 

CarSprite
  = { bitmap    = CarsBitmap
    , sequence  = BitmapSequence 1 2 4
    , loop      = True
    } 

StatusSprite n
  = { bitmap   = StatusBitmap
    , sequence = [(n, 10000)]
    , loop     = True
    }

SintSprite :: Sprite
SintSprite = { bitmap = MainCharBitmap, sequence = [], loop = False }

SintIdleSprite = { SintSprite & sequence = [(1, 1000)] }
SintWalkSprite = { SintSprite & sequence = [(2, 10), (3, 10), (4, 10), (3, 10)] } 
SintJumpSprite = { SintSprite & sequence = [(6, 2)] }
SintFallSprite = { SintSprite & sequence = [(7, 2)] }
SintFFstSprite = { SintSprite & sequence = [(8, 2)] }

SintTurnSprite = { SintSprite & sequence = [(5, 4)] }
SintDeadSprite = { SintSprite & sequence = [(5, 1000)], loop = True }

FadeSprite1 n = {bitmap = FadeBitmap, sequence = [(n * 13 + 1, 1000)], loop = True}
FadeSprite2 n = {bitmap = FadeBitmap, sequence = BitmapSequence (n * 13 + 1) 13 3, loop = False}

SunSprite
  = { bitmap    = SunRiseBitmap
    , sequence  = [(1, 10), (1, 10)]
    , loop      = True
    }


L1FrontObjectSprite :: Int -> Sprite
L1FrontObjectSprite n
    = { bitmap = Level1FrontBitmap
      , sequence = case n of
                       1 -> [(1, 1000)]
                       2 -> [(2, 1000)]
                       3 -> [(3, 25), (4, 25)]
                       4 -> [(5, 1000)]
                       otherwise -> [(1, 1000)]
      , loop = True 
      }



BitmapSequence :: Int Int Int -> [(Int, Int)]
BitmapSequence start count speed
    | count == 0  = []
    | otherwise   = [(start, speed)] ++
                        (BitmapSequence (start + 1) (count - 1) speed)



/* ---------- textitems ---------- */


Version :: GameText
Version
  = { format    = VERSION
    , value     = Nothing
    , position  = {x = 0, y = -26}
    , style     = { fontname = "Arial"
                  , fontsize = 16
                  , bold     = False
                  , italic   = False
                  }
    , color     = RGB {r = 200, g = 230, b = 255}
    , shadow    = Just StatShadow
    , alignment = {xyfromscreencenter = (True, True), xycentered = (True, True)}
    }


TypeNaam
  = { Version 
    & format = "Type je naam in en druk op [Enter]:"
    , position = {x = 0, y = -4}
    , color = RGB {r = 255, g = 255, b = 220}
    }


Naam :: String -> GameText
Naam s
  = { format    = s
    , value     = Nothing
    , position  = {x = -2, y = 19}
    , style     = { fontname = "Courier New"
                  , fontsize = 22
                  , bold     = False
                  , italic   = False
                  }
    , color     = RGB {r = 255, g = 255, b = 255}
    , shadow    = Just StatShadow
    , alignment = {xyfromscreencenter = (True, True), xycentered = (True, True)}
    }


Pers :: Int -> GameText
Pers score
  = { format    = if (score == 0) "Nieuwe speler, welkom!" "Persoonlijke top-score: %d punten."
    , value     = if (score == 0) Nothing (Just score)
    , position  = {x = 0, y = 40}
    , style     = { fontname = "Arial"
                  , fontsize = 16
                  , bold     = False
                  , italic   = False
                  }
    , color     = RGB {r = 200, g = 230, b = 255}
    , shadow    = Just StatShadow
    , alignment = {xyfromscreencenter = (True, True), xycentered = (True, True)}
    }



PersHiScores :: [HiS] Int Int String Int -> [GameText]
PersHiScores his score phis player rank
    = [{Version & format = player, position = {x = 0, y = -29}, color = RGB {r = 255, g = 255, b = 220}},
       {Version & format = "Behaalde score: %d punten.", value = Just score, position = {x = 0, y = -4}},
       {Version & format = (if (rank <= HIS_COUNT)
                              "Gefeliciteerd, nieuwe topscore!" 
                              (if (score > phis) 
                                "Nieuwe persoonlijk record!"
                                "Persoonlijke topscores:")), position = {x = 0, y = 14}}]
       ++ (hi his 3)
where
    hi [] n
        | n < HIS_COUNT  = [HiName n "Leeg"] ++ [HiSc n 0] ++ (hi [] (n + 1))
        | otherwise      = []
    hi [x:xs] n
        | (n >= HIS_COUNT) = [] 
        | (x.name == player) 
            = ([HiName n x.name] ++ [HiSc n x.hiscore] ++ (hi xs (n + 1)))
        | otherwise
            = (hi xs n)



HIS_COUNT :== 6

HiScores :: [HiS] -> [GameText]
HiScores his
    = [{Version & format = "Topscores:", position = {x = 0, y = -29}}] ++ (hi his 0)
where
    hi [] n
        | n < HIS_COUNT  = [HiName n "Leeg"] ++ [HiSc n 0] ++ (hi [] (n + 1))
        | otherwise      = []
    hi [x:xs] n
        | n < HIS_COUNT  = [HiName n x.name] ++ [HiSc n x.hiscore] ++ (hi xs (n + 1))
        | otherwise      = []

HiName :: Int String -> GameText
HiName n name
  = { format    = name
    , value     = Nothing
    , position  = {x = 160 - 130, y = yheight n}
    , style     = { fontname = "Arial"
                  , fontsize = 16
                  , bold     = False
                  , italic   = False
                  }
    , color     = RGB {r = 255 - 13 * n, g = 255 - 11 * n, b = 255 - 8 * n}
    , shadow    = Just StatShadow
    , alignment = zero
    }

HiSc n score
  = {(HiName n "%d") & value = Just score, position = {x = -160 + 130, y = yheight n}}

yheight n = 113 + (n - 2) * 15


BigStyle :: Style
BigStyle
  = { fontname = "Arial"
    , fontsize = 41
    , bold     = True
    , italic   = False
    }

Bonus :: Int -> GameText
Bonus x
  = { format    = " BONUS LEVEL, VERZAMEL ALLE PEPERNOTEN... "
    , value     = Nothing
    , position  = {x = x, y = 6}
    , style     = StatStyle
    , color     = White
    , shadow    = Just StatShadow
    , alignment = zero
    }

Morning :: Int -> GameText
Morning x
  = { format    = " DE OCHTEND BREEKT AAN... "
    , value     = Nothing
    , position  = {x = x, y = 6}
    , style     = StatStyle
    , color     = White
    , shadow    = Just StatShadow
    , alignment = zero
    }

Ready :: Int -> GameText
Ready x
  = { format    = " KLAAR, ZOEK DE UITGANG... "
    , value     = Nothing
    , position  = {x = x, y = 6}
    , style     = StatStyle
    , color     = White
    , shadow    = Just StatShadow
    , alignment = zero
    }

NotReady :: Int -> GameText
NotReady x
  = { format    = " ER MOETEN NOG PAKJES OF PEPERNOTEN BEZORGD WORDEN... "
    , value     = Nothing
    , position  = {x = x, y = 6}
    , style     = StatStyle
    , color     = White
    , shadow    = Just StatShadow
    , alignment = zero
    }

GameOver :: GameText
GameOver
  = { format    = "HELAAS..."
    , value     = Nothing
    , position  = {x = 0, y = 0}
    , style     = BigStyle
    , color     = White
    , shadow    = Just StatShadow
    , alignment = {xyfromscreencenter = (True, True), xycentered = (True, True)}
    }

TimeUp :: GameText
TimeUp
  = { format    = "TE LAAT!"   // tijd op
    , value     = Nothing
    , position  = {x = 0, y = 0}
    , style     = BigStyle
    , color     = White
    , shadow    = Just StatShadow
    , alignment = {xyfromscreencenter = (True, True), xycentered = (True, True)}
    }

Lives :: Int -> GameText
Lives n
  = { format    = "Sint   %2d"
    , value     = Just n
    , position  = {x = 10, y = STS}
    , style     = StatStyle
    , color     = White
    , shadow    = Just StatShadow
    , alignment = zero
    }

TimeLeft :: Int -> GameText
TimeLeft n
  = { format    = "Tijd  %3d"
    , value     = Just n
    , position  = {x = 90, y = STS}  // 113
    , style     = StatStyle
    , color     = White
    , shadow    = Just StatShadow
    , alignment = {zero & xycentered = (True, False)}
    }

PPN :: Int -> GameText
PPN n
  = { format    = "%d"
    , value     = Just n
    , position  = {x = 161-4, y = STS}
    , style     = StatStyle
    , color     = White
    , shadow    = Just StatShadow
    , alignment = {zero & xycentered = (True, False)}
    }

Score :: Int -> GameText
Score n
  = { format    = "Score  %07d"
    , value     = Just n
    , position  = {x = -10, y = STS}
    , style     = StatStyle
    , color     = White
    , shadow    = Just StatShadow
    , alignment = zero
    }

StatStyle :: Style
StatStyle
  = { fontname = "Arial"
    , fontsize = 14
    , bold     = True
    , italic   = False
    }

StatShadow :: Shadow
StatShadow
  = { shadowpos   = {x = 1, y = 1}
    , shadowcolor = Black
    }
