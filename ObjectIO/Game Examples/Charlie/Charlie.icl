module Charlie

/*  Charlie the Duck - (C) Copyright 1996-99, by Mike Wiering, Nijmegen.  */

/*
    COPYRIGHT NOTICE

    GRAPHICS BY MIKE WIERING, COPYRIGHT 1996-99, ALL RIGHTS RESERVED.

    This game is meant only as a demonstration for the Clean Game Library.
    You may create new games based on this source code, however DO NOT use
    any of the graphics, instead create your own.
    The graphics used in this game are from CHARLIE THE DUCK and CHARLIE II
    (a new game, still in development).
*/

import StdProcess
import StdGameDef, StdGame, StdGSt
import GameFunctions

/* music notes */
import notes

/* levels */
import TITLE
import L1
import L2

/* sprites */
import OBJ
import CLOUDS
import ENEMY
import BEES
import FROGS
import CH
import PART
import PALM
import WATER
import ENDING
import STATUS
import INFRONT

DEFAULT_LIVES :== 3

:: GameState
   = { curlevel    :: Int
     , maxlevel    :: Int
     , titlescreen :: Bool
     , statusline  :: Bool
     , exitcode    :: Int
     , lives       :: Int
     , coins       :: Int
     , diamonds    :: Int
     , score       :: Int
     , quit        :: Bool
     , gameover    :: Bool
     }


initialGameState = { curlevel = 0
                   , maxlevel = 3
                   , titlescreen = False
                   , statusline = False
                   , exitcode = EC_NONE
                   , lives = DEFAULT_LIVES
                   , coins = 0
                   , diamonds = 0
                   , score = 0
                   , quit = False
                   , gameover = False
                   }

EC_NONE      :==  0
EC_SUCCESS   :==  1
EC_FAILURE   :==  2
EC_QUIT      :==  3


/* ---------- main program: load game definition and start the game! ---------- */

Start world
    = startIO SDI 0 init [ProcessClose closeProcess] world
where
    init ps
        #   (_, ps) = OpenGame initialGameState DuckGame [ColorDepth 16] ps
                             /* use default mode: 230x240 */
        =   closeProcess ps


/* ---------- the complete game definition ---------- */

DuckGame :: (Game GameState)
DuckGame =
    { levels = [ TitleScreen
               , GameLevel1
               , GameLevel2 
               ]
    , quitlevel = accGSt QuitFunction
    , nextlevel = accGSt NextLevelFunction
    , statistics = accGSt Statistics
    }


/* if the quit function returns true, the game engine quit the level */

QuitFunction :: GameState -> (Bool, GameState)
QuitFunction gst
    = (gst.quit, {gst & quit = False})


/* function that returns the next level to run, 0 = end game */

NextLevelFunction :: GameState -> (Int, GameState)
NextLevelFunction gst =: {curlevel, maxlevel, titlescreen, exitcode, lives, gameover}
    | exitcode == EC_QUIT
        = (0, gst)
    | titlescreen
        = (next, {gst & titlescreen = False
                      , statusline = True
                      , lives = DEFAULT_LIVES
                      , curlevel = next})
    | exitcode == EC_FAILURE
         | lives > 0
             = (curlevel, {gst & lives = lives - 1})
         = title
    | exitcode == EC_SUCCESS
         = nextlevel
    = title
where
    title = (1, {gst & titlescreen = True
                     , statusline = False
                     , gameover = False
                     , curlevel = 1})
    nextlevel = if (curlevel + 1 > maxlevel)
                     title
                     (next, {gst & curlevel = next})
    next = curlevel + 1

/* function that returns text to be displayed */

Statistics :: GameState -> ([Statistic], GameState)
Statistics gst
    | gst.titlescreen
        = ([ TitleTextShadow, TitleText
           , DemoText
           , Copyright
           , MenuText 0 "Start"
           , MenuText 1 "Exit"
           ], gst)
    | gst.statusline
        = ([ Lives    gst.lives
           , Diamonds gst.diamonds
           , Coins    gst.coins
           , Score    gst.score
           ] ++ (if gst.gameover [GameOver] []), gst)
    = ([], gst)


/* ---------- definitions of the levels ---------- */

/* default block size */
W :== 20
H :== 16

DEFAULT_SIZE :== {w = W, h = H}

/* layers */
LYR_BACKGROUND   :==   1
LYR_FOREGROUND   :==   2
LYR_PLAYER       :==   3
LYR_INFRONT      :==   4

LYR_STATUS       :==  10


/* user events */
EV_QUIT_LEVEL    :==   1
EV_GAME_OVER     :==   2

EV_STOP_BLINKING :==  10
EV_STOP_MOVING   :==  11

EV_HEALTH        :==  20


/* ---------- objects ---------- */

/* bounds */
BND_MAIN_CHARACTER :==  (1 <<  0)
BND_POWER_UP       :==  (1 <<  1)   /* coins, diamonds, hearts, etc */
BND_BLOCKS         :==  (1 <<  2)   /* crates and bounce blocks */
BND_ENEMY          :==  (1 <<  3)
BND_KILL           :==  (1 <<  4)
BND_WATER          :==  (1 <<  5)
BND_ENDING         :==  (1 <<  6)
BND_STAT           :==  (1 <<  7)
/* predefined bounds
BND_MAP_CODES      :==  (1 << 30)
BND_STATIC_BOUNDS  :==  (1 << 31)
*/


/* object codes (initialized by code in the bound map) */

OBJ_AUTOINIT         :==     0

OBJ_START            :==  0x10   /* lower map values are subtypes */

OBJ_STATIC_COIN      :==  0x10
OBJ_FALLING_COIN     :==  0x11
OBJ_STATIC_DIAMOND   :==  0x12
OBJ_FALLING_DIAMOND  :==  0x13
OBJ_HEART            :==  0x14
OBJ_LIFE             :==  0x15

OBJ_CLOUD            :==  0x1A
OBJ_PALM             :==  0x1B
OBJ_GROUND1          :==  0x1C
OBJ_GROUND2          :==  0x1D
OBJ_GROUND3          :==  0x1E
OBJ_WATER            :==  0x1F

OBJ_BOUNCEBLOCK      :==  0x20

OBJ_PIN              :==  0x22

OBJ_ENEMY            :==  0x80
OBJ_BEE              :==  0x81
OBJ_FROG             :==  0x82

OBJ_INVISIBLE_CRATE  :==  0xB0
OBJ_CRATE            :==  0xC0

OBJ_MAIN_CHAR        :==  0xF0

OBJ_ENDING           :==  0xFE

/* objects created during the game */

OBJ_CRATE_PART       :== 0x100
OBJ_SPLASH           :== 0x101
OBJ_FLASH            :== 0x102

OBJ_STAT             :== 0x110




GameObjectList = [ AutoInitObject
                 , MainCharObject
                 , StaticCoinObject
                 , FallingCoinObject
                 , StaticDiamondObject
                 , FallingDiamondObject
                 , HeartObject
                 , LifeObject
                 , CrateObject
                 , InvisibleCrateObject
                 , CratePartObject
                 , EnemyObject
                 , BeeObject
                 , FrogObject
                 , CloudObject
                 , PalmFrontObject
                 , WaterObject
                 , SplashObject
                 , BounceBlockObject
                 , FlashObject
                 , EndingObject
                 , StatHeartObject
                 , BlockInFrontObject OBJ_GROUND1 (InFrontSprite 1)
                 , BlockInFrontObject OBJ_GROUND2 (InFrontSprite 2)
                 , BlockInFrontObject OBJ_GROUND3 (InFrontSprite 3)
                 , PinObject
                 ]


/* ---------- background cloud ---------- */

/*
   Because we only have a few little clouds, we will use
   objects instead of a complete layer here.
*/

CloudObject
    # obj = defaultGameObject OBJ_CLOUD size NoState
    # obj = { obj
            & sprites = [CloudSprite]
            , init    = (newinit size NoState)
            }
    = obj
where
    size    = {w = 40, h = 24}

    newinit size state subtype _ time gs
        # pos = case subtype of
                   1 -> { x =  36, y = 50 }
                   2 -> { x =  88, y = 28 }
                   3 -> { x = 240, y = 43 }
        # (objrec, gs) = defaultObjectRec subtype pos size time gs
        # objrec = { objrec
                   & options.static             = True
                   , options.ignorelevelbounds  = True
                   , layer                      = AtLayer LYR_BACKGROUND
                   }
        = ((state, objrec), gs)


/* ---------- palm front object ---------- */

/*
   This object is the part of a palm tree that is shown in front
   of Charlie. This could also be done with a complete layer, but
   again for speed we will use objects.
*/

PalmFrontObject
    # obj = defaultGameObject OBJ_PALM size NoState
    # obj = { obj
            & sprites = [PalmSprite 2, PalmSprite 1, PalmSprite 3]
            , init    = (newinit size NoState)
            }
    = obj
where
    size    = {w = 20, h = 32}

    newinit size state subtype pos time gs
        # (objrec, gs) = defaultObjectRec subtype pos size time gs
        # objrec = { objrec
                   & layer          = AtLayer LYR_INFRONT
                   , offset         = {x = 0, y = ~H}
                   , currentsprite  = (subtype + 1)
                   }
        = ((state, objrec), gs)


BlockInFrontObject objtype spr
    # obj = defaultGameObject objtype size NoState
    # obj = { obj
            & sprites = [spr]
            , init    = (newinit size NoState)
            }
    = obj
where
    size    = DEFAULT_SIZE

    newinit size state subtype pos time gs
        # (objrec, gs) = defaultObjectRec subtype pos size time gs
        # objrec = { objrec
                   & layer          = AtLayer LYR_INFRONT
                   }
        = ((state, objrec), gs)


/* ---------- coins and diamonds ---------- */

SPR_ITEM    :== 1
SPR_GLITTER :== 2

StaticCoinObject     = StaticGameItem  OBJ_STATIC_COIN     (CoinSprite 8)
FallingCoinObject    = FallingGameItem OBJ_FALLING_COIN    (CoinSprite 8)
StaticDiamondObject  = StaticGameItem  OBJ_STATIC_DIAMOND  DiamondSprite
FallingDiamondObject = FallingGameItem OBJ_FALLING_DIAMOND DiamondSprite
HeartObject          = FallingGameItem OBJ_HEART           HeartSprite
LifeObject           = FallingGameItem OBJ_LIFE            LifeSprite

FallingGameItem objecttype sprite
    # obj = StaticGameItem objecttype sprite
    # obj = { obj
            & init    = (newinit size NoState)
            }
    = obj
where
    size    = DEFAULT_SIZE

    newinit size state subtype pos time gs
        # pos = {pos & x = if (subtype == 1) (pos.x + W / 2) (pos.x)}
        # (objrec, gs) = defaultObjectRec subtype pos size time gs
        # xv = case subtype of
            11        -> ~1.6
            12        -> ~0.8
            14        ->  0.8
            15        ->  1.6
            otherwise ->  0.0
        # objrec = { objrec
                   & acceleration    = {rx = 0.0, ry = 1.0 / 16.0}
                   , speed           = {rx = xv, ry = ~1.25 + ((abs xv) / 4.0)}
                   , slowdown        = {fvx = Factor (1.0 / 32.0), fvy = Value 0.0}
                   , bounce          = {fvx = Value 0.0, fvy = Factor (4.0 / 5.0)}
                   , layer           = AtLayer LYR_FOREGROUND
                   , ownbounds       = BND_POWER_UP
                   , bouncebounds    = BND_STATIC_BOUNDS
                   , collidebounds   = BND_MAIN_CHARACTER
                   , forgetdistance  = {x = 8, y = 8}
                   }
        = ((state, objrec), gs)

StaticGameItem objecttype sprite
    # obj = defaultGameObject objecttype size NoState
    # obj = { obj
            & sprites   = [sprite, GlitterSprite 25]
            , init      = (newinit size NoState)
            , collide   = newcollide
            , animation = killobject
            }
    = obj
where
    size    = DEFAULT_SIZE

    newinit size state subtype pos time gs
        # pos = {pos & x = if (subtype == 1) (pos.x + W / 2) (pos.x)}
        # (objrec, gs) = defaultObjectRec subtype pos size time gs
        # objrec = { objrec
                   & options.ignorelevelbounds  = True
                   , ownbounds                  = BND_POWER_UP
                   , bouncebounds               = BND_STATIC_BOUNDS
                   , collidebounds              = BND_MAIN_CHARACTER
                   , layer                      = AtLayer LYR_FOREGROUND
                   }
        = ((state, objrec), gs)

    newcollide (st, or) bnds objtype objrec gs
        | objtype == OBJ_MAIN_CHAR
            # (points, gs) = ItemScoreAndSound objecttype gs
            # gs = addscore points gs
            = ((st, {or & currentsprite         = SPR_GLITTER
                        , options.removemapcode = True
                        , layer                 = AtLayer LYR_INFRONT
                        , ownbounds             = 0
                        , collidebounds         = 0
                        }), gs)
        = ((st, or), gs)

ItemScoreAndSound ot gs
    # (pan, gs) = RandomPan gs
    | (ot == OBJ_STATIC_COIN) || (ot == OBJ_FALLING_COIN)
        # (_, gs) = PlaySoundSample SND_COIN DefaultVolume pan (getnotefreq 61) 0 gs
        # (_, gs) = PlaySoundSample SND_COIN DefaultVolume pan (getnotefreq 73) 4 gs
        = (50, inccoins gs)
    | (ot == OBJ_FALLING_DIAMOND) || (ot == OBJ_STATIC_DIAMOND)
        # instr = SND_COIN
        # (_, gs) = PlaySoundSample instr DefaultVolume pan (getnotefreq 68) 0 gs
        # (_, gs) = PlaySoundSample instr DefaultVolume pan (getnotefreq 75) 8 gs
        # (_, gs) = PlaySoundSample instr HighVolume pan (getnotefreq 80) 16 gs
        = (150, incdiamonds gs)
    | (ot == OBJ_HEART) || (ot == OBJ_LIFE)
        # instr = if (ot == OBJ_LIFE) SND_FLUTE SND_XYLOFOON
        # (_, gs) = PlaySoundSample instr HighVolume pan (getnotefreq  96)  0 gs
        # (_, gs) = PlaySoundSample instr HighVolume pan (getnotefreq 100)  5 gs
        # (_, gs) = PlaySoundSample instr HighVolume pan (getnotefreq 103) 10 gs
        # (_, gs) = PlaySoundSample instr HighVolume pan (getnotefreq 108) 15 gs
        # (_, gs) = PlaySoundSample instr HighVolume pan (getnotefreq 112) 20 gs
        # (_, gs) = PlaySoundSample instr HighVolume pan (getnotefreq 115) 25 gs
        # (_, gs) = PlaySoundSample instr HighVolume pan (getnotefreq 120) 30 gs
        = if (ot == OBJ_LIFE) (500, inclives gs) (100, gs)
    = (100, gs)

/*
OBJ_STATIC_COIN      :==  0x10
OBJ_FALLING_COIN     :==  0x11
OBJ_STATIC_DIAMOND   :==  0x12
OBJ_FALLING_DIAMOND  :==  0x13
OBJ_HEART            :==  0x14
OBJ_LIFE             :==  0x15
*/

killobject (st, or) gs
    = ((st, {or & active = False}), gs)


/* ---------- crates ---------- */

/*
   Crates contain items which appear when Charlie opens these
   crates by jumping on top of them.
*/

CrateObject          = Crate True
InvisibleCrateObject = Crate False

Crate visible
    # obj = defaultGameObject (if visible OBJ_CRATE OBJ_INVISIBLE_CRATE) size NoState
    # obj = { obj
            & sprites = [CrateSprite]
            , init    = (newinit size NoState)
            , collide = newcollide
            }
    = obj
where
    size    = DEFAULT_SIZE

    newinit size state subtype pos time gs
        # (objrec, gs) = defaultObjectRec subtype pos size time gs
        # objrec = { objrec
                   & layer                      = AtLayer LYR_FOREGROUND
                   , ownbounds                  = if visible
                                                    (BND_STATIC_BOUNDS + BND_BLOCKS)
                                                    (BND_BLOCKS)
                   , collidebounds              = BND_MAIN_CHARACTER
                   , currentsprite              = if visible 1 0
                   }
        = ((state, objrec), gs)

    newcollide (st, or) bnds othertype otherobjrec gs
        | ((othertype == OBJ_MAIN_CHAR) && (bnds.bottom))
            # pos1 = or.pos
            # pos2 = {pos1 & y = pos1.y + 8}
            # (_, gs) = CreateNewGameObject OBJ_CRATE_PART 1 pos1 gs
            # pos1 = {pos1 & x = pos1.x + 4}
            # (_, gs) = CreateNewGameObject OBJ_CRATE_PART 2 pos1 gs
            # pos1 = {pos1 & x = pos1.x + 4}
            # (_, gs) = CreateNewGameObject OBJ_CRATE_PART 3 pos1 gs
            # (_, gs) = CreateNewGameObject OBJ_CRATE_PART 4 pos2 gs
            # pos2 = {pos2 & x = pos2.x + 4}
            # (_, gs) = CreateNewGameObject OBJ_CRATE_PART 5 pos2 gs
            # pos2 = {pos2 & x = pos2.x + 4}
            # (_, gs) = CreateNewGameObject OBJ_CRATE_PART 6 pos2 gs
            # or = {or & options.removemapcode = True, active = False}
            # obj = case or.subtype of
                0  ->  if visible OBJ_FALLING_COIN OBJ_STATIC_COIN
                1  ->  if visible OBJ_FALLING_DIAMOND OBJ_STATIC_DIAMOND
                2  ->  OBJ_HEART
                3  ->  OBJ_LIFE
                4  ->  OBJ_FALLING_COIN
                5  ->  OBJ_FALLING_DIAMOND
            # (_, gs) = CreateNewGameObject obj 0 or.pos gs
            | (or.subtype == 4) || (or.subtype == 5)
                # (_, gs) = CreateNewGameObject obj 11 or.pos gs
                # (_, gs) = CreateNewGameObject obj 15 or.pos gs
                # (_, gs) = CreateNewGameObject obj 12 or.pos gs
                # (_, gs) = CreateNewGameObject obj 14 or.pos gs
                = ((st, or), gs)
            = ((st, or), gs)
        = ((st, or), gs)

CratePartObject
    # obj = defaultGameObject OBJ_CRATE_PART size NoState
    # obj = { obj
            & sprites = [CratePartSprite]
            , init    = (newinit size NoState)
            }
    = obj
where
    size    = {w = 12, h = 8}

    newinit size state subtype pos time gs
        # (objrec, gs) = defaultObjectRec subtype pos size time gs
        # ((xv, yv), gs) = case subtype of
                            1 -> (rnd (~1.0, ~3.0) gs)
                            2 -> (rnd ( 0.0, ~3.2) gs)
                            3 -> (rnd ( 1.0, ~3.0) gs)
                            4 -> (rnd (~1.0, ~2.0) gs)
                            5 -> (rnd ( 0.0, ~2.2) gs)
                            6 -> (rnd ( 1.0, ~2.0) gs)
        # objrec = { objrec & acceleration   = {rx = 0.0, ry = 1.0 / 12.0}
                            , speed          = {rx = xv, ry = yv}
                            , forgetdistance = {x = 1, y = 1}
                   }
        = ((state, objrec), gs)
    where
        rnd (x, y) gs
            # (r1, gs) = (RRnd 1.0 gs)
            # (r2, gs) = (RRnd 1.0 gs)
         = ((x + r1, y + r2), gs)


/* ---------- flash ---------- */

FlashObject
    # obj = defaultGameObject OBJ_FLASH size NoState
    # obj = { obj
            & sprites   = [FlashSprite]
            , init      = (newinit size NoState)
            , animation = killobject
            }
    = obj
where
    size    = {w = 24, h = 20}

    newinit size state subtype pos time gs
        # (objrec, gs) = defaultObjectRec subtype pos size time gs
        # objrec = { objrec
                   & layer         = AtLayer LYR_INFRONT
                   }
        = ((state, objrec), gs)


/* ---------- bounce block ---------- */

BounceBlockObject
    # obj = defaultGameObject OBJ_BOUNCEBLOCK size NoState
    # obj = { obj
            & sprites   = [BounceBlockSprite, BounceBlockShortSprite]
            , init      = (newinit size NoState)
            , collide   = newcollide
            , animation = newanimation
            }
    = obj
where
    size    = DEFAULT_SIZE

    newinit size state subtype pos time gs
        # (objrec, gs) = defaultObjectRec subtype pos size time gs
        # objrec = { objrec
                   & layer                      = AtLayer LYR_FOREGROUND
                   , speed                      = {rx = ~(toReal subtype), ry = 0.0}
                   , bounce                     = {fvx = Factor 1.0, fvy = Value 0.0}
                   , forgetdistance             = {x = 5 + 15 * subtype, y = 5}
                   , ownbounds                  = BND_STATIC_BOUNDS + BND_BLOCKS
                   , collidebounds              = BND_MAIN_CHARACTER
                   , bouncebounds               = BND_STATIC_BOUNDS
                   }
        = ((state, objrec), gs)

    newcollide (st, or) bnds othertype otherobjrec gs
        | othertype == OBJ_MAIN_CHAR
            | (bnds.top || bnds.bottom)
                = ((st, {or & offset.y = (if bnds.top 4 (~4))
                            , currentsprite = 2}), gs)
            = ((st, or), gs)
        = ((st, or), gs)

    newanimation (st, or) gs
        | (or.offset.y == 0)
            = ((st, {or & currentsprite = 1}), gs)
        # or = {or & offset.y = ~(decr or.offset.y)}
        = ((st, or), gs)
    where
        decr x
            | x < 0     = x + 1
            | otherwise = x - 1


/* ---------- water ---------- */

WaterObject
    # obj = defaultGameObject OBJ_WATER size NoState
    # obj = { obj
            & sprites = [WaterSprite]
            , init    = (newinit size NoState)
            }
    = obj
where
    size    = {w = 20, h = 8}

    newinit size state subtype pos time gs
        # (objrec, gs) = defaultObjectRec subtype pos size time gs
        # objrec = { objrec
                   & layer         = AtLayer LYR_INFRONT
                   , ownbounds     = BND_WATER
                   , offset        = {x = 0, y = ~11}
                   }
        = ((state, objrec), gs)

Splash :: !Point2 (!*GSt gs) -> (!GRESULT, !*GSt gs)
Splash pos gs
    # pos = {pos & y = pos.y - 11}
    # (_, gs) = CreateNewGameObject OBJ_SPLASH 1 pos gs
    # (_, gs) = CreateNewGameObject OBJ_SPLASH 2 pos gs
    # (_, gs) = CreateNewGameObject OBJ_SPLASH 3 pos gs
    = (GR_OK, gs)

/*
    Splash subtype:
       1: left wave
       2: right wave
       3: splash
*/

SplashObject
    # obj = defaultGameObject OBJ_SPLASH size NoState
    # obj = { obj
            & sprites   = [WaveSprite, WaveSprite, SplashSprite]
            , init      = (newinit size NoState)
            , animation = killobject
            }
    = obj
where
    size    = {w = 20, h = 14}

    newinit size state subtype pos time gs
        # (objrec, gs) = defaultObjectRec subtype pos size time gs
        # objrec = { objrec
                   & layer = AtLayer LYR_INFRONT
                   , currentsprite = subtype
                   , offset = case subtype of
                                1 -> {x = ~20, y = ~12}
                                2 -> {x =  20, y = ~12}
                                3 -> {x =   0, y =  12}
                   , displayoptions.mirrorleftright = if (subtype == 1) True False
                   }
        = ((state, objrec), gs)


/* ---------- enemies ---------- */

EnemyObject
    # obj = defaultGameObject OBJ_ENEMY size NoState
    # obj = { obj
            & sprites = [EnemySprite1, EnemySprite2]
            , init    = (newinit size NoState)
            , collide = newcollide
            }
    = obj
where
    size    = {w = 20, h = 18}

    newinit size state subtype pos time gs
        # (objrec, gs) = defaultObjectRec subtype pos size time gs
        # objrec = { objrec
                   & offset         = {x = ~2, y = ~2}
                   , speed          = {rx = ~0.5, ry = 0.0}
                   , bounce         = {fvx = Factor 1.0, fvy = Value 0.0}
                   , layer          = AtLayer LYR_FOREGROUND
                   , options        = { objrec.options
                                      & automirrorleftright = True
                                      }
                   , ownbounds      = BND_ENEMY
                   , bouncebounds   = BND_STATIC_BOUNDS + BND_ENEMY + BND_MAP_CODES
                   , collidebounds  = BND_MAIN_CHARACTER
                   , currentsprite  = (1 + subtype)
                   , forgetdistance = {x = 6, y = 4}
                   }
        = ((state, objrec), gs)

    newcollide (st, or) bnds othertype otherobjrec gs
        | ((othertype == OBJ_MAIN_CHAR) && (bnds.bottom))
            = ((st, kill or), gs)
        = ((st, or), gs)

kill :: ObjectRec -> ObjectRec
kill or =
    {or & displayoptions.mirrorupdown = True
        , acceleration = {rx = 0.0, ry = 1.0 / 16.0}
        , speed = {rx = ~(or.speed.rx / 2.0), ry = ~3.0}
        , ownbounds = 0
        , bouncebounds = 0
        , collidebounds = 0
        , forgetdistance = {x = 1, y = 1}
        , layer = InFront
        , options.removemapcode = True
        }


/* ---------- pin ---------- */

PinObject
    # obj = defaultGameObject OBJ_PIN size NoState
    # obj = { obj
            & init    = (newinit size NoState)
            }
    = obj
where
    size    = {w = 20, h = 16}

    newinit size state subtype pos time gs
        # (objrec, gs) = defaultObjectRec subtype pos size time gs
        # objrec = { objrec
                   & ownbounds      = BND_KILL + BND_STATIC_BOUNDS
                   }
        = ((state, objrec), gs)


/* ---------- flying enemies ---------- */

BeeObject = FlyingObject OBJ_BEE [BeeSprite]

FlyingObject objtype sprlist
    # obj = defaultGameObject objtype size NoState
    # obj = { obj
            & sprites = sprlist
            , init    = (newinit size NoState)
            , collide = newcollide
            , move    = newmove
            }
    = obj
where
    size    = {w = 20, h = 16}

    newinit size state subtype pos time gs
        # (objrec, gs) = defaultObjectRec subtype pos size time gs
        # objrec = { objrec
                   & offset         = {x = 0, y = ~3}
                   , speed          = {rx = ~0.5, ry = 0.0}
                   , bounce         = {fvx = Factor 1.0, fvy = Factor 1.0}
                   , layer          = AtLayer LYR_FOREGROUND
                   , options        = { objrec.options
                                      & automirrorleftright = True
                                      }
                   , ownbounds      = BND_ENEMY
                   , bouncebounds   = BND_STATIC_BOUNDS + BND_ENEMY + BND_MAP_CODES
                   , collidebounds  = BND_MAIN_CHARACTER
                   , currentsprite  = (1 + subtype)
                   , forgetdistance = {x = 6, y = 4}
                   , skipmove       = 0
                   }
        = ((state, objrec), gs)

    newcollide (st, or) bnds othertype otherobjrec gs
        | ((othertype == OBJ_MAIN_CHAR) && (bnds.bottom))
            # (_, gs) = PlaySoundSample SND_BEE DefaultVolume PAN_CENTER
                                          DEFAULT_FREQUENCY 0 gs
            = ((st, kill or), gs)
        = ((st, or), gs)

    newmove (st, or) gs
        # (turn, gs) = IRnd 30 gs
        # (xadd, gs) = RRnd 0.05 gs
        # (yadd, gs) = RRnd 0.085 gs
        # (skmv, gs) = IRnd 25 gs
        # rxv = (if (turn == 1) (~ or.speed.rx) (or.speed.rx)) + xadd
        # ryv = or.speed.ry + yadd + 0.005
        # or = {or & skipmove = skmv, speed = {rx=rxv, ry=ryv}}
        = ((st, or), gs)


/* ---------- frog ---------- */

FrogObject
    # obj = defaultGameObject OBJ_FROG size NoState
    # obj = { obj
            & sprites    = [FrogSprite, FrogJumpSprite]
            , init       = (newinit size NoState)
            , collide    = newcollide
            , animation  = newanimation
            }
    = obj
where
    size    = {w = 20, h = 20}

    newinit size state subtype pos time gs
        # (objrec, gs) = defaultObjectRec subtype pos size time gs
        # objrec = { objrec
                   & speed          = {rx = ~1.0, ry = 0.0}
                   , acceleration   = {rx = 0.0, ry = 1.0 / 16.0}
                   , bounce         = {fvx = Factor 1.0, fvy = Value 2.0}
                   , layer          = AtLayer LYR_FOREGROUND
                   , options        = { objrec.options
                                      & automirrorleftright = True
                                      }
                   , ownbounds      = BND_ENEMY
                   , bouncebounds   = BND_STATIC_BOUNDS + BND_ENEMY
                   , collidebounds  = BND_MAIN_CHARACTER + BND_WATER
                   , forgetdistance = {x = 8, y = 4}
                   }
        = ((state, objrec), gs)

    newanimation (st, or) gs
        # or = {or & currentsprite = if (or.speed.ry < ~0.5) 2 1}
        = ((st, or), gs)

    newcollide (st, or) bnds othertype otherobjrec gs
        | ((othertype == OBJ_WATER) && (bnds.top))
            # (_, gs) = Splash {x = or.pos.x, y = or.pos.y + H} gs
            = ((st, or), gs)
        | ((othertype == OBJ_MAIN_CHAR) && (bnds.bottom))
            # (_, gs) = PlaySoundSample SND_FROG HighVolume PAN_CENTER
                                          DEFAULT_FREQUENCY 0 gs
            = ((st, kill or), gs)
        = ((st, or), gs)


/* ---------- ending of the level ---------- */

EndingObject
    # obj = defaultGameObject OBJ_ENDING size NoState
    # obj = { obj
            & sprites   = [ EndingSprite 1, EndingSprite 2, EndingSprite 3
                          , EndingSprite 4, EndingSprite 5, EndingSprite 6
                          , EndingSprite 7, EndingSprite 8]
            , init      = (newinit size NoState)
            , collide   = newcollide
            , move      = newmove
            }
    = obj
where
    size    = {w = 32, h = 26}

    newinit size state subtype pos time gs
        # (objrec, gs) = defaultObjectRec subtype pos size time gs
        # objrec = { objrec
                   & layer           = AtLayer LYR_FOREGROUND
                   , ownbounds       = BND_ENDING
                   , collidebounds   = BND_MAIN_CHARACTER
                   , offset          = {x = 4, y = 7}
                   }
        = ((state, objrec), gs)

    newcollide (st, or) bnds othertype otherobjrec gs
        # instr = SND_FLUTE
        # vol = HighVolume
        # (_, gs) = PlaySoundSample instr vol PAN_CENTER (getnotefreq 107)  0 gs
        # (_, gs) = PlaySoundSample instr vol PAN_CENTER (getnotefreq 109) 10 gs
        # (_, gs) = PlaySoundSample instr vol PAN_CENTER (getnotefreq 104) 20 gs
        # (_, gs) = PlaySoundSample instr vol PAN_CENTER (getnotefreq 107) 30 gs
        # (_, gs) = PlaySoundSample instr vol PAN_CENTER (getnotefreq 109) 40 gs
        # (_, gs) = PlaySoundSample instr LowVolume PAN_LEFT  (getnotefreq 104) 49 gs
        # (_, gs) = PlaySoundSample instr LowVolume PAN_RIGHT (getnotefreq 107) 50 gs
        # (_, gs) = PlaySoundSample instr vol PAN_CENTER (getnotefreq 112) 51 gs
        = ((st, {or & skipmove = 0
                    , framecounter = 0
                    , collidebounds = 0}), gs)

    newmove (st, or) gs
        | or.framecounter > 196
             = ((st, {or & skipmove = ~1, collidebounds = BND_MAIN_CHARACTER}), gs)
        # or = {or & currentsprite = ((or.currentsprite +
                                         (add or.framecounter)) rem 8) + 1
                   , skipmove = or.framecounter / 50}
        = ((st, or), gs)
    where
        add x
            | x < 50 = 50 - x
            | otherwise = 0


/* ---------- statistics objects ---------- */

ST_X        :==  10
ST_COLON    :==  11
ST_DIAM     :==  12
ST_COIN     :==  13

StatHeartObject
    # obj = defaultGameObject OBJ_STAT size NoState
    # obj = { obj
            & sprites   = [StatusSprite 1, StatusSprite 2, StatusSprite 3,
                           StatusSprite 4, StatusSprite 5, StatusSprite 6]
            , init      = (newinit size NoState)
            , userevent = newuserevent
            }
    = obj
where
    size    = {w = 12, h = 12}

    newinit size state subtype pos time gs
        # (objrec, gs) = defaultObjectRec subtype pos size time gs
        # objrec = { objrec
                   & options = {objrec.options & static = True}
                   , layer = AtLayer LYR_STATUS
                   , currentsprite = if (subtype >= ST_X) (subtype - 7) 2
                   , ownbounds = BND_STAT
                   }
        = ((state, objrec), gs)

    newuserevent (st, or) ev evpar1 evpar2 gs
        | (ev == EV_HEALTH) && (or.subtype < 10)
            # or = {or & currentsprite = if (evpar1 < or.subtype) 1 2}
            = ((st, or), gs)
        = ((st, or), gs)


/* ---------- autoinit object ---------- */

/*
   this object is automatically initialized when the level starts
*/

AutoInitObject
    # obj = defaultGameObject OBJ_AUTOINIT size NoState
    # obj = { obj
            & init  = (newinit size NoState)
            }
    = obj
where
    size = {w = 1, h = 1}
    newinit size state subtype pos time gs
        # gs = setexitcode EC_QUIT gs   /* for esc key */
        # (_, gs) = CreateNewGameObject OBJ_STAT  1       {x = 181, y = STY} gs
        # (_, gs) = CreateNewGameObject OBJ_STAT  2       {x = 193, y = STY} gs
        # (_, gs) = CreateNewGameObject OBJ_STAT  3       {x = 205, y = STY} gs
        # (_, gs) = CreateNewGameObject OBJ_STAT ST_X     {x =  46, y = STY} gs
        # (_, gs) = CreateNewGameObject OBJ_STAT ST_COLON {x = 257, y = STY} gs
        # (_, gs) = CreateNewGameObject OBJ_STAT ST_DIAM  {x =  85, y = STY} gs
        # (_, gs) = CreateNewGameObject OBJ_STAT ST_X     {x =  94, y = STY} gs
        # (_, gs) = CreateNewGameObject OBJ_STAT ST_COIN  {x = 133, y = STY} gs
        # (_, gs) = CreateNewGameObject OBJ_STAT ST_X     {x = 142, y = STY} gs

        # (_, gs) = CreateNewGameObject OBJ_CLOUD 1 pos gs
        # (_, gs) = CreateNewGameObject OBJ_CLOUD 2 pos gs
        # (_, gs) = CreateNewGameObject OBJ_CLOUD 3 pos gs

        # (objrec, gs) = defaultObjectRec subtype pos size time gs
        # objrec = {objrec & active = False}
        = ((state, objrec), gs)


/* ---------- main character ---------- */

MC_IDLE  :==  1
MC_WALK  :==  2
MC_JUMP  :==  3
MC_FALL  :==  4
MC_SWIM  :==  5
MC_DEAD  :==  6

:: MainCharState
   = { action     :: Int
     , lastspeed1 :: RealXY
     , lastspeed2 :: RealXY
     , enemynote  :: Int
     , health     :: Int
     }

MainCharObject
    # obj = defaultGameObject OBJ_MAIN_CHAR size newstate
    # obj = { obj
            & sprites    = [ CharlieIdleSprite
                           , CharlieWalkSprite
                           , CharlieJumpSprite
                           , CharlieFallSprite
                           , CharlieSwimSprite
                           , CharlieDeadSprite
                           ]
            , init       = (newinit size newstate)
            , keydown    = newkeydown
            , keyup      = newkeyup
            , animation  = newanimation
            , collide    = newcollide
            , userevent  = newuserevent
            }
    = obj
where
    size     = {w = 20, h = 32}
    newstate = { action = MC_IDLE
               , lastspeed1 = zero
               , lastspeed2 = zero
               , enemynote = 0
               , health = 3
               }

    ac = 1.0 / 5.0
    maxwalkspeed = {rx = 2.0, ry = 6.0}
    maxswimspeed = {maxwalkspeed & rx = 0.5}
    normaloffset = {x = ~2, y = ~2}

    newinit size state subtype pos time gs
        # pos = {x = pos.x + W / 2, y = pos.y + H - size.h}
        # (objrec, gs) = defaultObjectRec subtype pos size time gs
        # objrec = { objrec
                   & offset         = normaloffset
                   , acceleration   = {rx = 0.0, ry = 1.0 / 8.0}
                   , bounce         = {fvx = Value 0.0, fvy = Factor (1.0 / 16.0)}
                   , maxspeed       = maxwalkspeed
                   , slowdown       = {fvx = Factor (1.0 / 16.0), fvy = Value 0.0}
                   , layer          = AtLayer LYR_PLAYER
                   , options        = { objrec.options
                                      & checkkeyboard       = True
                                      , allowkeyboardrepeat = False
                                      , automirrorleftright = True
                                      }
                   , ownbounds      = BND_MAIN_CHARACTER
                   , bouncebounds   = BND_STATIC_BOUNDS
                   , collidebounds  = BND_ENEMY + BND_WATER + BND_ENDING +
                                         BND_POWER_UP + BND_BLOCKS + BND_KILL
                   , forgetdistance = {x = 10000, y = 10000}
                   }
        # (_, gs) = CreateObjectFocus
                         { scrollleft      = 132
                         , scrollup        =  50
                         , scrollright     = 132
                         , scrolldown      =  52
                         , maxxscrollspeed =   2
                         , maxyscrollspeed =   3
                         } gs
        = ((state, objrec), gs)

    newanimation (st=:{action = act}, or=:{offset = ofs}) gs
        # xstuck = ((or.speed.rx == st.lastspeed1.rx) &&
                    (or.speed.rx == st.lastspeed2.rx) &&
                    ((toInt or.speed.rx) == 0)
                   )
        # ystuck = ((or.speed.ry == st.lastspeed1.ry) &&
                    (or.speed.ry == st.lastspeed2.ry) &&
                    ((toInt or.speed.ry) == 0)
                   )
        # oldact = act
        # (act, ofs) = case act of
            MC_WALK   -> if xstuck
                             ((if (or.speed.ry > sp) MC_FALL MC_IDLE), ofs)
                             ((if (or.speed.ry > sp) MC_FALL MC_WALK), ofs)
            MC_JUMP   -> (if (or.speed.ry > sp) MC_FALL
                              (if (xstuck && ystuck)
                                  MC_IDLE
                                  MC_JUMP), ofs)
            MC_FALL   -> (if (or.speed.ry > sp) MC_FALL MC_WALK, ofs)
            MC_SWIM   -> (MC_SWIM, {ofs & y = if (ofs.y == 4) 3 (ofs.y + 1)})
            otherwise -> (MC_IDLE, ofs)
        # st = {st & lastspeed2 = st.lastspeed1}
        # st = {st & lastspeed1 = or.speed}
        | (act == MC_WALK) && (oldact == MC_FALL)
            # (_, gs) = PlaySoundSample SND_PLOF LowVolume
                       PAN_CENTER DEFAULT_FREQUENCY 0 gs
            # st = {st & enemynote = 0}
            = (({st & action = act}, {or & currentsprite = act, offset = ofs}), gs)
        = (({st & action = act}, {or & currentsprite = act, offset = ofs}), gs)
    where
        sp = 1.0 / 4.0

    newkeydown (st=:{action}, or) key gs
        | key == GK_LEFT
            # newaction = if (action == MC_IDLE) MC_WALK action
            = (({st & action = newaction},
               {or & acceleration.rx = or.acceleration.rx - ac, currentsprite = newaction}), gs)
        | key == GK_RIGHT
            # newaction = if (action == MC_IDLE) MC_WALK action
            = (({st & action = newaction},
               {or & acceleration.rx = or.acceleration.rx + ac, currentsprite = newaction}), gs)
        | key == GK_SPACE
            | (isMember action [MC_IDLE, MC_WALK, MC_SWIM])
                # act = action
                # (_, gs) = PlaySoundSample SND_JUMP DefaultVolume
                                PAN_CENTER DEFAULT_FREQUENCY 0 gs
                # ((st, or), gs) = (({st & action = MC_JUMP},
                                     {or & speed = (jumpspeed or.speed)
                                         , currentsprite = MC_JUMP
                                         , offset = normaloffset
                                         , maxspeed = maxwalkspeed
                                         }), gs)
                | (act == MC_SWIM)
                    # (_, gs) = PlaySoundSample SND_WATER_JUMP DefaultVolume
                                     PAN_CENTER DEFAULT_FREQUENCY 0 gs
                    # (_, gs) = Splash {x = or.pos.x, y = or.pos.y + 32} gs
                    = ((st, or), gs)
                = ((st, or), gs)
            = ((st, or), gs)
        | otherwise = ((st, or), gs)
    where
        jumpspeed :: RealXY -> RealXY
        jumpspeed sp=:{rx, ry} = {rx = rx, ry = ry - 4.35 - abs (rx) / 3.0}
        setaction :: Int Int -> Int
        setaction cur new = if ((cur == MC_WALK) ||
                                (cur == MC_IDLE))
                                   new
                                   cur

    newkeyup (st, or) key gs
        | key == GK_LEFT   = ((st, {or & acceleration.rx = or.acceleration.rx + ac}), gs)
        | key == GK_RIGHT  = ((st, {or & acceleration.rx = or.acceleration.rx - ac}), gs)
        | otherwise        = ((st, or), gs)

    newuserevent (st, or) ev evpar1 evpar2 gs
        | ev == EV_QUIT_LEVEL
            # gs = setexitcode evpar1 gs
            # gs = quitlevel gs
            = ((st, or), gs)
        | ev == EV_GAME_OVER
            = ((st, or), setgameover gs)
        | ev == EV_STOP_BLINKING
            = ((st, {or & displayoptions.blink = False}), gs)
        | ev == EV_STOP_MOVING
            = ((st, {or & speed = zero, acceleration = zero}), gs)
        = ((st, or), gs)

    newcollide (st, or) bnds othertype otherobjrec gs
        | (othertype == OBJ_WATER)
            | (st.action == MC_SWIM)
                = ((st, or), gs)
            # pos = {x = or.pos.x, y = otherobjrec.pos.y}
            # (_, gs) = Splash pos gs
            # (_, gs) = PlaySoundSample SND_SPLASH DefaultVolume
                                PAN_CENTER DEFAULT_FREQUENCY 0 gs
            = (({st & action = MC_SWIM}, 
                {or & currentsprite = MC_SWIM
                    , maxspeed = maxswimspeed
                    , offset = {normaloffset & y = 2} })
                  , gs)
        | othertype == OBJ_PIN
            # st = {st & health = 0}
            = hurt (st, or) gs
        | othertype == OBJ_BOUNCEBLOCK
            | bnds.top
                # (_, gs) = PlaySoundSample SND_WATER_JUMP DefaultVolume
                              PAN_CENTER 70000 0 gs
                = (({st & action = MC_JUMP}, {or & speed.ry = ~(abs or.speed.ry) * 2.0 - 5.25,
                                                   currentsprite = MC_JUMP}), gs)
            | bnds.bottom
                # (_, gs) = PlaySoundSample SND_WATER_JUMP DefaultVolume
                              PAN_CENTER 60000 0 gs
                = (({st & action = MC_FALL}, {or & speed.ry = or.speed.ry + 3.0,
                                                   currentsprite = MC_FALL}), gs)
            = ((st, or), gs)
        | othertype == OBJ_ENDING
            # or = {or & acceleration.rx = ac
                       , collidebounds = BND_POWER_UP
                       , options = {or.options & checkkeyboard = False
                                               , ignorelevelbounds = True}}
            # (_, gs) = CreateUserGameEvent EV_STOP_MOVING 0 0 Self ANY_SUBTYPE 125 gs
            # (_, gs) = CreateObjectFocus {zero & scrollright = 160
                                                , maxxscrollspeed = 1} gs
            # (_, gs) = CreateUserGameEvent EV_QUIT_LEVEL EC_SUCCESS 0 Self ANY_SUBTYPE 800 gs
            = ((st, or), gs)
        | othertype == OBJ_HEART
            | not (st.health < 3)
                = ((st, or), gs)
            # st = {st & health = st.health + 1}
            # (_, gs) = CreateUserGameEvent EV_HEALTH st.health 0
                          (BoundType BND_STAT) ANY_SUBTYPE 1 gs
            = ((st, or), gs)
        | bnds.top
            | (not (otherobjrec.ownbounds bitand BND_BLOCKS == 0))
                # (freq, gs) = IRRnd 4000 gs
                # (_, gs) = PlaySoundSample SND_CRATE VeryLowVolume
                                PAN_CENTER (12000 + freq) 0 gs
                = (({st & action = MC_JUMP}, {or & speed.ry = ~4.25,
                                                        currentsprite = MC_JUMP}), gs)
            | (not (otherobjrec.ownbounds bitand BND_ENEMY == 0))
                # gs = addscore (100 * (2 << st.enemynote) / 2) gs
                # (_, gs) = PlaySoundSample SND_HIT HighVolume
                              PAN_CENTER (getnotefreq
                              (MIDDLE_C + 76 + 2 * st.enemynote)) 0 gs
                # st = {st & enemynote = st.enemynote + 1}
                # pos = {x = (or.pos.x + otherobjrec.pos.x) / 2, y = or.pos.y + 20}
                # (_, gs) = CreateNewGameObject OBJ_FLASH 0 pos gs
                = (({st & action = MC_JUMP}, {or & speed.ry = ~4.35, 
                                                        currentsprite = MC_JUMP}), gs)
            = ((st, or), gs)
        | (not (otherobjrec.ownbounds bitand BND_ENEMY == 0))
            | or.displayoptions.blink
                = ((st, or), gs)
            = hurt (st, or) gs
        = ((st, or), gs)

    hurt (st, or) gs
            # st = {st & health = st.health - 1}
            # (_, gs) = CreateUserGameEvent EV_HEALTH st.health 0 
                          (BoundType BND_STAT) ANY_SUBTYPE 1 gs
            # (_, gs) = PlaySoundSample SND_AU HighVolume PAN_CENTER 48000  0 gs
            # (_, gs) = PlaySoundSample SND_AU HighVolume PAN_CENTER 46000  4 gs
            # (_, gs) = PlaySoundSample SND_AU HighVolume PAN_CENTER 42000  7 gs
            # (_, gs) = PlaySoundSample SND_AU HighVolume PAN_CENTER 38000  9 gs
            # (_, gs) = PlaySoundSample SND_AU HighVolume PAN_CENTER 30000 10 gs
            | st.health == ~1
                # st = {st & action = MC_DEAD}
                # or = {or & currentsprite = MC_DEAD
                           , speed = {rx = 0.0, ry = ~3.25}
                           , acceleration = {rx = 0.0, ry = 1.0 / 24.0}
                           , ownbounds = 0
                           , collidebounds = 0
                           , bouncebounds = 0
                           , layer = InFront
                           , options = {or.options & checkkeyboard = False
                                                   , ignorelevelbounds = True}}
                # (_, gs) = CreateUserGameEvent EV_STOP_MOVING 0 0 Self ANY_SUBTYPE 500 gs
                # (_, gs) = CreateObjectFocus zero gs
                # (_, gs) = CreateUserGameEvent EV_QUIT_LEVEL EC_FAILURE
                                        0 Self ANY_SUBTYPE 600 gs
                # (l, gs) = getlives gs
                | l == 0
                    # (_, gs) = CreateUserGameEvent EV_GAME_OVER 0 0 Self ANY_SUBTYPE 350 gs
                    = ((st, or), gs)
                = ((st, or), gs)
            # (_, gs) = CreateUserGameEvent EV_STOP_BLINKING 0 0 Self ANY_SUBTYPE 225 gs
            # or = {or & displayoptions.blink = True}
            = ((st, or), gs)

/* ---------- Menu object ---------- */

:: MenuState
   = { selected :: Int   /* menu options: 1, 2, ... max */
     , max      :: Int
     }

AutoMenuObject
    # obj = defaultGameObject OBJ_AUTOINIT size newstate
    # obj = { obj
            & sprites = [PointerSprite]
            , init = (newinit size newstate)
            , keydown = newkeydown
            }
    = obj
where
    size = DEFAULT_SIZE
    newstate = {selected = 1, max = 2}

    newinit size state subtype pos time gs
        # gs = setexitcode EC_QUIT gs   /* for esc key */
        # (objrec, gs) = defaultObjectRec subtype pos size time gs
        # objrec = {objrec & pos = {x = 126, y = 110}
                           , options = {objrec.options & static = True
                                                       , checkkeyboard = True}
                   }
    = ((state, objrec), gs)

    newkeydown (st, or) k gs
        | k == GK_DOWN
            | st.selected >= st.max
                = ((st, or), gs)
            # st = {st & selected = st.selected + 1}
            = ((st, {or & offset = {x = 0, y = 12 * (st.selected - 1)}}), gs)
        | k == GK_UP
            | st.selected <= 1
                = ((st, or), gs)
            # st = {st & selected = st.selected - 1}
            = ((st, {or & offset = {x = 0, y = 12 * (st.selected - 1)}}), gs)
        | (k == GK_SPACE) || (k == GK_RETURN)
            | st.selected == 1
                # gs = setexitcode EC_SUCCESS gs
                = ((st, or), quitlevel gs)
            = ((st, or), quitlevel gs)
        = ((st, or), gs)

/* ---------- Title screen ---------- */

TitleScreen
  = { boundmap     = { map = TitleBounds
                     , blocksize = DEFAULT_SIZE
                     , objstart  = OBJ_START
                     , startobjx = 4
                     , startobjy = 4
                     }
    , initpos      = {x = 0, y = 13 * H + H / 2}
    , layers       = [TitleBackground, TitleLayer]
    , objects      = [{MainCharObject & keydown = nop, keyup = nop}, AutoMenuObject]
    , music        = Just BackgroundMusic
    , soundsamples = []
    , leveloptions = { fillbackground = Nothing
                     , escquit        = True
                     , debugscroll    = False
                     , fadein         = False
                     , fadeout        = False
                     }
    }
where
    nop = \(st, or) k gs -> ((st, or), gs)

TitleLayer =
    { bmp       = TitleBitmap
    , layermap  = TitleMap
    , sequences = []
    , movement  = defaultMovement
    }

TitleBackground =
    { bmp       = TitleBackgrBitmap
    , layermap  = [{1}]
    , sequences = []
    , movement  = defaultScrollMovement 3
    }


/* ---------- level 1 ---------- */

GameLevel1
  = { boundmap     = { map = Level1Bounds
                     , blocksize = DEFAULT_SIZE
                     , objstart  = OBJ_START
                     , startobjx = 4
                     , startobjy = 4
                     }
    , initpos      = {x = 0, y = 13 * H + H / 2}
    , layers       = [Level1Background, Level1Layer]
    , objects      = GameObjectList
    , music        = Nothing // Just BackgroundMusic
    , soundsamples = GameSoundSampleList
    , leveloptions = { fillbackground = Nothing
                     , escquit        = True
                     , debugscroll    = False
                     , fadein         = True
                     , fadeout        = True
                     }
    }

Level1Layer =
    { bmp       = Level1Bitmap
    , layermap  = Level1Map
    , sequences = []
    , movement  = defaultMovement
    }

Level1Background =
    { bmp       = ForestBackground
    , layermap  = [{1}]
    , sequences = []
    , movement  = defaultScrollMovement 3
    }

/* ---------- level 2 ---------- */

GameLevel2
  = { boundmap     = { map = Level2Bounds
                     , blocksize = DEFAULT_SIZE
                     , objstart  = OBJ_START
                     , startobjx = 4
                     , startobjy = 4
                     }
    , initpos      = {x = 0, y = 13 * H + H / 2}
    , layers       = [Level2Background, Level2Layer]
    , objects      = GameObjectList
    , music        = Nothing // Just BackgroundMusic
    , soundsamples = GameSoundSampleList
    , leveloptions = { fillbackground = Nothing
                     , escquit        = True
                     , debugscroll    = False
                     , fadein         = True
                     , fadeout        = True
                     }
    }

Level2Layer =
    { bmp       = Level2Bitmap
    , layermap  = Level2Map
    , sequences = []
    , movement  = defaultMovement
    }

Level2Background =
    { bmp       = NightBackground
    , layermap  = [{1}]
    , sequences = []
    , movement  = defaultScrollMovement 3
    }



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

/* increment the number of diamonds */
incdiamonds gs = appGSt incgstdiamonds gs
where
    incgstdiamonds :: GameState -> GameState
    incgstdiamonds gst = {gst & diamonds = gst.diamonds + 1}

/* increment the number of coins */
inccoins gs = appGSt incgstcoins gs
where
    incgstcoins :: GameState -> GameState
    incgstcoins gst = {gst & coins = gst.coins + 1}

/* add a value to the score */
addscore points gs = appGSt (addgstscore points) gs
where
    addgstscore :: Int GameState -> GameState
    addgstscore points gst = {gst & score = gst.score + points}


/* gameover functions */
setgameover gs = appGSt setgstgameover gs
setgstgameover :: GameState -> GameState
setgstgameover gst = {gst & gameover = True}

/*
getgameover gs = accGSt getgstgameover gs
getgstgameover :: GameState -> (Bool, GameState)
getgstgameover gst = (gst.gameover, gst)
*/

/* ---------- random functions ---------- */

/* get random integer value 0..n */
IRnd n gs
    = (Rand rem n, gs)

/* get random integer value ~n..n */
IRRnd n gs
    = ((Rand rem n) - (Rand rem n), gs)

/* get random Real value ~n..n */
RRnd n gs =
    (n * (((toReal Rand) / max) - ((toReal Rand) / max)), gs)
where
    max = (toReal MaxRand)

Rand = 0; MaxRand = 255	// PA: dit moet opnieuw geimplementeerd worden, Rand kwam uit intrface_12 en dat mag niet.

/* ---------- music and sounds ---------- */

BackgroundMusic
  = { musicfile = "Charlie.mid"
    , restart   = True
    , continue  = False
    }

HighVolume    = (9 * MAX_VOLUME / 10)
DefaultVolume = (8 * MAX_VOLUME /  9)
LowVolume     = (7 * MAX_VOLUME /  8)
VeryLowVolume = (5 * MAX_VOLUME /  6)

RandomPan gs = IRRnd (PAN_RIGHT / 2) gs


SND_JUMP            :==   1
SND_WATER_JUMP      :==   2
SND_SPLASH          :==   3
SND_COIN            :==   4
SND_CRATE           :==   5
SND_ENEMY           :==   6
SND_PLOF            :==   7
SND_HIT             :==   8
SND_FROG            :==   9
SND_BEE             :==  10
SND_XYLOFOON        :==  11
SND_FLUTE           :==  12
SND_CLARINET        :==  13
SND_AU              :==  14

GameSoundSampleList =
  [ { soundid = SND_JUMP        , soundfile = "JUMP.WAV"     , soundbuffers =  1 }
  , { soundid = SND_SPLASH      , soundfile = "WATER.WAV"    , soundbuffers =  3 }
  , { soundid = SND_WATER_JUMP  , soundfile = "WATERJMP.WAV" , soundbuffers =  8 }
  , { soundid = SND_COIN        , soundfile = "COIN.WAV"     , soundbuffers = 20 }
  , { soundid = SND_CRATE       , soundfile = "CRATE.WAV"    , soundbuffers =  2 }
  , { soundid = SND_ENEMY       , soundfile = "ENEMY.WAV"    , soundbuffers =  3 }
  , { soundid = SND_PLOF        , soundfile = "PLOF.WAV"     , soundbuffers =  1 }
  , { soundid = SND_HIT         , soundfile = "HIT.WAV"      , soundbuffers =  3 }
  , { soundid = SND_FROG        , soundfile = "FROG.WAV"     , soundbuffers =  2 }
  , { soundid = SND_BEE         , soundfile = "BEE.WAV"      , soundbuffers =  2 }
  , { soundid = SND_XYLOFOON    , soundfile = "XYLOFOON.WAV" , soundbuffers =  8 }
  , { soundid = SND_FLUTE       , soundfile = "FLUTE.WAV"    , soundbuffers =  8 }
  , { soundid = SND_CLARINET    , soundfile = "CLARINET.WAV" , soundbuffers =  8 }
  , { soundid = SND_AU          , soundfile = "AU.WAV"       , soundbuffers =  5 }
  ]



/* ---------- bitmaps and sprites ---------- */

TitleBackgrBitmap :: GameBitmap
TitleBackgrBitmap
  = { bitmapname  = "TitleBackground.BMP"
    , unitsize    = {w = 320, h = 320}
    , dimensions  = (1, 1)
    , transparent = Nothing
    }

ForestBackground :: GameBitmap
ForestBackground
  = { bitmapname  = "ForestBackground.BMP"
    , unitsize    = {w = 320, h = 320}
    , dimensions  = (1, 1)
    , transparent = Nothing
    }

NightBackground :: GameBitmap
NightBackground
  = { bitmapname  = "NightBackground.BMP"
    , unitsize    = {w = 320, h = 320}
    , dimensions  = (1, 1)
    , transparent = Nothing
    }

CloudSprite :: Sprite
CloudSprite
  = { bitmap   = CloudsBitmap
    , sequence = BitmapSequence 3 2 50
    , loop     = True
    }

CrateSprite :: Sprite
CrateSprite
  = { bitmap   = ObjectsBitmap
    , sequence = BitmapSequence 1 2 60
    , loop     = True
    }

CratePartSprite :: Sprite
CratePartSprite
  = { bitmap   = PartBitmap
    , sequence = [(1, 100)]
    , loop     = True
    }


DiamondSprite :: Sprite
DiamondSprite
  = { bitmap   = ObjectsBitmap
    , sequence = BitmapSequence 3 2 55
    , loop     = True
    }

CoinSprite :: Int -> Sprite
CoinSprite speed
  = { bitmap   = ObjectsBitmap
    , sequence = BitmapSequence 5 8 speed
    , loop     = True
    }

GlitterSprite :: Int -> Sprite
GlitterSprite speed
  = { bitmap   = ObjectsBitmap
    , sequence = BitmapSequence 13 4 speed
    , loop     = False
    }

LifeSprite :: Sprite
LifeSprite
  = { bitmap   = ObjectsBitmap
    , sequence = BitmapSequence 17 2 50
    , loop     = True
    }

HeartSprite :: Sprite
HeartSprite
  = { bitmap   = ObjectsBitmap
    , sequence = BitmapSequence 19 2 50
    , loop     = True
    }


EnemySprite1 :: Sprite
EnemySprite1
  = { bitmap   = EnemyBitmap
    , sequence = BitmapSequence 1 2 35
    , loop     = True
    }

EnemySprite2 :: Sprite
EnemySprite2
  = { bitmap   = EnemyBitmap
    , sequence = BitmapSequence 3 2 35
    , loop     = True
    }

FlashSprite :: Sprite
FlashSprite
  = { bitmap   = EnemyBitmap
    , sequence = BitmapSequence 5 5 3
    , loop     = False
    }

BeeSprite :: Sprite
BeeSprite
  = { bitmap   = BeesBitmap
    , sequence = [(1, 1), (2, 1), (3, 1), (2, 1)]
    , loop     = True
    }

CharlieIdleSprite :: Sprite
CharlieIdleSprite
  = { bitmap   = MainCharBitmap
    , sequence = [(1, 500), (7, 40), (1, 1000), (7, 35), (1, 80), (7, 45)]
    , loop     = False
    }

CharlieWalkSprite :: Sprite
CharlieWalkSprite
  = { bitmap   = MainCharBitmap
    , sequence = [(2, 15), (1, 15)]
    , loop     = False
    }

CharlieJumpSprite :: Sprite
CharlieJumpSprite
  = { bitmap   = MainCharBitmap
    , sequence = [(3, 2)]
    , loop     = False
    }

CharlieFallSprite :: Sprite
CharlieFallSprite
  = { bitmap   = MainCharBitmap
    , sequence = [(4, 2)]
    , loop     = False
    }

CharlieSwimSprite :: Sprite
CharlieSwimSprite
  = { bitmap   = MainCharBitmap
    , sequence = [(5, 16)]
    , loop     = False
    }

CharlieDeadSprite :: Sprite
CharlieDeadSprite
  = { bitmap   = MainCharBitmap
    , sequence = [(6, 1000)]
    , loop     = True
    }

PalmSprite :: Int -> Sprite
PalmSprite n
  = { bitmap   = PalmFrontBitmap
    , sequence = BitmapSequence (4 + n - 1) 1 5000
    , loop     = True
    }

BounceBlockSprite :: Sprite
BounceBlockSprite
  = { bitmap   = ObjectsBitmap
    , sequence = [(25, 1000)]
    , loop     = True
    }

BounceBlockShortSprite :: Sprite
BounceBlockShortSprite
  = { bitmap   = ObjectsBitmap
    , sequence = [(25, 2)]
    , loop     = False
    }

PointerSprite :: Sprite
PointerSprite
  = { bitmap   = ObjectsBitmap
    , sequence = [(27, 1000)]
    , loop     = True
    }

FrogSprite :: Sprite
FrogSprite
  = { bitmap   = FrogsBitmap
    , sequence = [(5, 5)]
    , loop     = False
    }

FrogJumpSprite :: Sprite
FrogJumpSprite
  = { bitmap   = FrogsBitmap
    , sequence = [(6, 5)]
    , loop     = False
    }

WaterSprite :: Sprite
WaterSprite
  = { bitmap   = WaterBitmap
    , sequence = BitmapSequence 1 10 3
    , loop     = True
    }

WaveSprite :: Sprite
WaveSprite
  = { bitmap   = WaterBitmap
    , sequence = BitmapSequence 11 5 12
    , loop     = False
    }

SplashSprite :: Sprite
SplashSprite
  = { bitmap   = WaterBitmap
    , sequence = BitmapSequence 16 5 10
    , loop     = False
    }

EndingSprite :: Int -> Sprite
EndingSprite n
  = { bitmap   = EndingBitmap
    , sequence = [(n, 10000)]
    , loop     = True
    }

InFrontSprite :: Int -> Sprite
InFrontSprite n
  = { bitmap   = InFrontBitmap
    , sequence = [(n, 10000)]
    , loop     = True
    }

StatusSprite :: Int -> Sprite
StatusSprite n
  = { bitmap   = StatusBitmap
    , sequence = [(n, 10000)]
    , loop     = True
    }

BitmapSequence :: Int Int Int -> [(Int, Int)]
BitmapSequence start count speed
    | count == 0  = []
    | otherwise   = [(start, speed)] ++
                        (BitmapSequence (start + 1) (count - 1) speed)

/* ---------- statistics ---------- */

TitleText :: Statistic
TitleText
  = { format    = "CHARLIE THE DUCK"
    , value     = Nothing
    , position  = {x = ~1, y = 24}
    , style     = BigStyle
    , color     = RGB {r = 255, g = 240, b = 180}
    , shadow    = Nothing
    , alignment = {xyfromscreencenter = (True, False), xycentered = (True, False)}
    }

TitleTextShadow :: Statistic
TitleTextShadow = {TitleText & position = {x = 0, y = 25}
                             , color = RGB {r = 180, g = 128, b = 32}
                             , shadow = Just TitleShadow}

BigStyle :: Style
BigStyle
  = { fontname = "Arial"
    , fontsize = 31
    , bold     = True
    , italic   = True
    }

TitleShadow :: Shadow
TitleShadow
  = { shadowpos   = {x = 1, y = 1}
    , shadowcolor = Black
    }

DemoText :: Statistic
DemoText
  = { format    = "Concurrent Clean Game Library demo"
    , value     = Nothing
    , position  = {x = 0, y = 60}
    , style     = StatStyle
    , color     = White
    , shadow    = Just StatShadow
    , alignment = {xyfromscreencenter = (True, False), xycentered = (True, False)}
    }

Copyright :: Statistic
Copyright
  = { format    = "(C) Copyright 1999, Mike Wiering"
    , value     = Nothing
    , position  = {x = 0, y = 72}
    , style     = StatStyle
    , color     = White
    , shadow    = Just StatShadow
    , alignment = {xyfromscreencenter = (True, False), xycentered = (True, False)}
    }

MenuText :: Int String -> Statistic
MenuText i s
  = { format    = s
    , value     = Nothing
    , position  = {x = 150, y = 112 + 12 * i}
    , style     = StatStyle
    , color     = White
    , shadow    = Just StatShadow
    , alignment = zero
    }

GameOver :: Statistic
GameOver
  = { format    = "GAME OVER"
    , value     = Nothing
    , position  = {x = 0, y = 0}
    , style     = BigStyle
    , color     = White
    , shadow    = Just StatShadow
    , alignment = {xyfromscreencenter = (True, True), xycentered = (True, True)}
    }


STY :== 7 // 221
STS :== 5 // (~7)

Lives :: Int -> Statistic
Lives n
  = { format    = "Charlie   %2d"
    , value     = Just n
    , position  = {x = 10, y = STS}
    , style     = StatStyle
    , color     = White
    , shadow    = Just StatShadow
    , alignment = zero
    }

Diamonds :: Int -> Statistic
Diamonds n
  = { format    = "%d"
    , value     = Just n
    , position  = {x = 113, y = STS}
    , style     = StatStyle
    , color     = White
    , shadow    = Just StatShadow
    , alignment = {zero & xycentered = (True, False)}
    }

Coins :: Int -> Statistic
Coins n
  = { format    = "%d"
    , value     = Just n
    , position  = {x = 161, y = STS}
    , style     = StatStyle
    , color     = White
    , shadow    = Just StatShadow
    , alignment = {zero & xycentered = (True, False)}
    }

Score :: Int -> Statistic
Score n
  = { format    = "Score  %07d"
    , value     = Just n
    , position  = {x = ~10, y = STS}
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
