implementation module gamehandle

//  This module defines the internal representation of a game

from	StdList	import map
import	StdGameDef

:: GameHandle gs
   = { levels`     :: [LevelHandle (GSt gs)]             // levels
     , quitlevel`  :: (GSt gs) -> (Bool, GSt gs)         // when true, the game engine quits
     , nextlevel`  :: (GSt gs) -> (Int, GSt gs)          // 1,2,... level in list, 0 = exit
     , statistics` :: (GSt gs) -> ([Statistic], GSt gs)  // all text items
     }

:: LevelHandle state
   = { boundmap`      :: BoundMap               // map of all static bounds in a level
     , initpos`       :: Point2                 // center of screen in boundmap
     , layers`        :: [Layer]                // all visible (scrolling) layers
                                                //   [back ... front]
     , objects`       :: [ObjectHandle state]   // all other objects in the level
     , music`         :: Maybe Music            // background music
     , soundsamples`  :: [SoundSample]
     , leveloptions`  :: LevelOptions
     }

:: ObjectHandle gs
   = E.state:
     { objecttype` :: ObjectType
     , sprites`    :: [Sprite]
     , spriteids`  :: [SpriteID]
     , instances`  :: [(InstanceID, state)]
     , init`       :: !SubType !Point2 !GameTime !gs -> *(!*(state, ObjectRec), !gs)
     , done`       :: !*(state, ObjectRec) !gs -> gs // *(!*(ObjectType, SubType), !gs)
     , move`       :: !*(state, ObjectRec) !gs -> *(!*(state, ObjectRec), !gs)
     , animation`  :: !*(state, ObjectRec) !gs -> *(!*(state, ObjectRec), !gs)
     , touchbound` :: !*(state, ObjectRec) !DirectionSet !MapCode !gs -> *(!*(state, ObjectRec), !gs)
     , collide`    :: !*(state, ObjectRec) !DirectionSet !ObjectType !ObjectRec !gs -> *(!*(state, ObjectRec), !gs)
     , frametimer` :: !*(state, ObjectRec) !gs -> *(!*(state, ObjectRec), !gs)
     , keydown`    :: !*(state, ObjectRec) !KeyCode !gs -> *(!*(state, ObjectRec), !gs)
     , keyup`      :: !*(state, ObjectRec) !KeyCode !gs -> *(!*(state, ObjectRec), !gs)
     , userevent`  :: !*(state, ObjectRec) !EventType !EventPar !EventPar !gs -> *(!*(state, ObjectRec), !gs)
     }

:: InstanceID
   :== Int



createObjectHandle :: (Object .gs) -> ObjectHandle .gs
createObjectHandle {objecttype, sprites, init, done, move, animation, touchbound,
                    collide, frametimer, keydown, keyup, userevent}
    = { objecttype` = objecttype
      , sprites`    = sprites
      , spriteids`  = []
      , instances`  = []
      , init`       = init
      , done`       = done
      , move`       = move
      , animation`  = animation
      , touchbound` = touchbound
      , collide`    = collide
      , frametimer` = frametimer
      , keydown`    = keydown
      , keyup`      = keyup
      , userevent`  = userevent
      }

createLevelHandle :: (Level .gs) -> LevelHandle .gs
createLevelHandle {boundmap, initpos, layers, objects, music, soundsamples, leveloptions}
    = { boundmap`      = boundmap
      , initpos`       = initpos
      , layers`        = layers
      , objects`       = map createObjectHandle objects
      , music`         = music
      , soundsamples`  = soundsamples
      , leveloptions`  = leveloptions
      }

createGameHandle :: (Game .gs) -> GameHandle .gs
createGameHandle {levels, quitlevel, nextlevel, statistics}
    = { levels`     = map createLevelHandle levels
      , quitlevel`  = quitlevel
      , nextlevel`  = nextlevel
      , statistics` = statistics
      }
