definition module gamehandle

//  this module defines the internal representation of a game

import StdGameDef

:: GameHandle gs
   = { levels`    :: ![LevelHandle (GSt gs)]       // levels
     , quitlevel` :: !St (GSt gs) Bool             // when true, the game engine quits
     , nextlevel` :: !St (GSt gs) Int              // 1,2,... level in list, 0 = exit
     , textitems` :: !St (GSt gs) [GameText]       // all text items
     }

:: LevelHandle state
   = { boundmap`      :: !BoundMap                   // map of all static bounds in a level
     , initpos`       :: !Point2                     // center of screen in boundmap
     , layers`        :: ![Layer]                    // all layers [back..front]
     , objects`       :: ![GameObjectHandle state]   // all other objects in the level
     , music`         :: !Maybe Music                // background music
     , soundsamples`  :: ![SoundSample]
     , leveloptions`  :: !LevelOptions
     }

:: GameObjectHandle gs
   = E. state:
     { objectcode` :: !ObjectCode
     , sprites`    :: ![Sprite]
     , spriteids`  :: ![SpriteID]
     , instances`  :: ![(InstanceID, state)]
     , init`       :: !SubCode !Point2 !GameTime !gs         -> GameObjectState state gs
     , done`       :: !(GameObjectState state gs)            -> gs
     , move`       :: !                                         ObjectFun state gs
     , animation`  :: !                                         ObjectFun state gs
     , touchbound` :: !DirectionSet MapCode                  -> ObjectFun state gs
     , collide`    :: !DirectionSet ObjectCode GameObjectRec -> ObjectFun state gs
     , frametimer` :: !                                         ObjectFun state gs
     , keydown`    :: !KeyCode                               -> ObjectFun state gs
     , keyup`      :: !KeyCode                               -> ObjectFun state gs
     , userevent`  :: !EventCode !EventPar !EventPar         -> ObjectFun state gs
     }

:: InstanceID
   :== Int

createObjectHandle :: !(GameObject .gs) -> GameObjectHandle .gs

createLevelHandle  :: !(Level .gs)      -> LevelHandle .gs

createGameHandle   :: !(Game .gs)       -> GameHandle .gs
