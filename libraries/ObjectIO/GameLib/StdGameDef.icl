implementation module StdGameDef

from	StdOverloaded	import zero
from	StdString		import String
from	StdIOBasic		import Point2, Size
from	StdMaybe		import Maybe, Just, Nothing
from	StdPictureDef	import Colour,
								RGB, RGBColour, Black, White, 
								DarkGrey, Grey, LightGrey, 
								Red, Green, Blue, 
								Cyan, Magenta, Yellow
import	StdGSt

:: GameAttribute gs
   = ScreenSize Size     // screen resolution, default 320x240
   | ColorDepth Int      // screen color depth, default 8 (256 colors)

:: Game gs
   = { levels      :: [Level (GSt gs)]                   // levels
     , quitlevel   :: (GSt gs) -> (Bool, GSt gs)         // when true, the game engine quits
     , nextlevel   :: (GSt gs) -> (Int, GSt gs)          // 1,2,... level in list, 0 = exit
     , statistics  :: (GSt gs) -> ([Statistic], GSt gs)  // all text items
     }

:: Level state
   = { boundmap      :: !BoundMap          // map of all static bounds in a level
     , initpos       :: !Point2            // center of screen in boundmap
     , layers        :: ![Layer]           // all visible (scrolling) layers
                                           //   [back ... front]
     , objects       :: ![Object state]    // all other objects in the level
     , music         :: !Maybe Music       // background music
     , soundsamples  :: ![SoundSample]     // list of sound samples
     , leveloptions  :: !LevelOptions      // level options
     }

:: LevelOptions
   = { fillbackground  :: !Maybe Colour    // fill the screen before drawing the layers
     , escquit         :: !Bool            // quit the level whenever the Esc key is pressed
     , debugscroll     :: !Bool            // scroll through the level with the arrow keys
     , fadein          :: !Bool            // fade in from black
     , fadeout         :: !Bool            // fade out to black
     }

:: Music
   = { musicfile :: !String     // MIDI file
     , restart   :: !Bool       // restart the music when it ends?
     , continue  :: !Bool       // music continues after end of level
     }

:: SoundSample
   = { soundid      :: !SoundID    // id for the sample (any number)
     , soundfile    :: !String     // WAV file
     , soundbuffers :: !Int        // max times sample can be played together
     }

:: SoundID
   :== Int

:: BoundMap
   = { map       :: ![{#Int}]  // map of all static bounds
     , blocksize :: !Size      // size of the map units (in pixels)
     , objstart  :: !Int       // min. value for objects (lower values are ignored)
     , startobjx :: !Int       // X-distance from screen where objects are initialized
     , startobjy :: !Int       // Y-distance from screen where objects are initialized
     }

:: GameRegion
   :== [Int]

:: Bounds
   :== Int

:: DirectionSet
   = { top    :: !Bool
     , left   :: !Bool
     , bottom :: !Bool
     , right  :: !Bool
     }

:: Layer
   = { bmp       :: !GameBitmap        // bitmap that contains all the tiles
     , layermap  :: !LayerMap          // map of the tiles in the level
     , sequences :: ![TileSequence]    // tiles that change repeatedly
     , movement  :: !Movement          // function to scroll the layer
     }

:: GameTime
   :== Int     // time in frames

:: GameBitmap
   = { bitmapname  :: !String        // bitmap that contains smaller blocks
     , unitsize    :: !Size          // size of these blocks (width, height)
     , dimensions  :: !(!Int,!Int)   // number of blocks (horizontal, vertical)
     , transparent :: !Maybe Point2  // position of a transparent pixel
     }

:: LayerMap
   :== [{#Int}]  // map of block numbers in a bitmap
                 // 0: empty; 1..n: block number; n+1..2n: mirror block;
                 // 2n+1..3n: upsidedown; 3n+1..4n: mirror and upsidedown;
                 // -1..-m: block sequence number
                 // (n = # blocks in gamebitmap; m = # sequences)

:: TileSequence
   :== (!Int, Sequence)

:: Sequence
   :== [(Int, Int)]

:: Movement
   :== Point2 GameTime -> Point2   // calculate layer's position from game position

:: SpriteID
   :== Int

:: Sprite
   = { bitmap   :: !GameBitmap   // sprites may have their own bitmap
     , sequence :: !Sequence     // seqence of blocks
     , loop     :: !Bool         // if FALSE, callback animation function
     }

:: Object gs
   = E.state:
     { objecttype :: !ObjectType    // identifier for the type of object, 0 = AutoInitObject
     , sprites    :: ![Sprite]      // sprite 1..n
     , init       :: !SubType !Point2 !GameTime !gs -> *(!*(state, ObjectRec), !gs)
     , done       :: !*(state, ObjectRec) !gs -> gs
     , move       :: !*(state, ObjectRec) !gs -> *(!*(state, ObjectRec), !gs)
     , animation  :: !*(state, ObjectRec) !gs -> *(!*(state, ObjectRec), !gs)
     , touchbound :: !*(state, ObjectRec) !DirectionSet !MapCode !gs -> *(!*(state, ObjectRec), !gs)
     , collide    :: !*(state, ObjectRec) !DirectionSet !ObjectType !ObjectRec !gs -> *(!*(state, ObjectRec), !gs)
     , frametimer :: !*(state, ObjectRec) !gs -> *(!*(state, ObjectRec), !gs)
     , keydown    :: !*(state, ObjectRec) !KeyCode !gs -> *(!*(state, ObjectRec), !gs)
     , keyup      :: !*(state, ObjectRec) !KeyCode !gs -> *(!*(state, ObjectRec), !gs)
     , userevent  :: !*(state, ObjectRec) !EventType !EventPar !EventPar !gs -> *(!*(state, ObjectRec), !gs)
     }

:: ObjectType
   :== Int

:: SubType
   :== Int

:: MapCode
   :== Int

:: KeyCode
   :== Int

:: EventType
   :== Int

:: EventPar
   :== Int

:: FVXY
   = { fvx  :: !FV
     , fvy  :: !FV
     }

:: FV
   = Factor !Real
   | Value !Real

:: RealXY
   = { rx :: !Real
     , ry :: !Real
     }

:: ObjectRec
   = { active              :: !Bool            // move and check collisions?
     , subtype             :: !SubType         // object's sub type
     , size                :: !Size            // the actual size
     , pos                 :: !Point2          // current position
     , offset              :: !Point2          // relative offset for sprite
     , currentsprite       :: !Int             // current animation sequence
     , displayoptions      :: !DisplayOptions  // invisible/mirror etc.
     , ownbounds           :: !Bounds          // bound(s) of the object (bits)
     , bouncebounds        :: !Bounds          // just bounce against these bounds
     , collidebounds       :: !Bounds          // call collide func for these bounds
     , forgetdistance      :: !Point2          // make object inactive at distance
     , framecounter        :: !GameTime        // frame counter
     , layer               :: !LayerPosition   // layer the object moves in front of
     , acceleration        :: !RealXY          // x/y acceleration
     , speed               :: !RealXY          // object's x/y speed
     , bounce              :: !FVXY            // x/y bounce at static bounds * 256
     , maxspeed            :: !RealXY          // x/y maximum speed
     , slowdown            :: !FVXY            // x/y slow down
     , skipmove            :: !Int             // acceleration delay
     , options             :: !ObjectOptions   // object options
     }

:: DisplayOptions
   = { blink               :: !Bool       // object blinks
     , stretch             :: !Bool       // stretch sprite to fit in size
     , mirrorleftright     :: !Bool       // mirror the sprite
     , mirrorupdown        :: !Bool       // draw sprite up side down
     , rotation            :: !Rotation   // rotation
     }

:: Rotation
   = NoRotation | Rotate90 | Rotate180 | Rotate270

:: ObjectOptions
   = { ignorelevelbounds   :: !Bool         // object can move out of the level
     , checkkeyboard       :: !Bool         // generate keydown event for this object
     , allowkeyboardrepeat :: !Bool         // allow pressed key to repeat
     , static              :: !Bool         // object always moves with screen
     , hdirection          :: !HDirection   // horizontal direction of the object
     , vdirection          :: !VDirection   // vertical direction of the object
     , automirrorleftright :: !Bool         // mirror object if horizontal direction changes
     , automirrorupdown    :: !Bool         // mirror object if vertical direction changes
     , freeze              :: !Bool         // no movement at all until framecounter reaches 0
     , removemapcode       :: !Bool         // remove the object completely from the map?
     }

:: HDirection
   = DirLeft | DirRight

:: VDirection
   = DirUp | DirDown

:: LayerPosition
   = InFront | AtLayer Int

:: Statistic
   = { format    :: !String        // text to display or formatstring for value
     , value     :: Maybe Int      // value to display
     , position  :: !Point2        // position on screen
     , style     :: !Style         // style to write the text in
     , color     :: Colour         // color for the text
     , shadow    :: Maybe Shadow   // shadow
     , alignment :: Alignment      // text alignment
     }

:: Alignment
   = { xyfromscreencenter :: !(!Bool,!Bool)   // center position on the screen
     , xycentered         :: !(!Bool,!Bool)   // center text around position
     }

:: Style
   = { fontname :: !String   // any font name
     , fontsize :: !Int      // size of the text
     , bold     :: !Bool     // bold
     , italic   :: !Bool     // italic
     }

:: Shadow
   = { shadowpos   :: !Point2   // relative position of the shadow to the text
     , shadowcolor :: Colour    // color of the shadow
     }


instance zero RealXY
where
   zero = {rx = 0.0, ry = 0.0}

instance zero Alignment
where
   zero = { xyfromscreencenter = (False, False)
          , xycentered = (False, False)
          }

