definition module gameCrossCall_12

import	clCrossCall_12, gameintrface_12
from	ostypes		import OSWindowPtr
from	oswindow	import DelayActivationInfo

//	Initialisation of game. This function must be called before any game cross call can be done!
WinInitialiseGame :: !*OSToolbox -> *OSToolbox

  //----------------------------------------------//
 //    Game related crosscalls                   //
//----------------------------------------------//

WinCreateGameWindow :: !Bool !(!Int,!Int) !Int !*OSToolbox -> (![DelayActivationInfo],!OSWindowPtr,!*OSToolbox)

WinRunGameEngine :: !(CrossCallInfo -> .(.s -> .(*OSToolbox -> *(.CrossCallInfo,.s,*OSToolbox))))
                    !.s !Int !Int !Int !*OSToolbox
                 -> (!.s,!*OSToolbox)

WinInitGameObject :: !Int !Int !Int !Int !*OSToolbox -> (!GRESULT, !*OSToolbox)

// modified 01/11/99
WinCreateUserEvent :: !Int !Int !Int !Int !Int !Int !*OSToolbox -> (!GRESULT, !*OSToolbox)

WinPlaySoundSample :: !Int !Int !Int !Int !Int !*OSToolbox -> (!GRESULT, !*OSToolbox)
