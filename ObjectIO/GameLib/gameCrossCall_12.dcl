definition module gameCrossCall_12

import clCrossCall_12, gameintrface_12

  //----------------------------------------------//
 //    Game related crosscalls                   //
//----------------------------------------------//

WinRunGameEngine :: !(CrossCallInfo -> .(.s -> .(*OSToolbox -> *(.CrossCallInfo,.s,*OSToolbox))))
                    !.s !Int !Int !Int !*OSToolbox
                 -> (!.s,!*OSToolbox)

WinInitGameObject :: !Int !Int !Int !Int !*OSToolbox -> (!GRESULT, !*OSToolbox)

// modified 01/11/99
WinCreateUserEvent :: !Int !Int !Int !Int !Int !Int !*OSToolbox -> (!GRESULT, !*OSToolbox)

WinPlaySoundSample :: !Int !Int !Int !Int !Int !*OSToolbox -> (!GRESULT, !*OSToolbox)
