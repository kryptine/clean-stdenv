implementation module gameCrossCall_12


import clCrossCall_12, gameintrface_12
import StdTuple
/*	PA: these imports have been moved to clCrossCall_12.
import code from "cGameLib_12.obj", "cOSGameLib_12.obj", "ddutil.obj", "Dsutil.obj"
import code from library "ddraw_library"
import code from library "dsound_library"
*/


  //----------------------------------------------//
 //    Game related crosscalls                   //
//----------------------------------------------//

WinRunGameEngine :: !(CrossCallInfo -> .(.s -> .(*OSToolbox -> *(.CrossCallInfo,.s,*OSToolbox)))) !.s !Int !Int !Int !*OSToolbox
                 -> (!.s,!*OSToolbox)
WinRunGameEngine handleGameEvents initState a b c tb
    # (finalOScci,finalState,tb) = IssueCleanRequest handleGameEvents (Rq3Cci CcRqRUNGAME a b c) initState tb
    = (finalState,tb)

WinInitGameObject :: !Int !Int !Int !Int !*OSToolbox -> (!GRESULT, !*OSToolbox)
WinInitGameObject objtype subtype x y tb
    # tb = snd (IssueCleanRequest2 (ErrorCallback2 "WinInitGameObject") (Rq4Cci CcRqCREATEGAMEOBJECT objtype subtype x y) tb)
    = (0, tb)

// modified 01/11/99
WinCreateUserEvent :: !Int !Int !Int !Int !Int !Int !*OSToolbox -> (!GRESULT, !*OSToolbox)
WinCreateUserEvent ev evpar1 evpar2 target subtarget time tb
    # tb = snd (IssueCleanRequest2 (ErrorCallback2 "WinCreateUserEvent") (Rq6Cci CcRqUSERGAMEEVENT ev evpar1 evpar2 target subtarget time) tb)
    = (0, tb)

WinPlaySoundSample :: !Int !Int !Int !Int !Int !*OSToolbox -> (!GRESULT, !*OSToolbox)
WinPlaySoundSample id vol pan freq delay tb
    # tb = snd (IssueCleanRequest2 (ErrorCallback2 "WinCreateUserEvent") (Rq5Cci CcRqPLAYSOUNDSAMPLE id vol pan freq delay) tb)
    = (0, tb)
