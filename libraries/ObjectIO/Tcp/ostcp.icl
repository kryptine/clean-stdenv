implementation module ostcp

import	StdInt, StdTuple
import	StdTCPDef
import	StdChannels
import	tcp, ostick
import	code from "cTCP_121.obj", library "wsock_library"
import	clCrossCall_12
import	code from "cCrossCallTCP_121.obj"	// PA: moved from ostoolbox


// PA: moved from ostoolbox:
OSinstallTCP :: !*OSToolbox -> *OSToolbox
OSinstallTCP tb
	= snd (issueCleanRequest2 (\_ tb->(return0Cci,tb)) (Rq0Cci CcRqCREATETCPWINDOW) (osInstallTCP tb))

osInstallTCP :: !*OSToolbox -> *OSToolbox
osInstallTCP _
	= code
	{
		.inline InstallCrossCallTCP
			ccall InstallCrossCallTCP "I-I"
		.end
	}
// ...PA

os_eom					::	!EndpointRef !*env
						->	(!Bool, !*env)
os_eom _ _
// check for eom
	= code
		{
			ccall os_eom "I:I:A"
		}
		
os_disconnected			::	!EndpointRef !*env
						->	(!Bool, !*env)
os_disconnected _ _
// check for disconnected
	= code
		{
			ccall os_disconnected "I:I:A"
		}

os_connectrequestavailable	::	!EndpointRef !*env
							->	(!Bool, !*env)
os_connectrequestavailable _ _
	= code
		{
			ccall os_connectrequestavailable "I:I:A"
		}

os_connectTCP			::	!Int !Bool !(!Bool, !Int) !(!Int,!Int) !*env
						->	(!(!InetErrCode,!Bool,!EndpointRef), !*env)
os_connectTCP _ _ _ _ _
	= code
		{
			ccall os_connectTCPC "IIIIII:VIII:A"
		}

os_select_inetevents	::	!EndpointRef !InetReceiverCategory !Int !Bool !Bool !Bool !*env
						->	!*env
os_select_inetevents endpointRef receiverType referenceCount get_receive_events get_sendable_events 
					alreadyEom env
	= code
		{
			ccall os_select_inetevents "IIIIII:V:A"
		}

getMbStopTime	::	!(Maybe !Timeout) !*env	-> (!(!Bool, !Int), !*env)
				|	ChannelEnv env
getMbStopTime Nothing env
	=((False,0), env)
getMbStopTime (Just timeout) env
	# (now, env) = getCurrentTick env
	= ((True, timeout + (unpack_tick now)), env)
