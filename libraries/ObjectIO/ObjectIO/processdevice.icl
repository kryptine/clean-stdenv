implementation module processdevice


//	Clean object I/O library, version 1.2

import StdBool, StdFunc, StdMisc
import StdPSt
import commondef, devicefunctions, iostate, processevent, StdProcessAttribute


processdeviceFatalError :: String String -> .x
processdeviceFatalError rule error
	= FatalError rule "processdevice" error


ProcessFunctions :: DeviceFunctions (PSt .l .p)
ProcessFunctions
	= {	dShow	= id//processShow
	  ,	dHide	= id//processHide
	  ,	dEvent	= processEvent
	  ,	dDoIO	= processIO
	  ,	dOpen	= processOpen
	  ,	dClose	= id//processClose
	  }

processOpen :: !(PSt .l .p) -> PSt .l .p
processOpen pState=:{io=ioState}
	# (hasProcess,ioState)			= IOStHasDevice ProcessDevice ioState
	| hasProcess
		= {pState & io=ioState}
	| otherwise
		# (deviceFunctions,ioState)	= IOStGetDeviceFunctions ioState
		# ioState					= IOStSetDeviceFunctions [ProcessFunctions:deviceFunctions] ioState
		= {pState & io=ioState}

processIO :: !DeviceEvent !(PSt .l .p) -> (!DeviceEvent,!PSt .l .p)

processIO deviceEvent=:ProcessRequestClose pState
	# (atts,pState)		= accPIO IOStGetProcessAttributes pState
	  (hasCloseAtt,att)	= Select isProcessClose undef atts
	| not hasCloseAtt
		= (deviceEvent,pState)
	| otherwise
		= (deviceEvent,getProcessCloseFun att pState)

processIO deviceEvent=:(ProcessRequestOpenFiles openFilesInfo) pState
	# (atts,pState)			= accPIO IOStGetProcessAttributes pState
	  (hasFilesOpenAtt,att)	= Select isProcessOpenFiles undef atts
	| not hasFilesOpenAtt
		= (deviceEvent,pState)
	| otherwise
		= (deviceEvent,getProcessOpenFilesFun att openFilesInfo pState)

processIO _ _
	= processdeviceFatalError "processIO" "unexpected DeviceEvent"
