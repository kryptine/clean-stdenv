definition module devicefunctions


//	********************************************************************************
//	Clean Standard Object I/O library, version 1.2.2
//	
//	Author: Peter Achten
//	Modified: 7 September 2001 for Clean 2.0
//	********************************************************************************


import	deviceevents


::	DeviceFunctions pst								// The major device callback functions:
	=	{	dDevice	:: Device						//	The device kind
		,	dShow	:: ShowFunction  pst			//	Show the device and its instances
		,	dHide	:: HideFunction  pst			//	Hide the device and its instances
		,	dEvent	:: EventFunction pst			//	Map an OSEvent to a DeviceEvent
		,	dDoIO	:: DoIOFunction  pst			//	Handle a DeviceEvent for this device
		,	dOpen	:: OpenFunction  pst			//	Open the initial device
		,	dClose	:: CloseFunction pst			//	Close the device and its instances
		}
::	ShowFunction  pst	:==	pst -> pst
::	HideFunction  pst	:==	pst -> pst
::	OpenFunction  pst	:==	pst -> pst
::	CloseFunction pst	:==	pst -> pst
::	EventFunction pst	:==	SchedulerEvent
						 ->	pst
						 ->	*(Bool, Maybe DeviceEvent, SchedulerEvent, pst)
::	DoIOFunction  pst	:==	DeviceEvent
						 -> pst
						 -> *(DeviceEvent, pst)
