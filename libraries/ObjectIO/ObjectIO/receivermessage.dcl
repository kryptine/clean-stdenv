definition module receivermessage


//	********************************************************************************
//	Clean Standard Object I/O library, version 1.2.2
//	
//	Author: Peter Achten
//	Modified: 7 September 2001 for Clean 2.0
//	********************************************************************************


import receivertable, semidynamic


::	QASyncMessage							// QASyncMessage: place this message in the async queue of this receiver
	=	{	qasmRecLoc	:: !RecLoc			// The location of the intended receiver
		,	qasmMsg		:: !SemiDynamic		// The asynchronous message that needs to be queued
		}
::	ASyncMessage							// ASyncMessage: this receiver should handle its first async message
	=	{	asmRecLoc	:: !RecLoc			// The location of the intended receiver
		}
::	SyncMessage								// SyncMessage: this receiver should handle this message synchronously
	=	{	smRecLoc	:: !RecLoc			// The location of the intended receiver
		,	smMsg		:: !SemiDynamic		// The synchronous message that needs to be handled
		,	smResp		:: ![SemiDynamic]	// The response of the intended receiver of this message
		,	smError		:: ![MessageError]	// Erroneous situation detected
		}
::	MessageError
	=	ReceiverUnable						// The receiver was not Able to receive
	|	ReceiverUnknown						// The receiver has not been found