module counter

//	**************************************************************************************************
//
//	Open a dialog that displays a number that can be incremented and decremented using two buttons.
//
//	The program has been written in Clean 1.3.2 and uses the Clean Standard Object I/O library 1.2
//	
//	**************************************************************************************************

import StdEnv, StdIO

::	NoState	= NoState

Start :: *World -> *World
Start world
	= startIO NDI NoState initIO [] world

initIO pst
	# (dialogid, pst)	= accPIO openId pst
	# (displayid,pst)	= accPIO openId pst
	# (_,pst)			= openDialog NoState (dialog dialogid displayid) pst
	= pst
where
	dialog dialogId displayId
		= Dialog "Counter" 
			{	newLS	= init
			,	newDef	=	EditControl (toString init)	displaywidth displayheight 
								[	ControlPos			(Center,NoOffset)
								,	ControlId			displayId
								,	ControlSelectState	Unable
								]
						:+:	CompoundControl
						(	ButtonControl "&-" 
								[	ControlFunction		(upd (-1))
								,	ControlTip			"Decrement counter value"
								]
						:+:	ButtonControl "&+"
								[	ControlFunction		(upd 1)
								,	ControlTip			"Increment counter value"
								]
						)	[ControlPos (Center,zero)]
			}
			[	WindowClose	(noLS closeProcess)
			,	WindowId	dialogId
			]
	where
		displaywidth	= PixelWidth 100
		displayheight	= 1
		init			= 0
		
		upd :: Int (Int,PSt .l) -> (Int,PSt .l)
		upd dx (count,pst)
			# count	= count+dx
			= (count,appPIO (setControlText displayId (toString count)) pst)
