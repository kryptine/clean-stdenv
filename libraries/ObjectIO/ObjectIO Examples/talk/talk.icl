module talk

//	**************************************************************************************************
//
//	This program creates two interactive processes that communicate via message passing.
//	In a future distributed version this program can be used as a graphical talk application.
//
//	The program has been written in Clean 1.3.2 and uses the Clean Standard Object I/O library 1.2
//	
//	**************************************************************************************************

import	StdEnv, StdIO

//	The essential data types. Other data types are given at the end of the program text.
//	The message type of talk processes:
::	Message
	=	NewLine String						// Transmit a line of text
	|	Quit								// Request termination
::	NoState
	=	NoState								// The singleton data type

//	Start creates two talk processes A and B that communicate by means of message passing.
Start :: *World -> *World
Start world
	#	(a,    world)	= openRId world
	#	(b,    world)	= openRId world
	#	(talkA,world)	= talk "A" a b world
	#	(talkB,world)	= talk "B" b a world
	=	startProcesses (ProcessGroup NoState (ListCS [talkA,talkB])) world

/*	talk name me you
	defines a talk process named name, to which messages can be sent of type Message
	via me, and that sends messages of type Message to a receiver you.
*/
talk :: String (RId Message) (RId Message) *World -> (Process .p,*World)
talk name me you world
	#	(wId,  world)	= openId world
	#	(outId,world)	= openId world
	#	(inId, world)	= openId world
		input			= EditControl	"" (PixelWidth (hmm 50.0)) 5
							[	ControlId		inId
							,	ControlKeyboard	inputfilter Able (noLS1 (input wId inId you))
							,	ControlResize	editResize
							,	ControlTip		"Type your message here"
							]
		output			= EditControl	"" (PixelWidth (hmm 50.0)) 5
							[	ControlId		outId
							,	ControlPos		(Below inId,NoOffset)
							,	ControlSelectState Unable
							,	ControlResize	editResize
							,	ControlTip		"Received messages appear here"
							]
	=	(	Process SDI	NoState (initialise input output wId outId inId) [ProcessClose (quit you)]
		,	world
		)
where
	initialise input output wId outId inId pst
		#	(size,pst)	= controlSize (input:+:output) True Nothing Nothing Nothing pst
			talkwindow	= Window ("Talk "+++name) (input:+:output)
							[	WindowId		wId
							,	WindowViewSize	size
							]
			menu		= Menu ("&Talk "+++name)
							(	MenuItem "&Quit" [MenuShortKey 'q',MenuFunction (noLS (quit you))]
							)	[]
			receiver	= Receiver me (noLS1 (receive wId outId)) []
		#	(_,pst)		= openWindow undef talkwindow pst
		#	(_,pst)		= openMenu   undef menu pst
		#	(_,pst)		= openReceiver undef receiver pst
		=	pst		

/*	editResize handles the resize of the two input fields. 
*/
editResize :: Size Size Size -> Size
editResize _ _ newWindowSize=:{h}
	= {newWindowSize & h=h/2}

/*	input handles keyboard input in the input EditControl: 
	for every KeyDown keyboard input that has been accepted by the input EditControl, input sends the 
	current content text of the input EditControl to the other talk process with (NewLine text).
*/
inputfilter :: KeyboardState -> Bool
inputfilter keystate
	=	getKeyboardStateKeyState keystate<>KeyUp

input :: Id Id (RId Message) KeyboardState (PSt .l .p) -> PSt .l .p
input wId inId you _ ps
	#	(Just window,ps)	= accPIO (getWindow wId) ps
		text				= fromJust (snd (hd (getControlTexts [inId] window)))
	=	snd (asyncSend you (NewLine text) ps)
	
/*	The message passing protocol of a talk process.
	On receipt of:
	(1)	NewLine text:set the new text to the output control field of the talk dialog.
	(2) Quit:	     this is always the last message of the other talk process when termination is 
		              requested. The process should terminate itself.
*/
receive :: Id Id Message (PSt .l .p) -> PSt .l .p
receive wId outId (NewLine text) ps=:{io}
	=	{ps & io=setControlTexts [(outId,text)] (setEditControlCursor outId (size text) io)}
receive _ _ Quit ps
	=	closeProcess ps

/*	The quit command first sends the Quit message to the other talk process and then quits itself.
*/	
quit :: (RId Message) (PSt .l .p) -> PSt .l .p
quit you ps
	=	closeProcess (snd (syncSend you Quit ps))
