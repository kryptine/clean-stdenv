module clipboardview

//	**************************************************************************************************
//
//	A program that can show and set the current content of the clipboard.
//
//	The program has been written in Clean 1.3.2 and uses the Clean Standard Object I/O library 1.2
//	
//	**************************************************************************************************

import StdEnv												// Import all standard library modules
import StdIO												// Import all standard gui library modules

Start :: *World -> *World									// The main rule
Start world
	# (ids,world)	= openIds 3 world						// Create 3 Id values
	= startIO NDI											// Evaluate an interactive process with:
			  Void											//    no local  process state
			  (initialise ids)								//    the initialisation action
			  []											//    only default attributes
			  world

initialise ids pst
	# (error,pst)	= openDialog Void clipview pst			// Open the clipview dialog
	| error<>NoError										// In case of an error:
		= closeProcess pst									//    terminate the interactive process
	| otherwise												// Otherwise:
		= pst												//    initialisation is complete
where
	viewid	= ids!!0
	showid	= ids!!1
	setid	= ids!!2
	
	clipview
		= Dialog											// The clipview window is a Dialog
			"Clipboard Viewer"								// The title of the clipview window
			(												// The controls of the clipview window:
				showclip									//    content display and refresh button
			:+:												//    +
				setclip										//    edit display and set button
			:+:												//    +
				quit										//    quit button
			)
			[	WindowId viewid								// Id of the clipview window
			]
	
	showclip												// The content display:
		=   EditControl "" width nrlines					//    an EditControl to display strings,
				[	ControlSelectState Unable				//    the user can't type text,
				,	ControlId          showid				//    its Id so the program can set text
				,	ControlPos         (Left,NoOffset)		//    its position (new line & left)
				]
		:+: ButtonControl "Show"							//    button to get the clipboard content,
				[	ControlFunction    (noLS show)			//    performed by the function show
				,	ControlTip         "Display clipboard content"
				]
	
	setclip													// The edit display:
		=   EditControl "" width nrlines					//    an EditControl,
				[	ControlId          setid				//    its Id so the program can get text,
				,	ControlPos         (Left,NoOffset)		//    its position (new line & left)
				,	ControlTip         "Type text for clipboard"
				]
		:+: ButtonControl "Set"								//    a button to set clipboard content,
				[	ControlFunction    (noLS set)			//    performed by the function set
				,	ControlTip         "Move text to clipboard"
				]
	
	quit=   ButtonControl "Quit"							// The quit button:
				[	ControlFunction    (noLS closeProcess)	//    simply closes the entire process
				,	ControlPos         (Center,NoOffset)	//    its position (new line & center)
				,	ControlTip         "Quit clipboardview"
				]
	
	width	= PixelWidth (hmm 100.0)
	nrlines	= 5
	
	show pst
		# (content,pst)	= getClipboard pst
		  text			= getString content
		= appPIO (setControlTexts [(showid,text)]) pst
	
	set pst
		# (dialog,pst)	= accPIO (getWindow viewid) pst
		  text			= fromJust (snd (getControlText setid (fromJust dialog)))
		= setClipboard [toClipboard text] pst
	
	getString [clip:clips]
		| isNothing item	= getString clips
		| otherwise			= fromJust item
	where
		item				= fromClipboard clip
	getString []
		= ""
