definition module StdPrint

//	********************************************************************************
//	Clean Standard Object I/O library, version 1.2
//	
//	StdPrint specifies general printing functions.
//	Related functions and modules:
//	-	getResolution (StdPicture)
//	-	resizeBitmap  (StdBitmap)
//	-	StdPrintText to print text.
//	********************************************************************************
 

// MW11 was from	StdPicture		import	Picture, Point
from	StdPicture		import	Picture, Point2
from	StdIOCommon		import	Size, Rectangle, IdFun, UpdateState, ViewFrame, UpdateArea
from	StdOverloaded	import	==
from	osprint			import	PrintSetup, JobInfo, PrintInfo, Alternative, 
								Cancelled, StartedPrinting, PrintEnvironments
from	iostate			import	IOSt, PSt
from	StdFile			import	FileEnv, Files

::	PageDimensions
	=	{	page		:: !Size		// size of the drawable area of the page 
		,	margins		:: !Rectangle	// This field contains information about the
										// size of the margins on a sheet in pixels.
										// Drawing can't occur within these margins.
										// The margin Rectangle is bigger than the
										// page size. Its values are:
										// corner1.x<=0 && corner1.y<=0 && 
										// corner2.x>=page.w && corner2.y>=page.h
		,	resolution	:: !(!Int,!Int)	// horizontal and vertical printer
										// resolution in dpi
		}

defaultPrintSetup	::	!*env -> (!PrintSetup, !*env)
					| FileEnv env
		// returns a default print setup 

printSetupDialog	::	!PrintSetup !*printEnv -> (!PrintSetup, !*printEnv)
					|	PrintEnvironments printEnv
		// lets the user choose a print setup via the print setup dialog
		
getPageDimensions	::	!PrintSetup	!Bool->	PageDimensions
instance == PageDimensions

fwritePrintSetup	::	!PrintSetup !*File -> *File
//	writes PrintSetup to file (text or data)

freadPrintSetup		::	!*File !*env -> (!Bool, !PrintSetup, !*File, !*env)	
					|	FileEnv env
//	reads PrintSetup from File (text or data). If resulting Boolean is True:success,
//	otherwise the default PrintSetup is returned


print	::	!Bool !Bool
			.(PrintInfo !*Picture -> ([IdFun *Picture],!*Picture))
			!PrintSetup !*printEnv
		->	(!PrintSetup,!*printEnv)
		|	PrintEnvironments printEnv

/*	print doDialog emulateScreen pages printSetup env
	sends output to the printer and returns the used print setup, which can differ
	from the input print setup
	doDialog:
		if True a dialog will pop up that lets the user choose all printing options,
		otherwise printing will happen in the default way.
	emulateScreen:
		if True, the printing routine will emulate the resolution of the screen. 
		That means that a pixel on paper has the same dimension as on screen. 
		Otherwise, the used resolution will be the printer resolution, with the 
		effect that coordinates get much "tighter". 
	pages:
		this function should calculate a list of functions, each function 
		representing one page to be printed. Each of these drawing functions is
		applied	to an initial printer Picture.
	env:
		a PrintEnvironment is either the PSt or the Files system. 
*/

printUpdateFunction
		:: 	!Bool (UpdateState -> *Picture -> *Picture) [Rectangle]
			!PrintSetup !*printEnv 
		->	(!PrintSetup, !*printEnv)
		| PrintEnvironments printEnv

/*	printUpdateFunction doDialog update area printSetup env
	sends the content of the update function of a given area to the printer:
	doDialog:
		identical to print.
	update:
		this function will be applied to an UpdateState of value 
			{oldFrame=area,newFrame=area,updArea=[area]}.
	area:
		the area to be sent to the printer. If a rectangle of this area does not
		fit on one sheet, it will be distributed on several sheets. 
	printSetup,env,result value:
		identical to print.
*/

// MW11 changed Point into Point2
printPagePerPage ::	!Bool !Bool 
					.x
					.(.x -> .(PrintInfo -> .(*Picture -> ((.Bool,Point2),(.state,*Picture)))))
					((.state,*Picture) -> ((.Bool,Point2),(.state,*Picture)))
					!PrintSetup !*printEnv 
				-> 	(Alternative .x .state,!*printEnv)
		        | PrintEnvironments printEnv

/*	printPagePerPage doDialog emulateScreen x prepare pages printSetup env
	sends output to the printer.
	This function can be used more efficiently than print. The major difference is 
	that the pages function is a state transition function instead of a page list 
	producing function. Each page transition function generates one page for the 
	printer. An additional feature of printPagePerPage is that it is possible to
	set the origin of the printer Pictures.

	doDialog:
		identical to print. 
	emulateScreen:
		identical to print.
	x:
		this value is passed to the prepare function.
	prepare:
		this function calculates the initial page print state. 
		Iff there are no pages to print, the return Boolean must be True.
		The returned Point is the Origin of the first printer Picture.
	pages:
		this state transition function produces the printed pages. 
		The state argument consists of the state information and an initial printer
		Picture which Origin has been set by the previous return Point value. 
		If there are no more pages to print, the return Boolean must be True. In 
		that case the result of printPagePerPage is (StartedPrinting state),
		with state the current state value. If printing should continue, the
		return Boolean is False.
		The returned Point is the Origin of the next printer Picture. 
	printSetup, env:
		identical to print.
	
	If printing is cancelled via the print dialog, then (Cancelled x) will be
	returned, otherwise (StartedPrinting ...)
*/
 
instance PrintEnvironments World
// other instances are the Files subworld and PSt (see osprint.dcl)

