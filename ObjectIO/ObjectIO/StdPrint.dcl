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
 

import StdIOCommon, StdPicture, osPrint


print :: !Bool !Bool
		 .(PrintInfo !*Picture -> ([IdFun *Picture],!*Picture))
         !*printEnv 
      -> *printEnv
      | PrintEnvironments printEnv

/*	print doDialog emulateScreen prepare pages env
	sends output to the printer:
	doDialog:
		if True a dialog will pop up that lets the user choose all printing options,
		otherwise printing will happen in the default way.
	emulateScreen:
		if True, the printing routine will emulate the resolution of the screen. 
		That means that a pixel on paper has the same dimension as on screen. 
		Otherwise, the used resolution will be the printer resolution, with the 
		effect that coordinates get much "tighter". 
		For high resolution output emulateScreen should be False. 
	prepare:
		this function should be used to calculate some formatting information. For 
		this purpose it is applied to the PrintInfo record and the printer Picture. 
		For instance, 
	pages:
		this function should calculate a list of functions, each function 
		representing one page to be printed. The drawing functions are each applied
		to an initial printer Picture.
	env:
		a PrintEnvironment is either the PSt or the Files system. 
*/

printUpdateFunction
		:: 	!Bool (UpdateState -> *Picture -> *Picture) [!Rectangle]
			*printEnv 
		->	*printEnv
		| PrintEnvironments printEnv

/*	printUpdateFunction doDialog update area env
	sends the content of the update function of a given area to the printer:
	doDialog:
		identical to print.
	update:
		this function will be applied to an UpdateState of value 
			{oldFrame=area,newFrame=area,updArea=[area]}.
	area:
		the area to be sent to the printer. If a rectangle of this area does not
		fit on one sheet, it will be distributed on several sheets. 
	env:
		identical to print.
*/

printPagePerPage ::	!.Bool !Bool 
					.x
					.(.x -> .(PrintInfo -> .(*Picture -> ((.Bool,Point2),(.state,*Picture)))))
					((.state,*Picture) -> ((.Bool,Point2),(.state,*Picture)))
					!*printEnv 
				-> 	(Alternative .x .state,!*printEnv)
		        | PrintEnvironments printEnv

/*	printPagePerPage doDialog emulateScreen x prepare pages env
	sends output to the printer.
	This function can be used more efficiently than print. The major difference is 
	that the pages function is a state transition function instead of a page list 
	producing function. Each page transition function generates one page for the 
	printer. An additional feature of is that it is possible to set the origin of 
	the printer Pictures.

	doDialog:
		identical to print. 
	emulateScreen:
		identical to print.
	x:
		this value is passed to the prepare function.
	prepare:
		this function calculate the initial page print state. 
		Iff there are no pages to print, the return Boolean must be True.
		The returned Point2 is the Origin of the first printer Picture.
	pages:
		this state transition function produces the printed pages. 
		The state argument consists of the state information and an initial printer
		Picture which Origin has been set by the previous return Point2 value. 
		If there are no more pages to print, the return Boolean must be True. In 
			that case the result of printPagePerPage is (StartedPrinting state),
			with state the current state value. If printing should continue, the
			return Boolean is False.
		The returned Point2 is the Origin of the next printer Picture. 
	env:
		identical to print.
	
	If printing is cancelled via the the print dialog, then (Cancelled x) will be
	returned, otherwise (StartedPrinting ...)
*/
