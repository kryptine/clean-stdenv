definition module osPrint

//	Clean Object I/O library, version 1.2

import StdFile, StdPicture, StdPSt

::	PrintInfo
	=	{	page		:: !Size		// size of the drawable area of the page 
		,	margins		:: !Rectangle	// This field contains information about the
										// size of the margins on a sheet.
										// Drawing can't occur within these margins.
										// The margin Rectangle is bigger than the
										// page size. Its values are:
										// corner1.x<=0 && corner1.y<=0 && 
										// corner2.x>=page.w && corner2.y>=page.h
		,	range		:: !(!Int,!Int)	// First and last page as typed in by the 
										// user.
										// If the user chooses "ALL", then the first
										// page will be one, and the last page will 
										// be a "huge" number. 
		,	copies		:: !Int			// Number of copies. This will not 
										// necessarily be equal to the number of 
										// copies, as specified in the print dialog.
										// Some printer drivers take themselves care
										// of producing the appropriate number of 
										// copies => printInfo.copies==1.
		,	screenRes	:: !(!Int,!Int)	// The horizontal and vertical resolution of
										// the screen in dpi (see getResolution). 
		}
	// The units of measurement are pixels.

::	Alternative x y
	=	Cancelled x
	|	StartedPrinting y

class PrintEnvironments printEnv where
	OSprintPagePerPage ::!Bool !Bool
					.x 
					.(.x -> .(PrintInfo -> .(*Picture -> ((.Bool,Point2),(.state,*Picture)))))
					((.state,*Picture) -> ((.Bool,Point2),(.state,*Picture)))
					!*printEnv
				-> 	(Alternative .x .state, !*printEnv)

instance PrintEnvironments (PSt .l .p)
instance PrintEnvironments Files

