implementation module osprint

import StdEnv,memory
import ospicture, print, StdPicture, iostate
from	quickdraw import QGetPort,QSetPort, :: GrafPtr

::	PrintSetup	:==	String
::	JobInfo
	=	{	range		:: !(!Int,!Int)
		,	copies		:: !Int
		}
::	PrintInfo
	=	{	printSetup	:: PrintSetup
		,	jobInfo		:: JobInfo
		}
::	Alternative x y
	=	Cancelled x
	|	StartedPrinting y


import dodebug
eval :: !Bool !String !.a -> .a
eval a s p
	| a = abort s
		= DebugStr s p

os_getpagedimensions	::	!PrintSetup	!Bool
						->	(!(!Int,!Int), !(!(!Int,!Int),!(!Int,!Int)), !(!Int,!Int))
os_getpagedimensions printSetup emulateScreenRes
	# emulateScreenRes = DebugStr` ("os_getpagedimensions",emulateScreenRes) emulateScreenRes
	# (err,dimensions)	= getPageDimensionsC printSetup emulateScreenRes
	# err = DebugStr` ("err",err) err
	| err<>0	
		= abort "osPrint08: fatal error: out of memory (2)"
	= dimensions
	
os_defaultprintsetup	::	!*env
						->	(!PrintSetup, !*env)
os_defaultprintsetup  env
	# ((err,printSetup),env)	= getDefaultPrintSetupC env
	| err<>0
		= abort "osPrint08: fatal error: out of memory (1)"
	= (printSetup,env)
						
	
class PrintEnvironments printEnv
where
	os_printpageperpage :: !Bool !Bool 
						   !.x
						   .(.x -> .(PrintInfo -> .(*Picture -> *((.Bool,Point2),*(.state,*Picture)))))
						   (*(.state,*Picture) -> *((.Bool,Point2),*(.state,*Picture)))
						   !PrintSetup !*printEnv
						-> (Alternative .x .state,!*printEnv)
	os_printsetupdialog		:: !PrintSetup !*printEnv
							->	(!PrintSetup, !*printEnv)

// on the Mac printing in any environment behaves equal

instance PrintEnvironments (PSt .l)
where
	os_printpageperpage doDialog emulateScreen x initFun transFun printSetup pState
		= printPagePerPageBoth doDialog emulateScreen x initFun transFun printSetup pState
	os_printsetupdialog printSetup env
		# ((err,printSetup),env)	= printSetupDialogC printSetup env
		| err<>0
			= abort "osPrint08: fatal error: out of memory (3)"
		= (printSetup,env)
		
instance PrintEnvironments Files
where
	os_printpageperpage doDialog emulateScreen x initFun transFun printSetup files
		= printPagePerPageBoth doDialog emulateScreen x initFun transFun printSetup files
	os_printsetupdialog printSetup env
		# ((err,printSetup),env)	= printSetupDialogC printSetup env
		| err<>0
			= abort "osPrint08: fatal error: out of memory (3)"
		= (printSetup,env)

printPagePerPageBoth ::	!Bool !Bool
					.x 
					.(.x -> .(PrintInfo -> .(*Picture -> ((.Bool,Point2),(.state,*Picture)))))
					((.state,*Picture) -> ((.Bool,Point2),(.state,*Picture)))
					!PrintSetup !*anyEnv
				-> 	(Alternative .x .state,!*anyEnv)
printPagePerPageBoth doDialog emulateScreen x initFun transFun printSetup env
	# (os, env) = envGetToolbox env
	
	// open dialogs and get printInfo record
	
	  (err, pRecHdl, printInfo, os) = getPrintInfo doDialog emulateScreen printSetup os

	| err==1
		= abort memErrorMessage

	// possibly the user canceled printing via the dialog
	| err==2
//		# (_, os) = DisposHandle pRecHdl os 
    	= (Cancelled x, envSetToolbox os env)
    # (oldGrPort,os) = QGetPort os						// save current grafport
  	  (err, (grPort,os)) = prOpenDoc pRecHdl os			// open document

	// possibly the user canceled printing via command period
	| err == 1
		= abort memErrorMessage

	| err == 2 											// not fatal error (prob. user canceled)
      	# os = prCloseDoc (grPort,os) pRecHdl 					
		  os = prClose os								// will balance call of PrOpen via getPrintInfo
//		  (_, os) = DisposHandle pRecHdl os
		= (Cancelled x, envSetToolbox os env)  
	
	// ok, get a printer picture and apply it to the function initFun
	
    # os = QSetPort grPort os 							// set grafport
//	| os == 42 || os <> 42 = abort "OK!\n"
	# os = eval False "0" os
	# picture = packPicture zero defaultPen False 0 os
    # picture = eval False "1" picture
    # (endOrig,(initState,picture)) = initFun x printInfo picture	// get initialised state
    # picture = eval False "2" picture
	# (_,_,_,_,os) = unpackPicture picture
	# os = eval False "3" os

	  // now all pages can be printed
//	| os == 42 || os <> 42 = abort "OK!\n"
	
  	# ((pRecHdl,os),finalState) = printPages transFun endOrig initState (pRecHdl,os)
      os = prCloseDoc (grPort,os) pRecHdl 					// do the balancing functions
      os = QSetPort oldGrPort os
      os = prClose os

//    # (_, os) = DisposHandle pRecHdl os 
    = (StartedPrinting finalState, envSetToolbox os env)  

memErrorMessage	= "error: not enough extra memory for printing"

printPages _ (True,_) state intPict
  =(intPict,state)
printPages fun (_,origin) state (grPort,os)
  // prOpenPage will completely reinitialize the picture
  # (ok, os)	= prOpenPage grPort os
  | ok==0								// probably: the user canceled printing
	 # (_, os)	= prClosePage grPort os
	 = ((grPort,os),state)
  #	picture = packPicture origin defaultPen False 0 os
	// apply drawfunctions contained in this page
	(endOrig,(state,picture))			= fun (state,picture)
	// finish drawing
	(_,_,_,_,os) = unpackPicture picture
   	// end page
   	(ok, os)	= prClosePage grPort os
  | ok==0
	 = ((grPort,os),state)
  // draw rest of pages
  =	printPages fun endOrig state (grPort,os)      

zeroOrigin :== zero 		

///////////////////////////////////////////////////////////////////////////////

getPrintInfo :: !Bool !Bool !PrintSetup !*OSToolbox -> (!Int, !PRecHdl, !PrintInfo,!*OSToolbox)
getPrintInfo doDialog emulateScreen printSetup os
  #	( 	errCode,
    	pRecHdl,	
        first, last,
		copies,
		outPrintSetup,
		os ) = getPrintInfoC (if doDialog 1 0) (if emulateScreen 1 0) printSetup os
	first` = max 1 first
	last` = max first` last
	copies` = max 1 copies
  =( errCode,
  	 pRecHdl,
  	 { printSetup = outPrintSetup
	 , jobInfo =	{ range = (first`,last`)
	 				, copies = copies`
	 				}
	 },
     os
   )

 


os_printsetuptostring	::	!PrintSetup -> String
os_printsetuptostring printSetup
	= printSetup
	
os_stringtoprintsetup	::	!String -> PrintSetup
os_stringtoprintsetup string
	= string

envGetToolbox :: !*env -> (!*OSToolbox,!*env)
envGetToolbox env
  = (0,env)

envSetToolbox :: !*OSToolbox !*env -> *env
envSetToolbox os env
  = env

os_printsetupvalid		::	!PrintSetup !*env
						->	(!Bool, !*env)
os_printsetupvalid ps env
	= printsetupstringvalid ps env

