implementation module osPrint

//	Clean Object I/O library, version 1.2

import	StdBool, StdClass, StdFile, StdInt, StdMisc, StdTuple
from	osevent		import setReplyInOSEvent, OSEvent, CrossCallInfo
from	ospicture	import unpackPicture, packPicture, Origin, Pen, defaultPen
from	clCCall_12	import WinGetHorzResolution, WinGetVertResolution, WinMakeCString, WinReleaseCString, CSTR
import	clCrossCall_12
import	scheduler

:: PrintInfo
 = { page		:: !Size,
     margins	:: !Rectangle,
     range		:: !(!Int,!Int),  
     copies		:: !Int,
     screenRes	:: !(!Int,!Int)
   }
   
:: Alternative x y = Cancelled x | StartedPrinting y

class PrintEnvironments printEnv
where
	OSprintPagePerPage ::!Bool !Bool
					.x 
					.(.x -> .(PrintInfo -> .(*Picture -> ((.Bool,Point2),(.state,*Picture)))))
					((.state,*Picture) -> ((.Bool,Point2),(.state,*Picture)))
					!*printEnv
				-> 	(Alternative .x .state, !*printEnv)

instance PrintEnvironments (PSt .l .p)
where
	OSprintPagePerPage doDialog emulateScreen x initFun transFun pSt
	 	= accContext accFun pSt
	  where
	  	accFun context
			# (os, context) = EnvGetOS context
	  		# (x,mb_context,os) = printPagePerPageBothSemaphor
								doDialog emulateScreen x initFun transFun (Just context) os
			= (x,EnvSetOS os (fromJust mb_context))

instance PrintEnvironments Files
where
	OSprintPagePerPage doDialog emulateScreen x initFun transFun files
		# (os, files) = EnvGetOS files
		  (x,_,os) = printPagePerPageBothSemaphor
		  					doDialog emulateScreen x initFun transFun Nothing os
		= (x, EnvSetOS os files) 

printPagePerPageBothSemaphor p1 p2 x p4 p5 mb_context os
// with this mechanism it is assured, that only one print job can happen at a time
// addSemaphor adds the parameter to a C global and gives back the previous value of that
// global
	# (s,os) = addSemaphor 1 os
	| s>0 
		# (_,os) = addSemaphor (-1) os
		= (Cancelled x,mb_context,os)
	# (result,mb_context,os) = printPagePerPageBoth p1 p2 x p4 p5 mb_context os
	  (_,os) = addSemaphor (-1) os
	= (result,mb_context,os)

/*
printPagePerPageBoth :: .Bool .Bool .a 
						.(.a -> .(.PrintInfo -> .(.Picture -> *((.Bool,.Origin),*(.b,*Picture))))) 
						((.b,.Picture) -> *((.Bool,.Origin),*(.b,*Picture))) 
						*(Maybe *Context) 
						*Unq 
					-> 	*(Alternative .a .b,*Maybe *Context,*Unq)
*/
printPagePerPageBoth :: .Bool .Bool .a 
						.(.a -> .(.PrintInfo -> .(.Picture -> *((.Bool,.Origin),*(.b,*Picture))))) 
						((.b,.Picture) -> *((.Bool,.Origin),*(.b,*Picture))) 
						*(Maybe *Context) 
						*Unq 
					-> 	*(*Alternative .a .b,*Maybe *Context,*Unq)
printPagePerPageBoth doDialog emulateScreen x initFun transFun mb_context os
	  // do the print dialog (or not) and get the hdc and the printInfo

	  # (err, hdc, printInfo, mb_context, os) 
	  		= getPrintInfo doDialog emulateScreen mb_context os

	  // check, whether the user canceled

	  | err >= 0 = (Cancelled x, mb_context, os)

	  // call StartDoc either via the OS thread or direct

	  #	(err, mb_context, os) = CCstartDoc hdc mb_context os
  
	  | err <= 0 = (Cancelled x, mb_context, deleteDC (hdc,os))
			// user canceled printing to file from file dialog
  
	  // initialise printer picture and call the initFun function

	  # picture = initPicture zeroOrigin (hdc,os)
	    (endOrig,(initState,picture)) = initFun x printInfo picture
 	    (_,_,_,hdc,os) = unpackPicture picture

	  // now print all pages
  
	  # (finalState,hdc,mb_context,os)
	  		= printPages 0 transFun endOrig initState hdc mb_context os

	  // Sluit af

	    (mb_context, os) = CCendDoc hdc mb_context os
	  = (StartedPrinting finalState, mb_context, (deleteDC (hdc,os)))


/*
printPages :: ((.state,*Picture) -> ((Bool,Point2),(.state,*Picture))) 
				 (!Bool,!Point2) 
				 .state 
				 !InternalPicture
			 ->  (.state,!InternalPicture)
*/
printPages :: !Int
			 ((.a,.Picture) -> *((.Bool,u:Origin),*(.a,*Picture)))
			 !(Bool,v:Origin)
			 !.a !Int !*(Maybe *Context)
			 !*OSToolbox
		 ->  *(!.a,!Int,!*Maybe *Context,!.OSToolbox), [u <= v]
printPages _ _ (True,_) state hdc mb_context os
  =(state,hdc,mb_context,os)
printPages pageNr fun (_,origin) state hdc mb_context os

  // give OS thread eventually a chance to handle events
  # (mb_context,os) = evtlSwitchToOS pageNr hdc mb_context os
  
  #	(ok, (hdc,os))	= startPage (hdc,os)
  | ok == 0 = abort "\nosPrint: Failed in \"StartPage\". Probably not enough memory."
  #	picture = initPicture origin (hdc,os)
  // apply drawfunctions contained in this page
	((endOfDoc,nextOrigin),(state`,picture))	= fun (state,picture)
  // finish drawing
  # (_,_,_,hdc,os)	= unpackPicture picture
   	(ok, (hdc,os))	= endPage (hdc,os)
    // (not ok) should not cause an abort, because endPage returns an error, when user chooses
  	// "encapsulated postscript" as output format and the output is longer than one page.
  	// This situation can't be retrieved from the "GetLastError" code. An abort should not occur. 
	(canceled,os)	= wasCanceled os
	// draw rest of pages
  =	printPages (inc pageNr) fun (endOfDoc || canceled || (ok==0),nextOrigin)  state` hdc mb_context os
      
zeroOrigin :== zero  		

///////////////////////////////////////////////////////////////////////////////

getPrintInfo :: !.Bool !.Bool !*(Maybe *Context) !*Unq 
			-> *(!Int,!Int,!.PrintInfo,!*Maybe *Context,!*Unq)
getPrintInfo doDialog emulateScreen mb_context os
  #	( err,
		 first, last,
		 copies,
		 hdc,
		 mb_context,
		 os
		) = CCgetDC (if doDialog 1 0) (if emulateScreen 1 0) mb_context os
	( 	width, height,
		leftPaper, topPaper, rightPaper, bottomPaper,
		os
		) = getCaps hdc os
	first` = max 1 first
	last` = max first` last
	copies` = max 1 copies
  =( err,
     hdc,
     { page   = { w=width,h=height},
	   margins 
 		   = { corner1 = { x=leftPaper , y=topPaper },
 		   	   corner2 = { x=rightPaper, y=bottomPaper}
			 },
       range = (first`,last`),
       copies = copies`,
       screenRes = (WinGetHorzResolution,WinGetVertResolution)
     },
     mb_context,
     os
   )

handleContextOSEvent` :: !OSEvent !Context !*OSToolbox -> (!CrossCallInfo,!Context,!*OSToolbox)
handleContextOSEvent` osEvent context tb
	# (return,context) = handleContextOSEvent osEvent context
	= (setReplyInOSEvent return,context,tb)

CCgetDC :: !.Int !.Int !*(Maybe *Context) !*Unq -> *(!Int,!Int,!Int,!Int,!Int,!*Maybe *Context,!*Unq)
CCgetDC doDialog emulateScreen Nothing os
	# (ok,first,last,copies,deviceContext,os) = getDC doDialog emulateScreen 1 os
	= (ok,first,last,copies,deviceContext,Nothing,os)
CCgetDC doDialog emulateScreen (Just context) os
	# createcci = Rq2Cci CcRqGET_PRINTER_DC doDialog emulateScreen
	  (rcci, context, os)  = IssueCleanRequest handleContextOSEvent` createcci context os
	= (	rcci.p1, rcci.p2, rcci.p3, rcci.p4, rcci.p5,
		Just context,os
	  )

CCstartDoc :: !.DC !*(Maybe *Context) !*Unq -> *(!Int,!*Maybe *Context,!*Unq)
// error code: -1:no error, 0: user canceled file dialog, others: other error
CCstartDoc hdc Nothing os
	# (err,(_,os)) = startDoc (hdc,os)
	= (err,Nothing,os)
CCstartDoc hdc (Just context) os
	# createcci = Rq1Cci CcRqSTARTDOC hdc
	  (rcci,context, os)  = IssueCleanRequest handleContextOSEvent` createcci context os
	= (rcci.p1, Just context, os)

CCendDoc :: !.DC !*(Maybe *Context) !*Unq -> *(!*Maybe *Context,!*Unq)
CCendDoc hdc Nothing os
	# (_,os) = endDoc (hdc,os)
	= (Nothing,os)
CCendDoc hdc (Just context) os
	# createcci = Rq1Cci CcRqENDDOC hdc
	  (_,context, os)  = IssueCleanRequest handleContextOSEvent` createcci context os
	= (Just context,os)

evtlSwitchToOS :: !Int !.Int !*(Maybe *Context) !*OSToolbox -> *(!*Maybe *Context,!.OSToolbox)
evtlSwitchToOS _ _ Nothing os
	= (Nothing,os)
evtlSwitchToOS pageNr hdc (Just context) os
	# nrStr = toString pageNr
	# messageText = nrStr+++" page"+++(if (pageNr==1) "" "s")+++" printed" 
	# (textPtr,os) = WinMakeCString messageText os
	# createcci = Rq1Cci CcRqDISPATCH_MESSAGES_WHILE_PRINTING textPtr
	# (_,context, os)  = IssueCleanRequest handleContextOSEvent` createcci context os
	# os = WinReleaseCString textPtr os
	= (Just context, os) 

initPicture :: !.Origin !*(!.OSPictContext,!*OSToolbox) -> !.Picture
initPicture origin intPict
 = packPicture origin defaultPen False (fst intPict) (snd intPict)
	

EnvGetOS :: !*env -> (!*Unq,!*env)
EnvGetOS env
  = (42,env)

EnvSetOS :: !*Unq !*env -> *env
EnvSetOS os env
  = env


//////////////////////////////////////////////////
//												//
//				C CALLING FUNCTIONS				//	
//												//
//////////////////////////////////////////////////

:: DC :== Int			// device context
:: *Unq :== *Int		// to enforce evaluation order
:: OkReturn :== Int		// okReturn<>0 <=> ok !

:: *InternalPicture :== (!DC, !Unq)

startPage :: !InternalPicture -> (!OkReturn, !InternalPicture)
startPage _
	= code
	{
			ccall startPage "II-III"
	}

endPage :: !InternalPicture -> (!OkReturn, !InternalPicture)
endPage _
	= code
	{
			ccall endPage "II-III"
	}

startDoc :: !InternalPicture -> (!Int, !InternalPicture)
			// err code: >0:no error, <=0: user cancelled file dialog
startDoc _
	= code
	{
			ccall startDoc "II-III"
	}

endDoc :: !InternalPicture -> InternalPicture
endDoc _
	= code
	{
			ccall endDoc "II-II"
	}

wasCanceled :: !*Unq -> (!Bool,!*Unq)
wasCanceled _
	= code
	{
			ccall wasCanceled ":I:I"
	}

deleteDC :: !InternalPicture -> *Unq
deleteDC _
	= code
	{
			ccall deleteDC "II-I"
	}

getDC :: !Int !Int !Int !*Unq -> (!Int, !Int, !Int, !Int, !Int, !*Unq)
// getDC doDialog emulateScreen "getDC called directly from CleanThread"
// first element of result is an error code:
// -1:no error, others: non fatal error
getDC _ _ _ _
	= code
	{
			ccall getDC "IIII-IIIIII"
	}

getCaps :: !Int !*Unq -> ( !Int, !Int, !Int, !Int, !Int, !Int, !*Unq)
getCaps _ _
	= code
	{
			ccall getCaps "II-IIIIIII"
	}

addSemaphor :: !Int !*Unq  -> (!Int,!*Unq)
addSemaphor _ _
	= code
	{
			ccall addSemaphor "I:I:I"
	}

