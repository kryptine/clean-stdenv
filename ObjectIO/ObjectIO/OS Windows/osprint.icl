implementation module osprint

//	Clean Standard Object I/O library, version 1.1

// MW11 was import StdEnv,intrface,clCrossCall_12, iostate, scheduler
import StdEnv,clCCall_12,clCrossCall_12, iostate, scheduler
import ospicture, osevent, StdPicture, StdWindow, StdPSt

::	PrintSetup
	=	{	devmode	::	!String
		,	device	::	!String	// device, driver & output strings are null terminated
		,	driver	::	!String
		,	output	::	!String
		}
::	JobInfo
	=	{	range	::	!(!Int,!Int)
		,	copies	::	!Int
		}
::	PrintInfo
	=	{	printSetup			::	PrintSetup
		,	jobInfo				::	JobInfo
		}
::	Alternative x y
	=	Cancelled x
	|	StartedPrinting y


os_getpagedimensions	::	!PrintSetup	!Bool 
						->	!(!(!Int,!Int),
							  !(!(!Int,!Int),!(!Int,!Int)),
							  !(!Int,!Int))
os_getpagedimensions	{ devmode, device, driver } emulateScreenRes
	= os_getpagedimensionsC devmode device driver emulateScreenRes

os_defaultprintsetup	::	!*env
						->	(!PrintSetup, !*env)
os_defaultprintsetup env
	#	(dmSize,printerHandle,device,driver,output,env)	= getDevmodeSizeC env
	|	dmSize==0
		= ({devmode="\0", device="\0", driver="\0", output="\0"},env)
	#	devmode			= createArray dmSize ' '
		devmode			= { devmode & [dec dmSize]='\0'}
		env				= getDefaultDevmodeC devmode printerHandle device env	// alters contents of printSetup
	= ({devmode=devmode, device=device, driver=driver, output=output}, env)

os_printsetupdialog		::	!Bool !PrintSetup !*env
						->	(!PrintSetup, !*env)
os_printsetupdialog isWorld {devmode,device,driver,output} env
	# (os, env)		= EnvGetOS env
	# (devmodePtr,os) = WinMakeCString devmode os
	  (devicePtr,os) = WinMakeCString device os
	  (driverPtr,os) = WinMakeCString driver os
	  (outputPtr,os) = WinMakeCString output os
	  (ok, pdPtr, os)
	  		= CCPrintSetupDialog isWorld (size devmode) devmodePtr devicePtr driverPtr outputPtr os
	  os = WinReleaseCString devmodePtr os
	  os = WinReleaseCString devicePtr os
	  os = WinReleaseCString driverPtr os
	  os = WinReleaseCString outputPtr os
	| ok==0
		= ({devmode="\0",device="\0",driver="\0",output="\0"}, EnvSetOS os env)
	# (ndevmode,ndevice,ndriver,noutput,os)	= get_printSetup_with_PRINTDLG pdPtr os
	= ({devmode=ndevmode,device=ndevice,driver=ndriver,output=noutput}, EnvSetOS os env)

os_printsetupvalid		::	!PrintSetup !*env
						->	(!Bool, !*env)
os_printsetupvalid {devmode,device,driver} env
	= os_printsetupvalidC devmode device driver env

os_printsetupvalidC	::	!String !String !String!*env -> (!Bool, !*env)
os_printsetupvalidC _ _ _ _
	= code
		{
			ccall os_printsetupvalidC "SSS:I:A"
		}

class PrintEnvironments printEnv
  where
	// MW11 changed Point into Point2
	os_printpageperpage ::	!.Bool !Bool 
							!.x
							.(.x -> .(PrintInfo -> .(*Picture -> ((.Bool,Point2),(.state,*Picture)))))
							((.state,*Picture) -> ((.Bool,Point2),(.state,*Picture)))
							!PrintSetup !*printEnv
						-> 	(Alternative .x .state,!*printEnv)


instance PrintEnvironments (PSt .l .p)
where
	os_printpageperpage doDialog emulateScreen x initFun transFun printSetup pSt=:{io}
		#!	(windowStack, io)			= getWindowStack io
			windowStackIds				= map fst windowStack
			(zippedWithSelectState, io)	= seqList (map zipWithSelectState windowStackIds) io
			activeWindowIds				= [ id \\ (mbSelectState, id) <- zippedWithSelectState | isEnabled mbSelectState]
			io							= seq (map disableWindow activeWindowIds) io
	 		(result, pSt)				= accContext accFun { pSt & io=io }
			pSt							= appPIO (seq (map enableWindow activeWindowIds)) pSt
	 	= (result, pSt)
	  where
	  	accFun context
			# (os, context) = EnvGetOS context
	  		# (x,mb_context,os) = printPagePerPageBothSemaphor
								doDialog emulateScreen x initFun transFun printSetup (Just context) os
			= (x,EnvSetOS os (fromJust mb_context))
		zipWithSelectState :: .Id *(IOSt .a .b) -> *(.(Maybe SelectState,Id),*IOSt .a .b)
		zipWithSelectState id io
			#!	(mbSelectState, io)	= getWindowSelectState id io
			= ((mbSelectState, id), io)
		isEnabled (Just Able)	= True
		isEnabled _				= False


instance PrintEnvironments Files
where
	os_printpageperpage doDialog emulateScreen x initFun transFun printSetup files
		# (os, files) = EnvGetOS files
		  (x,_,os) = printPagePerPageBothSemaphor
		  					doDialog emulateScreen x initFun transFun printSetup Nothing os
		= (x, EnvSetOS os files) 

printPagePerPageBothSemaphor :: !.Bool !.Bool .a 
								.(.a -> .(.PrintInfo -> .(.Picture -> *((.Bool,.Origin),*(.b,*Picture))))) ((.b,.Picture) -> *((.Bool,.Origin),*(.b,*Picture)))
								!.PrintSetup *(Maybe *Context) !*OSToolbox
							 -> *(Alternative .a .b,*Maybe *Context,!.OSToolbox);
printPagePerPageBothSemaphor p1 p2 x p4 p5 printSetup mb_context os
// with this mechanism it is assured, that only one print job can happen at a time
// addSemaphor adds the parameter to a C global and gives back the previous value of that
// global
	# (s,os) = addSemaphor 1 os
	| s>0 
		# (_,os) = addSemaphor (-1) os
		= (Cancelled x,mb_context,os)
	# (result,mb_context,os) = printPagePerPageBoth p1 p2 x p4 p5 printSetup mb_context os
	  (_,os) = addSemaphor (-1) os
	= (result,mb_context,os)

printPagePerPageBoth :: !.Bool !.Bool .a
						.(.a -> .(.PrintInfo -> .(.Picture -> *((.Bool,.Origin),*(.b,*Picture))))) ((.b,.Picture) -> *((.Bool,.Origin),*(.b,*Picture)))
						.PrintSetup *(Maybe *Context) !*OSToolbox
					 -> *(Alternative .a .b,*Maybe *Context,!.OSToolbox);
printPagePerPageBoth doDialog emulateScreen x initFun transFun printSetup mb_context os
	  // do the print dialog (or not) and get the hdc and the printInfo

	  # (err, hdc, printInfo, mb_context, os) 
	  		= getPrintInfo doDialog emulateScreen printSetup mb_context os

	  | err == 4107 // this error occurs, when the printsetup contains bad values
			# (defaultPS, os)	= os_defaultprintsetup os
			= printPagePerPageBoth doDialog emulateScreen x initFun transFun defaultPS mb_context os
			
	  // check, whether the user canceled

	  | err >= 0 = (Cancelled x, mb_context, os)

	  // call StartDoc either via the OS thread or direct

	  #	(err, mb_context, os) = CCstartDoc hdc mb_context os
  
	  | err <= 0 = (Cancelled x, mb_context, deleteDC hdc os)
			// user canceled printing to file from file dialog
  
	  // initialise printer picture and call the initFun function

	  # picture = initPicture zeroOrigin (hdc,os)
	    (endOrig,(initState,picture)) = initFun x printInfo picture
// MW11 was 	    (_,_,hdc,os) = unpackPicture picture
 	    (_,_,_,hdc,os) = unpackPicture picture

	  // now print all pages
  
	  # (finalState,hdc,mb_context,os)
	  		= printPages 0 transFun endOrig initState hdc mb_context os

	  // Sluit af

	    (mb_context, os) = CCendDoc hdc mb_context os
	  = (StartedPrinting finalState, mb_context, (deleteDC hdc os))


printPages :: Int 
			((.a,.Picture) -> *((.Bool,u:Origin),*(.a,*Picture)))
			(Bool,v:Origin) .a HDC *(Maybe *Context) !*OSToolbox
		-> *(.a,HDC,*Maybe *Context,!.OSToolbox), [u <= v];
printPages _ _ (True,_) state hdc mb_context os
  =(state,hdc,mb_context,os)
printPages pageNr fun (_,origin) state hdc mb_context os

  // give OS thread eventually a chance to handle events
  # (mb_context,os) = evtlSwitchToOS pageNr hdc mb_context os
  
  #	(ok, os)	= startPage hdc os
  | ok == 0 = abort "\nPrint08: Failed in \"StartPage\". Probably not enough memory."
  #	picture = initPicture origin (hdc,os)
  // apply drawfunctions contained in this page
	((endOfDoc,nextOrigin),(state`,picture))	= fun (state,picture)
  // finish drawing
// MW11 was  # (_,_,hdc,os)	= unpackPicture picture
  # (_,_,_,hdc,os)	= unpackPicture picture
   	(ok, os)	= endPage hdc os
    // (not ok) should not cause an abort, because endPage returns an error, when user chooses
  	// "encapsulated postscript" as output format and the output is longer than one page.
  	// This situation can't be retrieved from the "GetLastError" code. An abort should not occur. 
	(canceled,os)	= wasCanceled os
	// draw rest of pages
  =	printPages (inc pageNr) fun (endOfDoc || canceled || (ok==0),nextOrigin)  state` hdc mb_context os
      
zeroOrigin :== zero  		

///////////////////////////////////////////////////////////////////////////////

getPrintInfo :: !.Bool !.Bool .PrintSetup *(Maybe *Context) !*OSToolbox
			 -> *(Int,Int,.PrintInfo,*Maybe *Context,!.OSToolbox);
getPrintInfo doDialog emulateScreen {devmode, device, driver, output} mb_context os
	# (devmodePtr,os) = WinMakeCString devmode os
	  (devicePtr,os) = WinMakeCString device os
	  (driverPtr,os) = WinMakeCString driver os
	  (outputPtr,os) = WinMakeCString output os
	  ( err, data, pdPtr, mb_context, os)
	  		= CCgetDC	(if doDialog 1 0) (if emulateScreen 2 0)	// these two bits will be packed into one word in CCgetDC
						(size devmode) devmodePtr devicePtr driverPtr outputPtr mb_context os
	  os = WinReleaseCString devmodePtr os
	  os = WinReleaseCString devicePtr os
	  os = WinReleaseCString driverPtr os
	  os = WinReleaseCString outputPtr os
	| doDialog && (err==(-1))
		= continuation err data mb_context (get_printSetup_with_PRINTDLG pdPtr os)
	= continuation err data mb_context (devmode,device,driver,output,os)
  where
	continuation err (first,last,copies,hdc) mb_context (devmode,device,driver,output,os)
		# first` = max 1 first
		  last` = max first` last
		  copies` = max 1 copies
		= ( err,
		    hdc,
		    {	printSetup	= { devmode=devmode, device=device ,driver=driver, output=output },
   			  	jobInfo		= {	range = (first`,last`),
   	    						copies = copies`
   	    					  }
 		     },
		     mb_context,
 		   	 os
   		   )

handleContextOSEvent` :: !OSEvent !Context !*OSToolbox -> (!CrossCallInfo,!Context,!*OSToolbox)
handleContextOSEvent` osEvent context tb
	# (return,context) = handleContextOSEvent osEvent context
	= (setReplyInOSEvent return,context,tb)
// MW11 was	= (replytocrosscallinfo return,context,tb)


CCgetDC :: !.Int !.Int !.Int !.Int !.Int !.Int !.Int !*(Maybe !*Context) !*OSToolbox -> *(!Int,!(!Int,!Int,!Int,!Int),!Int,!*Maybe *Context,!.OSToolbox);
CCgetDC doDialog emulateScreen devmodeSize devmodePtr devicePtr driverPtr outputPtr Nothing os
	# (ok,first,last,copies,pdPtr,deviceContext,os)
		= getDC doDialog emulateScreen 1 devmodeSize devmodePtr devicePtr driverPtr outputPtr os
	= (ok,(first,last,copies,deviceContext),pdPtr,Nothing,os)
CCgetDC doDialog emulateScreen devmodeSize devmodePtr devicePtr driverPtr outputPtr (Just context) os
	# createcci = Rq6Cci 	CcRqGET_PRINTER_DC (doDialog bitor emulateScreen) devmodeSize
							devmodePtr devicePtr driverPtr outputPtr
	# (rcci, context, os)  = IssueCleanRequest handleContextOSEvent` createcci context os
	= (	rcci.p1, (rcci.p2, rcci.p3, rcci.p4,rcci.p6), rcci.p5,
////////err,	 (first,   last,    copies, deviceContext),pdPtr,
		Just context,os
	  )

CCPrintSetupDialog :: !.Bool .Int .Int .Int .Int .Int !*OSToolbox -> (OkReturn,Int,!.OSToolbox);
CCPrintSetupDialog True devmodeSize devmodePtr devicePtr driverPtr outputPtr os
	= printSetup 1 devmodeSize devmodePtr devicePtr driverPtr outputPtr os
CCPrintSetupDialog False devmodeSize devmodePtr devicePtr driverPtr outputPtr os
	# createcci = Rq5Cci CcRqDO_PRINT_SETUP devmodeSize devmodePtr devicePtr driverPtr outputPtr
	  (rcci, os)  = IssueCleanRequest2 (ErrorCallback2 "ERROR in osPrint08") createcci os
	= (rcci.p1, rcci.p2, os)

CCstartDoc :: !.HDC !*(Maybe *Context) !*OSToolbox -> *(!Int,!*Maybe *Context,!*OSToolbox)
// error code: -1:no error, 0: user canceled file dialog, others: other error
CCstartDoc hdc Nothing os
	# (err,os) = startDoc hdc os
	= (err,Nothing,os)
CCstartDoc hdc (Just context) os
	# createcci = Rq1Cci CcRqSTARTDOC hdc
	  (rcci,context, os)  = IssueCleanRequest handleContextOSEvent` createcci context os
	= (rcci.p1, Just context, os)

CCendDoc :: !.HDC !*(Maybe *Context) !*OSToolbox -> *(!*Maybe *Context,!*OSToolbox)
CCendDoc hdc Nothing os
	# os = endDoc hdc os
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
	# messageText = if (pageNr==0)	""
									(nrStr+++" page"+++(if (pageNr==1) "" "s")+++" printed")
	# (textPtr,os) = WinMakeCString messageText os
	# createcci = Rq1Cci CcRqDISPATCH_MESSAGES_WHILE_PRINTING textPtr
	# (_,context, os)  = IssueCleanRequest handleContextOSEvent` createcci context os
	# os = WinReleaseCString textPtr os
	= (Just context, os) 

initPicture :: !.Origin !*(!.OSPictContext,!*OSToolbox) -> !.Picture
initPicture origin intPict
// MW11 was = packPicture origin defaultPen (fst intPict) (snd intPict)
 = packPicture origin defaultPen False (fst intPict) (snd intPict)
	

EnvGetOS :: !*env -> (!*OSToolbox,!*env)
EnvGetOS env
  = (42,env)

EnvSetOS :: !*OSToolbox !*env -> *env
EnvSetOS os env
  = env


//////////////////////////////////////////////////
//												//
//				C CALLING FUNCTIONS				//	
//												//
//////////////////////////////////////////////////

:: OkReturn :== Int		// okReturn<>0 <=> ok !

os_getpagedimensionsC	::	!String !String !String !Bool 
						->	(!(!Int,!Int), !(!(!Int,!Int),!(!Int,!Int)), !(!Int,!Int))
os_getpagedimensionsC _ _ _ _
	= code
		{
			ccall	os_getpagedimensionsC "SSSI-IIIIIIII"
		}
		
getDevmodeSizeC	::	!*env	-> (!Int,!Int,!String,!String,!String,!*env)
getDevmodeSizeC _
	= code
		{
			ccall getDevmodeSizeC ":VIISSS:A"
		}
		
getDefaultDevmodeC	::	!String !Int !String !*env	->	*env
getDefaultDevmodeC _ _ _ _
	= code
		{
			ccall getDefaultDevmodeC "SIS:V:A"
		}
		
printSetup	:: !Int !Int !Int !Int !Int !Int !*OSToolbox -> (!OkReturn,!Int,!*OSToolbox)
printSetup _ _ _ _ _ _ _
	= code
		{
			ccall printSetup "IIIIII:VII:I"
		}

get_printSetup_with_PRINTDLG	::	!Int !*OSToolbox -> (!String, !String, !String, !String, !*OSToolbox)
get_printSetup_with_PRINTDLG _ _
	= code
		{
			ccall get_printSetup_with_PRINTDLG "I:VSSSS:I"
		}

startPage :: !HDC !*OSToolbox -> (!OkReturn, !*OSToolbox)
startPage _ _
	= code
	{
			ccall startPage "I:I:I"
	}

endPage ::	!HDC !*OSToolbox -> (!OkReturn, !*OSToolbox)
endPage _ _
	= code
	{
			ccall endPage "I:I:I"
	}

startDoc :: !HDC !*OSToolbox -> (!Int, !*OSToolbox)
			// err code: >0:no error, <=0: user cancelled file dialog
startDoc _ _
	= code
	{
			ccall startDoc "I:I:I"
	}

endDoc :: !HDC !*OSToolbox -> *OSToolbox
endDoc _ _
	= code
	{
			ccall endDoc "I:V:I"
	}

wasCanceled :: !*OSToolbox -> (!Bool,!*OSToolbox)
wasCanceled _
	= code
	{
			ccall wasCanceled ":I:I"
	}

deleteDC :: !HDC !*OSToolbox -> *OSToolbox
deleteDC _ _
	= code
	{
			ccall deleteDC "I:V:I"
	}


getDC :: !Int !Int !Int !Int !Int !Int !Int !Int !*OSToolbox -> (!Int, !Int, !Int, !Int, !Int, !Int, !*OSToolbox)
// getDC doDialog emulateScreen "getDC called directly from CleanThread" devmodeSize
// first element of result is an error code:
// -1:no error, others: non fatal error
getDC _ _ _ _ _ _ _ _ _
	= code
	{
			ccall getDC "IIIIIIII:VIIIIII:I"
	}

addSemaphor :: !Int !*OSToolbox  -> (!Int,!*OSToolbox)
addSemaphor _ _
	= code
	{
			ccall addSemaphor "I:I:I"
	}

os_printsetuptostring	::	!PrintSetup -> String
os_printsetuptostring {devmode, device, driver, output}
	=		toString (size devmode)+++" "+++toString (size device)+++" "+++toString (size driver)+++" "
		 +++devmode+++device+++driver+++output

os_stringtoprintsetup	::	!String -> PrintSetup
os_stringtoprintsetup string
	#!	chList	= [ch \\ ch<-:string]
		(sizeChLists, rest)	= seqList (repeatn 3 (splitInt [])) chList
		sizes	= map (toInt o toString) sizeChLists
		(devmodeSize, deviceSize, driverSize)	= listTo3Tuple sizes
		devmode	= toString (rest % (0, devmodeSize-1))
		driverStartIndex = devmodeSize+deviceSize
		device	= toString (rest % (devmodeSize, driverStartIndex-1))
		outputStartIndex	= driverStartIndex+driverSize
		driver	= toString (rest % (driverStartIndex, outputStartIndex-1))
		output	= toString (rest % (outputStartIndex, (size string)-1))
	|		size devmode==devmodeSize && size device==deviceSize
		&&	size driver==driverSize && size output==(length rest)-outputStartIndex
		&&	devmodeSize>0 && deviceSize>0 && driverSize>0 && size output>0
		= {devmode=devmode, device=device, driver=driver, output=output}
	= {devmode="\0", device="\0", driver="\0", output="\0"}
  where
	splitInt akku []
		= (reverse akku, [])
	splitInt akku [ch:chs]
		|	isDigit ch
			= splitInt [ch:akku] chs
		= (reverse akku, chs)
	listTo3Tuple [e1,e2,e3] = (e1,e2,e3)
