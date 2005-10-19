implementation module webServerTest

/*
	Special library to test Webservers.
	Pieter Koopman 2005
*/

import StdEnv, gast, StdHtml, htmlTestHandler, htmlPrintUtil
import StdTime // for making the seed

gEq{|Html|} h1 h2 = False
//genShow{|Html|} sep b html c = ["Html":c]
genShow{|Html|} sep b html c = genShow{|*|} sep b (fetchInputOptions html) c
derive genShow InputType, Value, Maybe, UpdValue

derive bimap []

// --------- Utilities --------- //

htmlPageTitle :: Html -> [String]
htmlPageTitle (Html (Head headAttrs headTags) bodyTags) = [s \\ `Hd_Std stdAttrs <- headAttrs, Std_Title s <- stdAttrs ]

htmlEditBoxValues :: Html String -> [Int]
htmlEditBoxValues html s = [ i \\ (Inp_Text,IV i,Just (t,int,UpdI j)) <- fetchInputOptions html | s==t ]

// --------- The main function --------- //

:: *SUT = { ioOptions	:: [(InputType,Value,Maybe Triplet)]
		 , fStates		:: *FormStates
		 , nWorld 		:: *NWorld
		 }

calcNextHtml  :: (*HSt -> (Html,*HSt)) (i->HtmlInput) *SUT i -> ([Html],*SUT)
calcNextHtml userpage transinput {ioOptions,fStates,nWorld} input
= case calcnewevents ioOptions of
	Just (triplet,updvalue) = convert (doHtmlTest (Just (triplet,updvalue,fStates)) userpage nWorld)
	Nothing = convert (doHtmlTest Nothing userpage nWorld)
where
	convert (html,fStates,nWorld) = ([html],{ioOptions = fetchInputOptions html,fStates = fStates,nWorld = nWorld})

	calcnewevents :: [(InputType,Value,Maybe Triplet)] -> Maybe (Triplet,UpdValue)
	calcnewevents []     = Nothing
	calcnewevents [x:xs] = case calcnewevent x (transinput input) of
							Nothing -> calcnewevents xs
							else	-> else

	calcnewevent :: (InputType,Value,Maybe Triplet) HtmlInput -> Maybe (Triplet,UpdValue)
	calcnewevent (Inp_Button,SV buttonname,Just triplet=:(t,_,_)) (HtmlButton b) 
		| t == b
			= Just (triplet,UpdS buttonname)		// button pressed
	calcnewevent (Inp_Text,IV oldint,Just triplet=:(t,_,_)) (HtmlIntTextBox b i)
		| t == b
			= Just (triplet,UpdI i)				// text input
	calcnewevent _ _ = Nothing

testHtml :: [TestSMOption s i Html] (Spec s i Html) s (i->HtmlInput) (*HSt -> (Html,*HSt)) *World -> *World 
			| ggen{|*|} i & gEq{|*|} s & genShow{|*|} s & genShow{|*|} i
testHtml opts spec initState transInput userpage world
	# ({hours, minutes, seconds}, world) = getCurrentTime world
	  seed = (hours * 60 + minutes) * 60 + seconds
	  (ok1,console,world)		= fopen "console.txt" FWriteText world
	  (ok2,file,world)			= fopen "testOut.txt" FWriteText world
	  (inout,world) 			= stdio world
	  nworld 					= {worldC = world, inout = inout}	
	  (initFormStates,nworld)	= initTestFormStates nworld 
	  inits 					= {ioOptions = [], fStates = initFormStates, nWorld = nworld}
	  (sut,console,file)		= testConfSM ([Seed seed]++opts) spec initState (calcNextHtml userpage transInput) inits (\sut={sut & ioOptions = []}) console file
	  nworld					= sut.nWorld
	  (_,world)					= fclose console nworld.worldC
	  (_,world)					= fclose file world
	= world
