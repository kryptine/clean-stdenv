implementation module StdAGEC

import genericgecs, guigecs, infragecs
import StdGECExt, basicAGEC
		
// bimap GEC a b to use a b-editor for constructing an a-value

mkBimapGEC  :: (a (Current b) -> b) (b -> b) (b -> a) a -> (BimapGEC a b)
mkBimapGEC toGEC updGEC fromGEC value 
=	{ toGEC   = toGEC
	, fromGEC = fromGEC
	, updGEC  = updGEC
	, value   = value
	}

gGEC{|BimapGEC|} _ gGECb gecArgs pSt
	= bimapgec gGECb gecArgs pSt
bimapgec gGECb gecArgs=:{gec_value=mbimap,update=biupdate} pSt
	= convert (bhandle,pSt1)
where
	(bhandle,pSt1)	= gGECb {gecArgs & gec_value=mb,update=bupdate bhandle} pSt

	convert (bhandle,pst) = ({bhandle & gecSetValue = bimapSetValue bhandle.gecGetValue bhandle.gecSetValue
	                                  , gecGetValue = bimapGetValue bhandle.gecGetValue
	                          },pst)

	mb = case mbimap of
				Just bimap  = Just (bimap.toGEC bimap.value Undefined)
				Nothing	    = Nothing

	bupdate bhandle reason b pst 
		# nb	= bimap`.updGEC  b
		# na	= bimap`.fromGEC nb
		# pst	= bhandle.gecSetValue NoUpdate nb pst
		= biupdate reason {bimap` & value = na} pst

	bimapSetValue bGetValue bSetValue upd bimap pst
		# (b,pst) = bGetValue pst
	 	= bSetValue upd (bimap.toGEC bimap.value (Defined b)) pst
	bimapGetValue bGetValue pst
		# (b,pst) = bGetValue pst
		= ({bimap` & value = bimap`.fromGEC b},pst)
		
	bimap` = case mbimap of 
				Just bimap = bimap
				Nothing		= abort "Cannot make up function value for bimapGEC"

// Abstract editors

:: AGEC a = E. .b :  Hidden !(BimapGEC a b) !(A. .ps: InfraGEC (BimapGEC a b) (PSt ps)) !String

mkAGEC  :: !(BimapGEC a b) !String -> AGEC a | gGEC{|*|} b
mkAGEC bimapGEC descriptor =  Hidden bimapGEC (gGEC{|*->*->*|} undef gGEC{|*|}) descriptor

mkAGEC`  :: !(BimapGEC a (g b)) !String -> AGEC a | gGEC{|*->*|} g
mkAGEC` bimapGEC descriptor =  Hidden bimapGEC (gGEC{|*->*->*|} undef1 (gGEC{|*->*|} undef2)) descriptor
where
	undef1 = abort "mkAGEC` evaluated undefined GEC editor/1"
	undef2 = abort "mkAGEC` evaluated undefined GEC editor/2"
	
^^    :: (AGEC a) -> a
^^ (Hidden bimap ggec string) = bimap.value

(^=) infixl  :: (AGEC a) a -> (AGEC a)
(^=) (Hidden bimap ggec descr) nvalue = (Hidden {bimap & value = nvalue} ggec descr)

::	AGECSt a env
	=	E. b:
		{	agecBimapGEC:: !GECVALUE (BimapGEC a b) env		// The handle to the BimapGEC
		,	agecAGEC    :: !AGEC a							// The current AGEC value
		}

gGEC{|AGEC|} gGECa gecArgs pSt
	= gGEC_AGEC gGECa gecArgs pSt
where
	gGEC_AGEC gGECa gecArgs=:{gec_value=Just abstractGEC=:(Hidden bimapGEC gGECbimapGEC _),update=biupdate,location,makeUpValue,outputOnly,hasOBJECT} pSt
		# (abbaGEC,pSt)	= gGECbimapGEC {gecArgs & gec_value=Just bimapGEC,update=bupdate abstractGEC} pSt
		# (aGEC,pSt)	= openGECId pSt
		# aDef			= GECReceiver aGEC (fun bupdate)
		# lSt			= { agecBimapGEC = abbaGEC
						  , agecAGEC     = abstractGEC
						  }
		# (_,pSt)		= openReceiver lSt aDef pSt
//		# pSt			= openGEC aGEC pSt  // removed by mjp, saved a lot of space
		= (newGEC aGEC,pSt)
	where
		bupdate (Hidden bimap gGECb descr) reason nbimap pSt 
			= biupdate reason (Hidden {bimap & value = nbimap.value} gGECb descr) pSt
		
		fun :: !(A.b:(AGEC a) -> Update (BimapGEC a b) (PSt .ps)) !(GECMsgIn  (AGEC a)) !*(AGECSt a (PSt .ps),PSt .ps) -> (GECMsgOut (AGEC a), *(AGECSt a (PSt .ps),PSt .ps))
		fun _ InGetValue (lSt=:{agecBimapGEC=abbaGEC,agecAGEC=Hidden bimapGEC gGECbimapGEC descr},pSt)
			# (nval,pSt)	= abbaGEC.gecGetValue pSt
			= (OutGetValue (Hidden {bimapGEC & value=nval.value} gGECbimapGEC descr),(lSt,pSt))
		fun bupdate (InSetValue includeUpdate newAGECa=:(Hidden nval nGECfun ndescr)) (lSt=:{agecBimapGEC=abbaGEC,agecAGEC=(Hidden _ _ odescr)},pSt)
			| odescr == ndescr	// The same AGEC is used for the new value
				# (bimap,pSt)	= abbaGEC.gecGetValue pSt
				# pSt			= abbaGEC.gecSetValue includeUpdate {bimap & value=nval.value} pSt		// send new value down the current infrastructure
				= (OutDone,(lSt,pSt))
			| otherwise			// A different AGEC, of unknown type, is used for the new value
				# pSt			= abbaGEC.gecClose pSt													// close old infrastructure
				# (nabbaGEC,pSt)= nGECfun { gec_value   = Just nval										// build new infrastructure
								          , update      = bupdate newAGECa
								          , location    = location
								          , makeUpValue = makeUpValue
								          , outputOnly  = outputOnly
								          , hasOBJECT   = hasOBJECT
								          } pSt
				# pSt			= nabbaGEC.gecOpenGUI location pSt  //mjp
				# lSt			= { agecBimapGEC = nabbaGEC
								  , agecAGEC     = newAGECa
								  }
				= (OutDone,(lSt,pSt))
		fun _ InOpenGEC (lSt=:{agecBimapGEC=abbaGEC},pSt)
			= (OutDone,(lSt,abbaGEC.gecOpen pSt))
		fun _ InCloseGEC (lSt=:{agecBimapGEC=abbaGEC},pSt)
			= (OutDone,(lSt,abbaGEC.gecClose pSt))
		fun _ (InOpenGUI guiLoc objControlId) (lSt=:{agecBimapGEC=abbaGEC},pSt)
			= (OutDone,(lSt,abbaGEC.gecOpenGUI (guiLoc,objControlId) pSt))
		fun _ (InCloseGUI keepActiveCONS) (lSt=:{agecBimapGEC=abbaGEC},pSt)
			= (OutDone,(lSt,abbaGEC.gecCloseGUI keepActiveCONS pSt))
		fun _ (InSwitchCONS upd consPos) (lSt=:{agecBimapGEC=abbaGEC},pSt)
			= (OutDone,(lSt,abbaGEC.gecSwitch upd consPos pSt))
		fun _ (InArrangeCONS arrangement consPos) (lSt=:{agecBimapGEC=abbaGEC},pSt)
			= (OutDone,(lSt,abbaGEC.gecArrange arrangement consPos pSt))
		
		newGEC :: !(GECId t) -> .GECVALUE t (PSt .ps)
		newGEC tGEC
			= { gecOpen     = openGEC     tGEC
		      , gecClose    = closeGEC    tGEC
		      , gecOpenGUI  = openGECGUI  tGEC
		      , gecCloseGUI = closeGECGUI tGEC
		      , gecGetValue = getGECvalue tGEC
		      , gecSetValue = setGECvalue tGEC
		      , gecSwitch   = switchGEC   tGEC
		      , gecArrange  = arrangeGEC  tGEC
		      , gecOpened   = accPIO (isGECIdBound tGEC)
		      }

	gGEC_AGEC gGECa gecArgs pSt	
		# (aGEC,pSt)	= gGECa {gecArgs & gec_value=Nothing, update = \v r env -> env} pSt
		# (a,   pSt)	= aGEC.gecGetValue pSt
		# pSt			= aGEC.gecClose    pSt
		= gGEC_AGEC gGECa {gecArgs & gec_value=Just (hidAGEC a)} pSt


AGECtoCGEC :: String	(AGEC a) 		-> (GecCircuit a a) 	| gGEC{|*|}/*, generate{|*|}*/ a
AGECtoCGEC sa agec =  arr (\a -> agec ^= a) >>> edit sa >>> arr (\agec -> (^^ agec))

/*generate{|AGEC|} ga trace stream 
	# (a, trace, _, stream) = ga trace stream
	= (hidAGEC a, trace, \_ -> 0, stream)*/

CGECtoAGEC :: 			(GecCircuit a a ) a 	-> (AGEC a) 	| gGEC{|*|} a		// Use CGEC as AGEC 
CGECtoAGEC cgec a 
= mkAGEC { toGEC   = \a _ -> {inout = (Hide a,Hide a), gec = arr (\(Hide a) -> a) >>> cgec >>> arr (\a -> Hide a)}
		 , fromGEC = \{inout = (a,Hide b)} = b
		 , updGEC  = id
		 , value   = a
		 } "CGECtoAGEC"


gGEC{|GecComb|} gGECa gGECb args pSt
	= abort "Cannot make up function value for DataGec"	

