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
	= convert (bhandle,pst1)
where
	(bhandle,pst1) = gGECb {gecArgs & gec_value=mb,update=bupdate bhandle} pSt

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

:: AGEC a = E. .b :  Hidden (BimapGEC a b) (A. .ps: InfraGEC (BimapGEC a b) (PSt ps)) String

mkAGEC  :: (BimapGEC a b) String -> AGEC a | gGEC{|*|} b
mkAGEC bimapGEC descriptor =  Hidden bimapGEC (gGEC{|*->*->*|} undef gGEC{|*|}) descriptor

^^    :: (AGEC a) -> a
^^ (Hidden bimap ggec string) = bimap.value

(^=) infixl  :: (AGEC a) a -> (AGEC a)
(^=) (Hidden bimap ggec descr) nvalue = (Hidden {bimap & value = nvalue} ggec descr)


gGEC{|AGEC|} gGECa gecArgs pSt
	= let (aGEC,pSt1) = openGECId pSt
	  in  agecGEC aGEC gGECa gecArgs pSt1

::	AGECSt a env
	=	{	aGECVisible    :: !Bool
		,	aGECGUILoc     :: !GUILoc
		,	aGECOBJECTLoc  :: !OBJECTControlId
		,	aGECGECVALUE   :: GECVALUE a env
		}

agecGEC :: !(GECId (AGEC a)) !(InfraGEC a (PSt .ps)) -> InfraGEC (AGEC a) (PSt .ps)
/**************************************************************************************/
agecGEC ::   !(GECId     (AGEC a)) 
             !(InfraGEC  a        (PSt .ps))
           -> InfraGEC   (AGEC a) (PSt .ps)
agecGEC tGEC gGECa = aGECGEC` tGEC gGECa
where
	aGECGEC` tGEC /*gecguiFun*/ gGECa gecArgs=:{location=(guiLoc,objLoc),outputOnly,gec_value=mcv,update} pSt
		# lSt =
			{ aGECVisible  = False 
			, aGECGECVALUE = undef
			, aGECGUILoc   = guiLoc
			, aGECOBJECTLoc= objLoc
			}
		= GEC2 tGEC aGEC_trace_name gecguiFun (aGECFun gGECa mv) outputOnly lSt pSt
	where
		aGEC_trace_name				= "AGEC"
		update` r x y				= update r x y
	
		updateOBJECTA reason a pSt
			= update` reason (OBJECT a) pSt
	
		mv							= case mcv of
										Just (OBJECT x) = Just x
										_               = Nothing
		
		aGECFun _ _ _ _ InGetValue (lSt=:{aGECGECVALUE},pSt)
			# (bound,pSt)			= aGECGECVALUE.gecOpened pSt
			| bound
				# (a,pSt)			= aGECGECVALUE.gecGetValue pSt
				= (OutGetValue (OBJECT a),(lSt,pSt))
			| otherwise
				= abort ("gGEC{|"+++aGEC_trace_name+++"|}: handling request for InGetValue failed: no child GEC.")
		aGECFun gGECa _ _ _ msg=:(InSetValue yesUpdate v=:(OBJECT a)) (lSt=:{aGECVisible,aGECGECVALUE},pSt)
			# pSt				= aGECGECVALUE.gecSetValue NoUpdate a pSt
			# pSt				= if (yesUpdate === YesUpdate) (update` Changed v pSt) pSt
			= (OutDone,(lSt,pSt))
		aGECFun gGECa mv {guiLocs} tGEC msg=:InOpenGEC (lSt=:{aGECGUILoc,aGECOBJECTLoc},pSt)
			# [aLoc:_]				= guiLocs (aGECGUILoc,aGECOBJECTLoc)
			# (setA,pSt)			= gGECa {gecArgs & location=aLoc, gec_value=mv,update=updateOBJECTA} pSt
			# lSt					= {lSt & aGECGECVALUE=setA}
			= (OutDone,(lSt,pSt))
		aGECFun _ _ {guiClose} tGEC InCloseGEC (lSt=:{aGECVisible,aGECGECVALUE},pSt)
			# pSt					= if aGECVisible (guiClose pSt) pSt
			# pSt					= appPIO (closeReceiver (GECIdtoId tGEC)) pSt
			# lSt					= {lSt & aGECVisible=False}
			# (bound,pSt)			= aGECGECVALUE.gecOpened pSt
			| bound
				# pSt				= aGECGECVALUE.gecClose pSt
				= (OutDone,(lSt,pSt))
			| otherwise
				= (OutDone,(lSt,pSt))
		aGECFun gGECa mv tGUIGEC=:{guiLocs,guiOpen} tGEC msg=:(InOpenGUI guiLoc objLoc) (lSt=:{aGECVisible,aGECGECVALUE,aGECGUILoc},pSt)
			| aGECVisible && sameGUILoc
				= (OutDone,(lSt,pSt))
			| otherwise
				# (lSt,pSt)			= if sameGUILoc (lSt,pSt)
									                (snd (aGECFun gGECa mv tGUIGEC tGEC (InCloseGUI SkipCONS) (lSt,pSt)))
				# [aLoc:_]			= guiLocs (guiLoc,objLoc)
				# pSt				= guiOpen guiLoc pSt
				# pSt				= aGECGECVALUE.gecOpenGUI aLoc pSt
				# lSt				= {lSt & aGECVisible=True,aGECGUILoc=guiLoc,aGECOBJECTLoc=objLoc}
				= (OutDone,(lSt,pSt))
		where
			sameGUILoc				= guiLoc==aGECGUILoc
		aGECFun _ _ {guiClose} _ msg=:(InCloseGUI _) (lSt=:{aGECVisible,aGECGECVALUE},pSt)
			# pSt				= guiClose pSt
			# pSt				= aGECGECVALUE.gecCloseGUI SkipCONS pSt
			# lSt				= {lSt & aGECVisible=False}
			= (OutDone,(lSt,pSt))
		aGECFun _ _ _ _ (InSwitchCONS path) (lSt=:{aGECGECVALUE},pSt)
			# pSt				= aGECGECVALUE.gecSwitch path pSt
			= (OutDone,(lSt,pSt))
		aGECFun _ _ _ _ (InArrangeCONS arr path) (lSt=:{aGECGECVALUE},pSt)
			# pSt				= aGECGECVALUE.gecArrange arr path pSt
			= (OutDone,(lSt,pSt))
		aGECFun _ _ _ _ _ st
			= (OutDone,st)
/**************************************************************************************/

/******************************************************************************************************
gGEC{|AGEC|} gGECa gecArgs=:{gec_value=mbimap,update=biupdate} pSt
	= case mbimap of 
		Just abstractGEC=:(Hidden bimapGEC gGECbimapGEC descr) 
					= convert abstractGEC (gGECbimapGEC {gecArgs & gec_value=Just bimapGEC,update=bupdate abstractGEC} pSt)
//		Nothing		= abort "Cannot make up function value for AGEC"
		Nothing		= hackdefault pSt	// default is an invisable constant editor, not very useful, but no crash !!
where
	hackdefault pSt 
	# (handle,pSt) 	= gGECa {gecArgs & gec_value=Nothing,update = \v r pst -> pst} pSt
	  (aval,pSt)	= handle.gecGetValue pSt
	= createDummyGEC OutputOnly (constGEC aval) biupdate pSt 
	
	
	convert abstractGEC (ahandle,pst) 
					= ({ahandle & gecSetValue = AGECSetValue ahandle.gecSetValue ahandle.gecGetValue
	                            , gecGetValue = AGECGetValue abstractGEC ahandle.gecGetValue
	                   },pst)

	AGECSetValue aSetValue aGetValue upd (Hidden nval _ descr) pst  
					= case aGetValue pst of
							(bimap,pst) -> aSetValue upd {bimap & value = nval.value} pst
							(else,pst) -> abort "cannot be"
	AGECGetValue (Hidden bimap gGECb descr) aGetValue pst
		# (nval,pst) = aGetValue pst
		= (Hidden {bimap & value = nval.value} gGECb descr,pst)
	
	bupdate (Hidden bimap gGECb descr) reason nbimap pst 
	= biupdate reason (Hidden {bimap & value = nbimap.value} gGECb descr) pst
******************************************************************************************************/