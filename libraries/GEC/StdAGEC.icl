implementation module StdAGEC

import genericgecs, guigecs, infragecs
import StdGECExt, basicAGEC

:: BimapGEC a b 	= 	{ toGEC   :: a (Current b) -> b		// specify how to convert a into b, possibly using previous b settings
						, fromGEC :: b -> a					// specify how to convert b back to a
						, updGEC  :: b -> b					// will be called each time a b-value has been changed
						, value   :: a						// store initial a-value, will automatically always contain latest a-value made with b-editor
						, pred    :: a -> (Bool,a)			// only pass through new value if its satisfies this predicate, display new value
						}

:: AGEC a = E. .b :  View !(BimapGEC a b) !(A. .ps: InfraGEC b (PSt ps)) !String
		
// BimapGEC:: a b to use a b-editor for constructing an a-value

mkBimapGEC  :: (a (Current b) -> b) (b -> b) (b -> a) (a -> (Bool,a)) a -> (BimapGEC a b)
mkBimapGEC toGEC updGEC fromGEC pred value 
=	{ toGEC   = toGEC
	, fromGEC = fromGEC
	, updGEC  = updGEC
	, value   = value
	, pred	  = pred
	}

to_BimapGEC :: (Bimap a b) a -> (BimapGEC a b)
to_BimapGEC {map_to,map_from} a 
= {toGEC = \a _ -> map_to a, fromGEC = map_from, updGEC = id, value = a, pred = \t -> (True,t)}

from_BimapGEC :: (BimapGEC a b) -> (Bimap a b)
from_BimapGEC bimapGEC
= {map_to = \a -> bimapGEC.toGEC a Undefined , map_from = bimapGEC.fromGEC}

invBimap :: (Bimap a b) -> (Bimap b a)
invBimap {map_to, map_from} = {map_to = map_from, map_from = map_to}

Bimap_BimapGEC :: (Bimap a b) (BimapGEC a va) -> (BimapGEC b va)
Bimap_BimapGEC bimapab bimapGava = {toGEC=toGEC,fromGEC=fromGEC,updGEC=updGEC,value=value,pred=pred}
where
	toGEC nb Undefined		= bimapGava.toGEC (bimapab.map_from nb) Undefined
	toGEC nb (Defined va)	= bimapGava.toGEC (bimapab.map_from nb) (Defined va)
	fromGEC va 	= bimapab.map_to (bimapGava.fromGEC va)
	updGEC va 	= bimapGava.updGEC va
	value		= bimapab.map_to bimapGava.value
	pred nb 	= (True,nb)
	
// and the editor specialized for BimapGEC:

gGEC{|BimapGEC|} _ gecb gecArgs=:{gec_value=Just bimapG} pSt
	= mkBimapGECeditor bimapG gecb gecArgs pSt

// mkBimapGECeditor is a handy general conversion function

mkBimapGECeditor :: (BimapGEC a b) (InfraGEC b *(PSt .ps)) -> (InfraGEC (BimapGEC a b) *(PSt .ps)) 
mkBimapGECeditor bimapG gecb = bimapgec
where
	bimapgec gecArgs=:{gec_value=Just initbimapG,update=bimapupdate} pSt
	= (convert bhandle,pSt1)
	where
		(bhandle,pSt1) = gecb {gecArgs & gec_value=Just initb,update=updateb bimapupdate bhandle} pSt  // show b editor
	
		convert bhandle	= {bhandle & gecSetValue = bimapSetValue bhandle
		                           , gecGetValue = bimapGetValue bhandle}
	
		initb = bimapG.toGEC initbimapG.value Undefined
	
		updateb bimapupdate bhandle reason b pst 
		# nb	= bimapG.updGEC  b
		# na	= bimapG.fromGEC nb
		= case  (bimapG.pred na) of
			(True,na) 	# pst	= bhandle.gecSetValue NoUpdate (bimapG.toGEC na (Defined  nb)) pst
						= bimapupdate reason {initbimapG & value = na} pst 
			(False,na) 	= bhandle.gecSetValue NoUpdate (bimapG.toGEC na (Defined  b)) pst 
	
		bimapSetValue bhandle upd bimap pst
		# (b,pst) = bhandle.gecGetValue pst
	 	= bhandle.gecSetValue upd (bimap.toGEC bimap.value (Defined b)) pst
	
		bimapGetValue bGetValue pst
		# (b,pst) = bhandle.gecGetValue pst
		= ({bimapG & value = initbimapG.fromGEC b},pst)

// AGEC:: Abstract version of BimapGEC

mkAGEC  :: !(BimapGEC a b) !String -> AGEC a | gGEC{|*|} b
mkAGEC bimapGEC descriptor =  View bimapGEC gGEC{|*|} descriptor

mkAGEC`  :: !(BimapGEC a (g b)) !String -> AGEC a | gGEC{|*->*|} g
mkAGEC` bimapGEC descriptor =  View bimapGEC (gGEC{|*->*|} undef1) descriptor
where
	undef1 = abort "mkAGEC` evaluated undefined GEC editor/1"

^^    :: (AGEC a) -> a
^^ (View bimap ggec string) = bimap.value

(^=) infixl  :: (AGEC a) a -> (AGEC a)
(^=) (View bimap ggec descr) nvalue = (View {bimap & value = nvalue} ggec descr)

Specialize :: a (a -> AGEC a) (GECArgs a (PSt .ps)) !(PSt .ps) -> *(!GECVALUE a (PSt .ps),!(PSt .ps))
Specialize a toAGEC gecArgs=:{gec_value=Nothing} pSt 
= AGEC_to_gGEC (toAGEC a) {gecArgs & gec_value = Just a} pSt
Specialize a toAGEC gecArgs=:{gec_value=Just na} pSt 
= AGEC_to_gGEC (toAGEC na) gecArgs pSt

AGEC_to_gGEC:: (AGEC a) -> (InfraGEC a (PSt .ps))
AGEC_to_gGEC ageca=:(View bimapGa infraGECva ss)
= a_GEC_as_b_GEC {bimapto = bimapto, bimapfrom = bimapfrom, infraGEC = ageceditor}
where
	ageceditor = mkStaticAgec undef 
	bimapfrom a = (View {bimapGa & value = a} infraGECva ss)	
	bimapto (View bimapGa infraGECva ss) = bimapGa.value

:: GECaGECb a ps = E.va : {bimapto :: va -> a, bimapfrom :: a -> va ,infraGEC :: (InfraGEC va *(PSt ps))}
	
a_GEC_as_b_GEC :: (GECaGECb b .ps)  -> (InfraGEC b *(PSt .ps))
a_GEC_as_b_GEC {bimapto = atob, bimapfrom = btoa, infraGEC = geca} 
	= bgec
where
	bgec gecArgs=:{gec_value=Just initb,update=updateb} pSt
	= (convert ahandle,pSt1)
	where
		(ahandle,pSt1) = geca {gecArgs & gec_value = Just (btoa initb),update=updatea updateb} pSt  // show a editor

		convert ahandle	= {ahandle & gecSetValue = bSetValue ahandle	// let agec act as bgec
		                           , gecGetValue = bGetValue ahandle}
	
		updatea updateb reason na pst 
		= updateb reason (atob na) pst

		bSetValue ahandle upd nb pst
	 	= ahandle.gecSetValue upd (btoa nb) pst
	
		bGetValue ahandle pst
		# (na,pst) = ahandle.gecGetValue pst
		= (atob na,pst)

AGEC_a_as_AGEC_b 	:: (Bimap a b) (AGEC a) -> AGEC b 
AGEC_a_as_AGEC_b bimap (View bimapGava gecva ss)
= (View (Bimap_BimapGEC bimap bimapGava) gecva ss)

// and the editor for an AGEC...

gGEC{|AGEC|} gecx gecArgs pSt
//	= mkStaticAgec gecx gecArgs pSt
	= mkDynamicAgec gecx gecArgs pSt

// nice definition for a paper, but it is not one that can deal with dynamically changing agec's

mkStaticAgec gecx gecArgs=:{gec_value=Just (View bimapG gecb ss)} pSt
= a_GEC_as_b_GEC {bimapto = bimapto, bimapfrom = bimapfrom, infraGEC = mkBimapGECeditor bimapG gecb} gecArgs pSt
where
	bimapto nbimapG 	= (View {bimapG & value = nbimapG.value} gecb ss) 
	bimapfrom (View nbimapG _ ss) = {bimapG & value = nbimapG.value}
mkStaticAgec gecx gecArgs=:{gec_value=Nothing} pSt
# (geca,pSt)	= gecx {gecArgs & gec_value=Nothing, update = \v r env -> env} pSt
# (a,   pSt)	= geca.gecGetValue pSt
# pSt			= geca.gecClose    pSt
= mkStaticAgec gecx {gecArgs & gec_value=Just (hidAGEC a)} pSt

// hard definition using low level stuf, but it *can* deal with dynamically changing agec's
// however, every different AGEC should have a different descriptor (string)

// the following definition is locally used in mkDynamicAgec

::	AGECSt a env
	=	E. b:
		{	agecBimapGEC:: !GECVALUE (BimapGEC a b) env		// The handle to the BimapGEC
		,	agecAGEC    :: !AGEC a							// The current AGEC value
		}

mkDynamicAgec gGECa gecArgs=:{gec_value=Just abstractGEC=:(View bimapGEC gGECb descr),update= biupdate,location,makeUpValue,outputOnly,hasOBJECT} pSt
# (abbaGEC,pSt)	= mkBimapGECeditor  bimapGEC gGECb {gecArgs & gec_value=Just bimapGEC, update =bupdate abstractGEC} pSt
# (aGEC,pSt)	= openGECId pSt
# aDef			= GECReceiver aGEC (fun bupdate)
# lSt			= { agecBimapGEC = abbaGEC
				  , agecAGEC     = abstractGEC
				  }
# (_,pSt)		= openReceiver lSt aDef pSt
= (newGEC aGEC,pSt)
where
	bupdate (View bimapGEC gGECb descr) reason nbimap pSt 
		= biupdate reason (View {bimapGEC & value = nbimap.value} gGECb descr) pSt
	
	fun :: !(A.b:(AGEC a) -> Update (BimapGEC a b) (PSt .ps)) !(GECMsgIn  (AGEC a)) !*(AGECSt a (PSt .ps),PSt .ps) -> (GECMsgOut (AGEC a), *(AGECSt a (PSt .ps),PSt .ps))
	fun _ InGetValue (lSt=:{agecBimapGEC=abbaGEC,agecAGEC=View bimapGEC gGECbimapGEC descr},pSt)
	# (nval,pSt)	= abbaGEC.gecGetValue pSt
	= (OutGetValue (View {bimapGEC & value=nval.value} gGECbimapGEC descr),(lSt,pSt))
	fun bupdate (InSetValue includeUpdate newAGECa=:(View nval ngGECb ndescr)) (lSt=:{agecBimapGEC=abbaGEC,agecAGEC=(View _ _ odescr)},pSt)
	| odescr == ndescr	// The same AGEC is used for the new value
		# (bimap,pSt)	= abbaGEC.gecGetValue pSt
		# pSt			= abbaGEC.gecSetValue includeUpdate {bimap & value=nval.value} pSt		// send new value down the current infrastructure
		= (OutDone,(lSt,pSt))
	| otherwise			// A different AGEC, of unknown type, is used for the new value
		# pSt			= abbaGEC.gecClose pSt													// close old infrastructure
		# (nabbaGEC,pSt)= mkBimapGECeditor nval ngGECb  { gec_value   = Just nval										// build new infrastructure
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

// generic function combining AGEC's
// it will only show the invokations made, all other types are by default not shown !

generic gAGEC d :: d -> AGEC d 
gAGEC{|Int|} 	   d = idAGEC d 
gAGEC{|Real|} 	   d = idAGEC d 
gAGEC{|String|}    d = idAGEC d 
gAGEC{|UNIT|} 	   d = idAGEC d 
gAGEC{|PAIR|} ba bb (PAIR a b) = mkPAIR_AGEC mymap (ba a) (bb b)
where
	mymap  = 	{	map_to		= \(PAIR a b) -> a <-> b
				, 	map_from 	= \(a <-> b ) -> PAIR a b
				}
gAGEC{|EITHER|} ba bb (LEFT  a) = mkEITHER_AGEC bimapId (LEFT  (ba a))
gAGEC{|EITHER|} ba bb (RIGHT b) = mkEITHER_AGEC bimapId (RIGHT (bb b))
gAGEC{|CONS|} dc (CONS c) = AGEC_a_as_AGEC_b mymap (dc c)
where
	mymap  = 	{	map_to = \c -> CONS c
				, 	map_from 	 = \(CONS c) ->  c
				}
gAGEC{|OBJECT|} do   (OBJECT o) = AGEC_a_as_AGEC_b mymap (do o)
where
	mymap  = 	{	map_to		= \o -> OBJECT o
				, 	map_from 	= \(OBJECT o) -> o
				}

//:: GECaGECb a ps = E.va : {bimapto :: va -> a, bimapfrom :: a -> va ,infraGEC :: (InfraGEC va *(PSt ps))}

import cast

bimap{|AGEC|} bimapagec` =  cast { map_to= map_to, map_from=map_from }
where
	map_to   (View bimapG agec ss) =   (View (Bimap_BimapGEC  bimapagec bimapG) agec ss)
	map_from (View bimapG agec ss) =   (View (Bimap_BimapGEC xbimapagec bimapG) agec ss)
	where
		xbimapagec = cast { map_to= bimapagec.map_from, map_from=bimapagec.map_to }

	bimapagec =  cast bimapagec`		

// utility function defined for gAGEC ...

mkPAIR_AGEC 		:: (A. ax bx:Bimap (PAIR ax bx) (tv ax bx)) (AGEC a) (AGEC b) -> AGEC (PAIR a b) | gGEC {|*->*->*|} tv
mkPAIR_AGEC bimap ageca=:(View bimapGava gecva sa) agecb=:(View bimapGbvb gecvb sb)
= (View bimapGPabvavb ggecGPtv (sa +++ sb))
where
	bimapGPabvavb  	= Bimap_BimapGEC_PAIR bimap bimapGava bimapGbvb
	ggecGPtv 		= gGEC{|*->*->*|} gecva gecvb 

	Bimap_BimapGEC_PAIR :: (A. ax bx:Bimap (PAIR ax bx) (tv ax bx)) (BimapGEC a va) (BimapGEC b vb) 
			-> (BimapGEC (PAIR a b) (tv va vb))
	Bimap_BimapGEC_PAIR bimapPtv bimapGava bimapGbvb = {toGEC=ptotv,fromGEC=vtoP,updGEC=updtvvavb,value=nv,pred=pred}
	where
		ptotv (PAIR a b) Undefined		= bimapPtv.map_to (PAIR (bimapGava.toGEC a Undefined) (bimapGbvb.toGEC b Undefined))
		ptotv (PAIR a b) (Defined vab)	= vab
		vtoP tvvavb 
		# (PAIR va vb) = (bimapPtv.map_from tvvavb)
		= (PAIR (bimapGava.fromGEC va) (bimapGbvb.fromGEC vb))
		updtvvavb tvvavb 	= id tvvavb
		nv 			= (PAIR bimapGava.value bimapGbvb.value)
		pred nb 	= (True,nb)


mkEITHER_AGEC 		:: (A. ax bx:Bimap (EITHER ax bx) (tv ax bx)) (EITHER (AGEC a) (AGEC b)) 
							-> AGEC (EITHER a b) | gGEC {|*->*->*|} tv
mkEITHER_AGEC bimap (LEFT ageca=:(View bimapGava gecva sa))
= (View bimapLEFT ggecGPtv ("LEFT" +++ sa))
where
	bimapLEFT  		= Bimap_BimapGEC_EITHER bimap (LEFT bimapGava)
	ggecGPtv 		= gGEC{|*->*->*|} gecva gecva
mkEITHER_AGEC bimap (RIGHT ageca=:(View bimapGbvb gecvb sb))
= (View bimapRIGHT ggecGPtv ("RIGHT" +++ sb))
where
	bimapRIGHT  	= Bimap_BimapGEC_EITHER bimap (RIGHT bimapGbvb)
	ggecGPtv 		= gGEC{|*->*->*|} gecvb gecvb

Bimap_BimapGEC_EITHER :: (A. ax bx:Bimap (EITHER ax bx) (tv ax bx)) (EITHER (BimapGEC a va) (BimapGEC b vb)) 
		-> (BimapGEC (EITHER a b) (tv va vb))
Bimap_BimapGEC_EITHER bimapEtv (LEFT bimapGava) 
= {toGEC=toGEC,fromGEC=fromGEC,updGEC=id,value=value,pred=pred}
where
	toGEC (LEFT a) Undefined		= bimapEtv.map_to (LEFT (bimapGava.toGEC a Undefined))
	toGEC (LEFT a) (Defined vab)	= vab // is this ok ??
	fromGEC left 
	# (LEFT va) = (bimapEtv.map_from left)
	= (LEFT (bimapGava.fromGEC va))
	value 		= (LEFT bimapGava.value)
	pred nb 	= (True,nb)
Bimap_BimapGEC_EITHER bimapEtv (RIGHT bimapGava) 
= {toGEC=toGEC,fromGEC=fromGEC,updGEC=id,value=value,pred=pred}
where
	toGEC (RIGHT b) Undefined		= bimapEtv.map_to (RIGHT (bimapGava.toGEC b Undefined))
	toGEC (RIGHT b) (Defined vab)	= vab // is this ok ??
	fromGEC right 
	# (RIGHT vb) = (bimapEtv.map_from right)
	= (RIGHT (bimapGava.fromGEC vb))
	value 		= (RIGHT bimapGava.value)
	pred nb 	= (True,nb)


// utilities for making BimapGECs out of bimaps

gGEC{|GecComb|} gGECa gGECb args pSt
	= abort "Cannot make up function value for DataGec"	

AGECtoCGEC :: String	(AGEC a) 		-> (GecCircuit a a) 	| gGEC{|*|}/*, generate{|*|}*/ a
AGECtoCGEC sa agec =  arr (\a -> agec ^= a) >>> edit sa >>> arr (\agec -> (^^ agec))

CGECtoAGEC :: 			(GecCircuit a a ) a 	-> (AGEC a) 	| gGEC{|*|} a		// Use CGEC as AGEC 
CGECtoAGEC cgec a 
= mkAGEC { toGEC   = \a _ -> {inout = (Hide a,Hide a), gec = arr (\(Hide a) -> a) >>> cgec >>> arr (\a -> Hide a)}
		 , fromGEC = \{inout = (a,Hide b)} = b
		 , updGEC  = id
		 , value   = a
		 , pred	   = \na -> (True,na)
		 } "CGECtoAGEC"


// all sort of perhap useful utility functions ...

idiAGEC :: (A. .ps: InfraGEC a *(PSt .ps)) a -> AGEC a 
idiAGEC gec j 	= View 	{	toGEC	= \i _ ->i
						,	fromGEC = id
						,	value	= j
						,	updGEC	= id
						,	pred	= \t -> (True,t)
						} gec "idGEC"


mkiAGEC  :: (A. .ps: InfraGEC b *(PSt .ps)) !(BimapGEC a b) !String -> AGEC a 
mkiAGEC gec bimapGEC descriptor  =  View bimapGEC gec descriptor 

mkiAGEC2  :: (A. .ps: (GECaGECb a .ps)) a !String -> AGEC a 
mkiAGEC2 {bimapto = vatoa, bimapfrom = atova ,infraGEC = gecva} a descriptor  
=  View (to_BimapGEC {map_to = atova, map_from = vatoa} a) gecva descriptor 


	
			
