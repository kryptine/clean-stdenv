implementation module StdAGEC

import genericgecs, guigecs, infragecs
import StdGECExt, basicAGEC
		
// bimap GEC a b to use a b-editor for constructing an a-value

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

mkBimapGEC  :: (a (Current b) -> b) (b -> b) (b -> a) a -> (BimapGEC a b)
mkBimapGEC toGEC updGEC fromGEC value 
=	{ toGEC   = toGEC
	, fromGEC = fromGEC
	, updGEC  = updGEC
	, value   = value
	}

mkAGEC  :: (BimapGEC a b) String -> AGEC a | gGEC{|*|} b
mkAGEC bimapGEC descriptor =  Hidden bimapGEC (gGEC{|*->*->*|} undef gGEC{|*|}) descriptor

^^    :: (AGEC a) -> a
^^ (Hidden bimap ggec string) = bimap.value

(^=) infixl  :: (AGEC a) a -> (AGEC a)
(^=) (Hidden bimap ggec descr) nvalue = (Hidden {bimap & value = nvalue} ggec descr)

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
