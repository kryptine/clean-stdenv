implementation module StdDynamicGEC

import StdEnv
import StdIO
import genericgecs
import StdGEC, StdGECExt, StdAGEC
import StdGecComb
import EstherInterFace
from EstherParser import prettyDynamic
import StdDynamic, StdPSt, iostate

// TO TEST JUST REPLACE THE EXAMPLE NAME IN THE START RULE WITH ANY OF THE EXAMPLES BELOW
// ALL EXAMPLES HAVE TO BE OF FORM pst -> pst

dynamicGEC :: a -> AGEC a | TC a & gGEC {|*|} a	 					
dynamicGEC v = mkAGEC { toGEC   = toExpr
					  , fromGEC = fromExpr
					  , updGEC  = updExpr
					  , value   = v
					  } "dynamicGEC"
where
	toExpr v Undefined 		= display v (prettyVal  v)
	toExpr v (Defined b)	= b 				

	display v s 		= (hidGEC (v,s)) <-> (prettyValD v s) <->(prettyType v) <-> DynStr (dynamic v) s 

	fromExpr (hvs <-> _ <-> _ <-> _ )	= fst (^^ hvs)
	
	updExpr (_   <-> _ <-> _ <-> DynStr nd=:(d::a^) s)	= display d s
	updExpr (hvs <-> _ <-> _ <-> _) 					= display (fst (^^ hvs)) (snd (^^ hvs))

	prettyVal  v  	= fst (prettyDynamic (dynamic v))
	prettyValD v s	= case (dynamic v) of
						(x::(a -> b)) = Display (strip(s +++ " "))
						else		  = Display (fst(prettyDynamic (dynamic v)) +++ " ")  // +++ " " caused by bug in Display
	prettyType v	= Display (":: " +++ (snd(prettyDynamic (dynamic v))) +++ " ")

	strip s = { ns \\ ns <-: s | ns >= '\020' && ns <= '\0200'}	
	
:: DynString = DynStr Dynamic String

gGEC{|DynString|} gecArgs=:{gec_value=mbexpr,update=biupdate} pSt
	= case mbexpr of 
		Just dynexprGEC=:(DynStr dyn str) 
					= convert dynexprGEC (gGEC{|*|} {gecArgs & gec_value=Just str,update=bupdate dynexprGEC} pSt)
//		Nothing		= abort "Cannot make up function value for Dynam"
		Nothing		= convert (DynStr (dynamic "") "") (gGEC{|*|} {gecArgs & gec_value=Just "",update=bupdate (DynStr (dynamic "") "")} pSt)
where
	convert dynexprGEC (ahandle,pst) 
					= ({ahandle & gecSetValue = AGECSetValue ahandle.gecSetValue ahandle.gecGetValue
	                            , gecGetValue = AGECGetValue dynexprGEC ahandle.gecGetValue
	                   },pst)

	AGECSetValue aSetValue aGetValue upd (DynStr dyn str) pst  
					= aSetValue upd str pst
	AGECGetValue (DynStr dyn str) aGetValue pst
		# (nstr,pst) = aGetValue pst
		  (ndyn,pst) = applyWorld (stringToDynamic nstr) pst
		= (DynStr ndyn nstr,pst)
	
	bupdate (DynStr dyn str) reason nstr pst 
	# (ndyn,pst)= applyWorld (stringToDynamic nstr) pst
	= biupdate reason (DynStr ndyn nstr) pst

applyWorld :: (*World -> (!a,!*World)) *(PSt .pst) -> (!a,!*(PSt .pst))
applyWorld funworld pst
# (w,pst) 	= accPIO ioStGetWorld pst
  (a,w)		= funworld w
  pst 		= appPIO (ioStSetWorld w) pst
= (a,pst)

// this instance was needed because I could not create an instance of gGEC for ::T a  without | gGEC {|a|} a

gGEC{|(->)|} gGECa gGECb args=:{gec_value = Just (id), update = modeupdate} pSt
= createDummyGEC OutputOnly (id) modeupdate pSt

