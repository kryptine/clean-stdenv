implementation module htmlHGECarrow

import StdEnv
import htmlDataDef, htmlHandler
import StdArrow

startCircuit :: (GecCircuit a b) a *HSt -> ((b,[BodyTag]),*HSt) 
startCircuit (HGC circuit) initval hst 
# ((val,body),bool,hst) = circuit ((initval,[]),False,hst)
= ((val,reverse (removedup body [])),hst)
where
	removedup [] _ = []
	removedup [(id,body):rest] ids
	| isMember id ids = removedup rest ids
	| otherwise = [body: removedup rest [id:ids]]


// Bool is used for feedback loop
// True for second round (take argument, not current (updated) value stored in global state)

//:: GecCircuit a b = GecCircuit !.(A. .ps: (GecSet b ps) *(PSt ps) -> *(GecSet a ps, *PSt ps))

:: GecCircuit a b 	
	= HGC !(*((a, [(String,BodyTag)]), Bool, *HSt )  -> *((b,[(String,BodyTag)]),Bool,*HSt))

instance Arrow GecCircuit
where
	arr :: (a -> b) -> GecCircuit a b
	arr fun = HGC fun`
	where
		fun` ((a,body),bool,hst) = ((fun a,body),bool,hst)

	(>>>) infixr 1 :: (GecCircuit a b) (GecCircuit b c) -> GecCircuit a c
	(>>>) (HGC gec_ab) (HGC gec_bc) = HGC (gec_bc o gec_ab)

	first :: (GecCircuit a b) -> GecCircuit (a, c) (b, c)
	first (HGC gec_ab) = HGC first`
	where
		first` (((a,c),prevbody),bool,hst)
		# ((b,bodya),bool,hst) = gec_ab ((a,prevbody),bool,hst)
		= (((b,c),bodya),bool,hst)

edit :: String -> GecCircuit a a |  gHGEC{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} a
edit title = HGC mkApplyEdit`
where
	mkApplyEdit` :: ((a,[(String,BodyTag)]),Bool,*HSt ) -> ((a,[(String,BodyTag)]),Bool,*HSt) |  gHGEC{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} a
	mkApplyEdit` ((initval,prevbody),False,hst) 
	# ((a,bodya),hst) = mkApplyEditHGEC title initval initval hst
	= ((a,[(title,bodya):prevbody]),False,hst)
	mkApplyEdit` ((initval,prevbody),True,hst) // second time I come here: don't use the old state, but the new one ! 
	# ((a,bodya),hst) = mkEditHGEC2 title HEdit initval hst //to be implemented
	= ((a,[(title,bodya):prevbody]),True,hst)

display :: String -> GecCircuit a a |  gHGEC{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} a
display title = HGC mkEditHGEC`
where
	mkEditHGEC` ((val,prevbody),bool,hst) 
	# ((a,bodya),hst) = mkEditHGEC title HDisplay val hst
	= ((a,[(title,bodya):prevbody]),bool,hst)

store :: String s -> GecCircuit (s -> s) s |  gHGEC{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} s
store title initstore = HGC mkStoreHGEC`
where
	mkStoreHGEC` ((fun,prevbody),bool,hst) 
	# ((store,bodystore),hst) = mkStoreHGEC title fun initstore hst
	= ((store,[(title,bodystore):prevbody]),bool,hst)

self :: (a -> a) (GecCircuit a a) -> GecCircuit a a
self fun gecaa = gecaa >>> arr fun
	
feedback :: (GecCircuit a b) (GecCircuit b a) -> (GecCircuit a b)
feedback (HGC gec_ab) (HGC gec_ba) = HGC feedback`
where
	feedback` input
	# (res,bool,hst) = (gec_ba o gec_ab) input 
	# (res,bool,hst) = gec_ab (res,True,hst)
	= (res,False,hst)							// indicates that we loop from here 

lift :: String HMode (String HMode a *HSt -> ((b,BodyTag),*HSt)) -> (GecCircuit a b)
lift name mode fun = HGC fun`
where
	fun` ((a,body),bool,hst)
	# ((b,nbody),hst) =  fun name mode a hst
	= ((b,[(name,nbody):body]),bool,hst) 

