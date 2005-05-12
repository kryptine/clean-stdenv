implementation module htmlArrow

import StdEnv
import htmlDataDef, htmlHandler
import StdArrow

startCircuit :: (GecCircuit a b) a *HSt -> (Form b,*HSt) 
startCircuit (HGC circuit) initval hst 
# ((val,body),bool,hst) = circuit ((initval,[]),False,hst)
= (	{changed= True			// should be fixed
	,value	= val
	,body	= reverse (removedup body [])
	},hst)
where
	removedup [] _ = []
	removedup [(id,body):rest] ids
	| isMember id ids = removedup rest ids
	| otherwise = [body: removedup rest [id:ids]]


// Bool is used for feedback loop
// True for second round (take argument, not current (updated) value stored in global state)

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

edit :: FormId -> GecCircuit a a |  gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} a
edit title = HGC mkApplyEdit`
where
	mkApplyEdit` :: ((a,[(String,BodyTag)]),Bool,*HSt ) -> ((a,[(String,BodyTag)]),Bool,*HSt) |  gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} a
	mkApplyEdit` ((initval,prevbody),False,hst) 
	# (na,hst) = mkApplyEditForm title initval initval hst
	= ((na.value,[(title,BodyTag na.body):prevbody]),False,hst)
	mkApplyEdit` ((initval,prevbody),True,hst) // second time I come here: don't use the old state, but the new one ! 
	# (na,hst) = mkEditForm/*2*/ title Edit initval hst //to be implemented
	= ((na.value,[(title,BodyTag na.body):prevbody]),True,hst)

display :: FormId -> GecCircuit a a |  gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} a
display title = HGC mkEditForm`
where
	mkEditForm` ((val,prevbody),bool,hst) 
	# (na,hst) = mkEditForm title Display val hst
	= ((na.value,[(title,BodyTag na.body):prevbody]),bool,hst)

store :: FormId s -> GecCircuit (s -> s) s |  gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} s
store title initstore = HGC mkStoreForm`
where
	mkStoreForm` ((fun,prevbody),bool,hst) 
	# (store,hst) = mkStoreForm title fun initstore hst
	= ((store.value,[(title,BodyTag store.body):prevbody]),bool,hst)

self :: (a -> a) (GecCircuit a a) -> GecCircuit a a
self fun gecaa = gecaa >>> arr fun
	
feedback :: (GecCircuit a b) (GecCircuit b a) -> (GecCircuit a b)
feedback (HGC gec_ab) (HGC gec_ba) = HGC feedback`
where
	feedback` input
	# (res,bool,hst) = (gec_ba o gec_ab) input 
	# (res,bool,hst) = gec_ab (res,True,hst)
	= (res,False,hst)							// indicates that we loop from here 

lift :: FormId Mode (FormId Mode a *HSt -> (Form b,*HSt)) -> (GecCircuit a b)
lift name mode fun = HGC fun`
where
	fun` ((a,body),bool,hst)
	# (nb,hst) =  fun name mode a hst
	= ((nb.value,[(name,BodyTag nb.body):body]),bool,hst) 

