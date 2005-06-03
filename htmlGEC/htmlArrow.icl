implementation module htmlArrow

import StdEnv
import htmlDataDef, htmlHandler, htmlFormlib
import StdArrow

startCircuit :: (GecCircuit a b) a *HSt -> (Form b,*HSt) 
startCircuit (HGC circuit) initval hst 
# ((val,body),bool,hst) = circuit ((initval,[]),False,hst)
= (	{changed= True			// should be fixed
	,value	= val
	,form	= reverse (removedup body [])
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

edit :: FormId -> GecCircuit a a |  gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
edit formid = HGC mkApplyEdit`
where
	mkApplyEdit` :: ((a,[(String,BodyTag)]),Bool,*HSt ) -> ((a,[(String,BodyTag)]),Bool,*HSt) |  gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
	mkApplyEdit` ((initval,prevbody),False,hst) 
	# (na,hst) = mkApplyEditForm formid initval initval hst
	= ((na.value,[(formid.id,BodyTag na.form):prevbody]),False,hst)
	mkApplyEdit` ((initval,prevbody),True,hst) // second time I come here: don't use the old state, but the new one ! 
	# (na,hst) = mkSetForm {formid & mode = Edit} initval  hst 
	= ((na.value,[(formid.id,BodyTag na.form):prevbody]),True,hst)

display :: FormId -> GecCircuit a a |  gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
display formid = HGC mkEditForm`
where
	mkEditForm` ((val,prevbody),bool,hst) 
	# (na,hst) = mkEditForm {formid & mode = Display} val hst
	= ((na.value,[(formid.id,BodyTag na.form):prevbody]),bool,hst)

store :: FormId s -> GecCircuit (s -> s) s |  gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC s
store formid initstore = HGC mkStoreForm`
where
	mkStoreForm` ((fun,prevbody),bool,hst) 
	# (store,hst) = mkStoreForm formid initstore fun  hst
	= ((store.value,[(formid.id,BodyTag store.form):prevbody]),bool,hst)

self :: (a -> a) (GecCircuit a a) -> GecCircuit a a
self fun gecaa = gecaa >>> arr fun
	
feedback :: (GecCircuit a b) (GecCircuit b a) -> (GecCircuit a b)
feedback (HGC gec_ab) (HGC gec_ba) = HGC feedback`
where
	feedback` input
	# (res,bool,hst) = (gec_ba o gec_ab) input 
	# (res,bool,hst) = gec_ab (res,True,hst)
	= (res,False,hst)							// indicates that we loop from here 

lift :: FormId (FormId a *HSt -> (Form b,*HSt)) -> (GecCircuit a b)
lift formid fun = HGC fun`
where
	fun` ((a,body),bool,hst)
	# (nb,hst) =  fun formid a hst
	= ((nb.value,[(formid.id,BodyTag nb.form):body]),bool,hst) 

