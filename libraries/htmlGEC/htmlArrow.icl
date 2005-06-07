implementation module htmlArrow

import StdEnv
import htmlDataDef, htmlHandler, htmlFormlib
import StdArrow

startCircuit :: (GecCircuit a b) a *HSt -> (Form b,*HSt) 
startCircuit (HGC circuit) initval hst 
# ((val,body),ch,hst) = circuit ((initval,[]),False,hst)
= (	{changed= ch
	,value	= val
	,body	= reverse (removedup body [])
	},hst)
where
	removedup [] _ = []
	removedup [(id,body):rest] ids
	| isMember id ids = removedup rest ids
	| otherwise = [body: removedup rest [id:ids]]

:: GecCircuit a b 	
	= HGC !((GecCircuitState a)  -> GecCircuitState b)
:: *GecCircuitState a :== *((a, [(String,BodyTag)]), GecCircuitChanged, *HSt )
:: GecCircuitChanged :== Bool


instance Arrow GecCircuit
where
	arr :: (a -> b) -> GecCircuit a b
	arr fun = HGC fun`
	where
		fun` ((a,body),ch,hst) = ((fun a,body),ch,hst)

	(>>>) infixr 1 :: (GecCircuit a b) (GecCircuit b c) -> GecCircuit a c
	(>>>) (HGC gec_ab) (HGC gec_bc) = HGC (gec_bc o gec_ab)

	first :: (GecCircuit a b) -> GecCircuit (a, c) (b, c)
	first (HGC gec_ab) = HGC first`
	where
		first` (((a,c),prevbody),ch,hst)
		# ((b,bodya),ch,hst) = gec_ab ((a,prevbody),ch,hst)
		= (((b,c),bodya),ch,hst)

edit :: FormId -> GecCircuit a a |  gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
edit formid = HGC mkApplyEdit`
where
	mkApplyEdit` :: (GecCircuitState a)  -> GecCircuitState a |  gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
	mkApplyEdit` ((initval,prevbody),ch,hst) 
	# (na,hst) = mkApplyEditForm formid initval initval hst
	= ((na.value,[(formid.id,BodyTag na.body):prevbody]),ch||na.changed,hst) // propagate change

display :: FormId -> GecCircuit a a |  gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
display formid = HGC mkEditForm`
where
	mkEditForm` ((val,prevbody),ch,hst) 
	# (na,hst) = mkEditForm {formid & mode = Display} val hst
	= ((na.value,[(formid.id,BodyTag na.body):prevbody]),ch||na.changed,hst)

store :: FormId s -> GecCircuit (s -> s) s |  gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC s
store formid initstore = HGC mkStoreForm`
where
	mkStoreForm` ((fun,prevbody),ch,hst) 
	# (store,hst) = mkStoreForm formid initstore fun  hst
	= ((store.value,[(formid.id,BodyTag store.body):prevbody]),ch||store.changed,hst)

self :: (a -> a) (GecCircuit a a) -> GecCircuit a a
self fun gecaa = feedback gecaa (arr fun)
	
feedback :: (GecCircuit a b) (GecCircuit b a) -> (GecCircuit a b)
feedback (HGC gec_ab) (HGC gec_ba) = HGC (gec_ab o gec_ba o gec_ab)

lift :: FormId (FormId a *HSt -> (Form b,*HSt)) -> (GecCircuit a b)
lift formid fun = HGC fun`
where
	fun` ((a,body),ch,hst)
	# (nb,hst) =  fun formid a hst
	= ((nb.value,[(formid.id,BodyTag nb.body):body]),ch||nb.changed,hst) 

