implementation module StdGecComb

import genericgecs
import StdGECExt, store, StdFunc, StdMisc
import StdAGEC

:: GecRec a b ps =
		{	value	:: b
		,	get		:: *(PSt ps) -> *(b, *PSt ps)
		,	set		:: IncludeUpdate a *(PSt ps) -> *PSt ps
		}

:: CGEC a b
	= GEC !.(A. .ps: (GecRec b a ps) *(PSt ps) -> *(GecRec a b ps, *PSt ps))

CGEC :: !(CGEC a b) a *(PSt .ps) -> *PSt .ps
CGEC (GEC gec) a pst =
	let (_, pst1) = gec {value = a, get = \pst -> (a, pst), set = \_ _ pst -> pst} pst
	in pst1

gecEdit :: String -> CGEC a a | gGEC{|*|} a 
gecEdit title = GEC \rec=:{value, set} pst ->
	let ({gecGetValue, gecSetValue}, pst1) = createNGEC title Interactive value (\r -> set (maybeUpdate r)) pst
	in  ({rec & get = gecGetValue, set = gecSetValue}, pst1)

gecDisplay :: String -> CGEC a a | gGEC{|*|} a 
gecDisplay title = GEC \rec=:{value, set} pst ->
	let ({gecGetValue, gecSetValue}, pst1) = createNGEC title OutputOnly value (\r -> set (maybeUpdate r)) pst
	in  ({rec & get = gecGetValue, set = gecSetValue}, pst1)

gecFun :: (a -> b) -> CGEC a b
gecFun f = GEC \rec=:{value, set} pst ->
	let ({gecGetValue, gecSetValue}, pst1) = createDummyGEC OutputOnly (Hide value) (\r (Hide na) -> set (maybeUpdate r)  (f na)) pst
	in  ({value = f value, get = Get gecGetValue, set = Set gecSetValue}, pst1)
where
	Get gecGetValue pst
	# (Hide na,pst) = gecGetValue pst
	= (f na,pst)
	
	Set gecSetValue r na pst
	= gecSetValue r (Hide na) pst

gecConst :: b -> CGEC a b 
gecConst b = GEC \recba  pst ->
		(	{	value	= b
			,	get		= \pSt -> (b,pSt)
			,	set		= \upd a pSt -> pSt
			}, pst)

gecIO :: (A. .ps: a *(PSt .ps) -> *PSt .ps) -> CGEC a a
gecIO f = GEC \rec=:{set} pst -> ({rec & set = \r a pst -> set r a (case r of YesUpdate -> f a pst; _ -> pst)}, pst)

%| :: !(CGEC a a) -> CGEC a a
%| (GEC gec1) = GEC \rec2=:{set=set2} pst -> 
	let (rec1=:{set=set1}, pst1) = gec1 {rec2 & set = \r a pst -> set2 r a (set1 NoUpdate a pst)} pst
	in  (rec1, pst1)

(|&|) infixl 4 :: !(CGEC a b) !(CGEC b c) -> CGEC a c 
(|&|) (GEC gec1) (GEC gec2) = GEC \rec3=:{value=a, get=get3} pst -> 
	let (rec2=:{value=c, get=get2}, pst1) = gec2 {rec3 & value = b, get = get1} pst
		(rec1=:{value=b, get=get1}, pst2) = gec1 {rec2 & value = a, get = get3} pst1
	in  ({rec1 & value = c, get = get2}, pst2)

(|>|) infix 5 :: !(CGEC a b) !(CGEC a c) -> CGEC a (b, c)
(|>|) (GEC gec1) (GEC gec2) = GEC \rec3=:{set=set3} pst -> 
	let (rec2=:{value=c, get=get2, set=set2}, pst1) = gec2 {rec3 & set = \r c pst -> let (b, pst`) = get1 pst in set3 r (b, c) pst`} pst
		(rec1=:{value=b, get=get1, set=set1}, pst2) = gec1 {rec3 & set = \r b pst -> let (c, pst`) = get2 pst in set3 r (b, c) pst`} pst1
	in  ({value = (b, c), get = \pst -> let (b, pst`) = get1 pst; (c, pst``) = get2 pst` in ((b, c), pst``), set = \r a pst -> set2 r a (set1 r a pst)}, pst2)

(|<>|) infix 5 :: !(CGEC a b) !(CGEC c d) -> CGEC (a, c) (b, d)
(|<>|) (GEC gec1) (GEC gec2) = GEC \rec3=:{value=(a, c), get=get3, set=set3} pst ->
	let (rec2=:{value=d, get=get2, set=set2}, pst1) = gec2 {value=c, get = \pst -> let ((_, d), pst`) = get3 pst in (d, pst`), set = \r c pst -> let (a, pst`) = get1 pst in set3 r (a, c) pst`} pst
		(rec1=:{value=b, get=get1, set=set1}, pst2) = gec1 {value=a, get = \pst -> let ((b, _), pst`) = get3 pst in (b, pst`), set = \r a pst -> let (c, pst`) = get2 pst in set3 r (a, c) pst`} pst1
	in  ({value = (b, d), get = \pst -> let (b, pst`) = get1 pst; (d, pst``) = get2 pst` in ((b, d), pst``), set = \r (a, b) pst -> set2 r b (set1 r a pst)}, pst2)

(@|) infixl 6 :: (a -> b) !(CGEC b c) -> CGEC a c
(@|) f (GEC gec1) = GEC \rec2=:{value=a, get=get2, set=set2} pst -> 
	let (rec1=:{set=set1}, pst1) = gec1 {rec2 & value = f a, get = \pst -> let (a, pst`) = get2 pst in (f a, pst`)} pst
	in  ({rec1 & set = \r a pst -> set1 r (f a) pst}, pst1)

(|@) infixl 6 :: !(CGEC a b) (b -> c) -> CGEC a c
(|@) (GEC gec1) f = GEC \rec2=:{set=set2} pst -> 
	let (rec1=:{value=b, get=get1}, pst1) = gec1 {rec2 & set = \r b pst -> set2 r (f b) pst} pst
	in  ({rec1 & value = f b, get = \pst -> let (b, pst`) = get1 pst in (f b, pst`)}, pst1)

(@|@) infixl 6 :: !(a -> b, b -> a) !(CGEC b b) -> CGEC a a
(@|@) (f, g) (GEC gec1) = GEC \{value=a, get=get2, set=set2} pst -> 
	let ({value=b, get=get1, set=set1}, pst1) = gec1 {value = f a, get = \pst -> let (a, pst`) = get2 pst in (f a, pst`), set = \r b pst -> set2 r (g b) pst} pst
	in  ({value = g b, get = \pst -> let (b, pst`) = get1 pst in (g b, pst`), set = \r a pst -> set1 r (f a) pst}, pst1)

(|>>>|) infixl 4 :: !(CGEC a b) !(CGEC b c) -> CGEC a c 
(|>>>|) (GEC gec1) (GEC gec2) = GEC \rec3=:{value=a, get=get3} pst -> 
	let (rec2=:{value=c, get=get2}, pst1) = gec2 {rec3 & value = b, get = get1} pst
		(rec1=:{value=b, get=get1}, pst2) = gec1 {rec2 & value = a, get = get3} pst1
	in  ({rec1 & value = c, get = get2}, pst2)

(|<<<|) infixl 4 :: !(CGEC b c) !(CGEC a b) -> CGEC a c 
(|<<<|) gec1 gec2 = gec2 |>>>| gec1

(|***|) infix 5 :: !(CGEC a b) !(CGEC c d) -> CGEC (a, c) (b, d)
(|***|) (GEC gec1) (GEC gec2) = GEC \rec3=:{value=(a, c), get=get3, set=set3} pst ->
	let (rec2=:{value=d, get=get2, set=set2}, pst1) = gec2 {value=c, get = \pst -> let ((_, d), pst`) = get3 pst in (d, pst`), set = \r c pst -> let (a, pst`) = get1 pst in set3 r (a, c) pst`} pst
		(rec1=:{value=b, get=get1, set=set1}, pst2) = gec1 {value=a, get = \pst -> let ((b, _), pst`) = get3 pst in (b, pst`), set = \r a pst -> let (c, pst`) = get2 pst in set3 r (a, c) pst`} pst1
	in  ({value = (b, d), get = \pst -> let (b, pst`) = get1 pst; (d, pst``) = get2 pst` in ((b, d), pst``), set = \r (a, b) pst -> set2 r b (set1 r a pst)}, pst2)

(|&&&|) infix 5 :: !(CGEC a b) !(CGEC a c) -> CGEC a (b, c)
(|&&&|) (GEC gec1) (GEC gec2) = GEC \rec3=:{set=set3} pst -> 
	let (rec2=:{value=c, get=get2, set=set2}, pst1) = gec2 {rec3 & set = \r c pst -> let (b, pst`) = get1 pst in set3 r (b, c) pst`} pst
		(rec1=:{value=b, get=get1, set=set1}, pst2) = gec1 {rec3 & set = \r b pst -> let (c, pst`) = get2 pst in set3 r (b, c) pst`} pst1
	in  ({value = (b, c), get = \pst -> let (b, pst`) = get1 pst; (c, pst``) = get2 pst` in ((b, c), pst``), set = \r a pst -> set2 r a (set1 r a pst)}, pst2)


gecloop:: !(CGEC (a,c) (b,c)) -> CGEC a b
gecloop (GEC gec1) = GEC \rec2=:{value=a,get=geta,set=setb} pst -> 
	let (rec1=:{value=bc,get=getbc,set=setac}, pst1) 
			= gec1 {value=(a, abort "(a,snd bc)"), get=getac geta getbc, set=setbc geta setb setac} pst
	in  ({value=abort "fst bc",get=getb getbc,set=seta getbc setac}, pst1)
where
	getb getbc ps
	# ((b,c),ps) = getbc ps
	= (b,ps)
	
	seta getbc setac r a ps 
	# ((b,c),ps) = getbc ps
	= setac r (a,c) ps

	getac geta getbc ps
	# (a,ps)     = geta ps
	# ((b,c),ps) = getbc ps
	= ((a,c),ps)
	
	setbc geta setb setac r (b,c) ps
	# (a,ps)     = geta ps
	# ps = setac NoUpdate (a,c) ps
	= setb r b ps
	
gecfirst :: !(CGEC a b) -> CGEC (a,c) (b,c)
gecfirst gec =  gec |<>| gecFun id

gecsecond :: !(CGEC a b) -> CGEC (c,a) (c,b)
gecsecond gec =  gecFun id |<>| gec

AGECtoCGEC :: String (AGEC a) -> (CGEC a a) | gGEC{|*|} a
AGECtoCGEC sa agec =  (\a -> agec ^= a) @| gecEdit sa |@ (\agec -> (^^ agec))

CGECtoAGEC :: (CGEC a a ) a -> (AGEC a) | gGEC{|*|} a
CGECtoAGEC cgec a = mkAGEC  { toGEC   = \a _ -> {inout = (a,a), gec = cgec}
							, fromGEC = \{inout} = snd inout
							, updGEC  = id
							, value   = a
							}

maybeUpdate :: !UpdateReason -> .IncludeUpdate
maybeUpdate Changed = YesUpdate
maybeUpdate _ = NoUpdate

gGEC{|GecComb|} gGECa gGECb args=:{gec_value=Just {inout = (inVal,outVal),gec=GEC gecfun},update=biupdate} pSt
=  convert pSt2
where
	(gecrec, pSt1)  = gecfun {value = inVal, get = \pst -> (inVal, pst), set = updateio} pSt
	(iohandle,pSt2) = gGEC {|*->*->*|} gGECa gGECb {args & gec_value=Just (inVal,outVal),update=ioupdate}  pSt1

	convert pSt = 
		( 	{ iohandle 	& gecGetValue = getval
						, gecSetValue = \upd {inout=(ninVal,_)} pst -> pst //gecrec.set upd ninVal pst 
			} ,pSt)

	getval pst
	# (noutVal,pst)= gecrec.get pst
	# ((ninVal,_),pst) = iohandle.gecGetValue pst
	= ({gec=GEC gecfun,inout=(ninVal,noutVal)},pst)

	ioupdate reason (ninVal,nOutVal) pst 
	# pst = gecrec.set YesUpdate ninVal pst
	= biupdate reason {gec=GEC gecfun,inout=(inVal,nOutVal)} pst

	updateio upd noutVal pst 
	# ((ninVal,_),pst) = iohandle.gecGetValue pst
	= iohandle.gecSetValue NoUpdate (ninVal,noutVal) pst

gGEC{|GecComb|} gGECa gGECb args pSt
	= abort "Cannot make up function value for DataGec"	

mkGEC 			:: String						-> CGEC a a			| gGEC {|*|} a
mkGEC s	= gecEdit s 

applyGEC 		:: String (a -> b)				-> CGEC a b  		| gGEC{|*|} a & gGEC{|*|} b
applyGEC s fab = gecEdit s |@ fab |&| gecDisplay s

apply2GEC 		:: String (b -> c) (a -> b)		-> CGEC a c			| gGEC{|*|} a & gGEC{|*|} b & gGEC{|*|} c 
apply2GEC s gbc fab = gecEdit s |@ fab |&| gecDisplay s |@ gbc |&| gecDisplay s

applyGEC2 		:: String (a b -> c)			-> CGEC (a,b) c		| gGEC{|*|} a & gGEC{|*|} b & gGEC{|*|} c
applyGEC2 s fabc  
	= ((gecEdit s |<>| gecEdit s) |@ \(a,b) -> fabc a b) |&| gecDisplay s

selfGEC 		:: String (a -> a) 				-> CGEC a a			| gGEC{|*|} a 
selfGEC s faa   = %| (gecEdit s |@ faa)

mutualGEC 		:: String (b -> a)(a -> b) 		-> CGEC a a 		| gGEC{|*|} a & gGEC{|*|} b 
mutualGEC s  fba fab  = %|(gecEdit s |@ fab |&| gecEdit s |@ fba )

predGEC 		:: String (a -> Bool) 		-> CGEC a a 		| gGEC{|*|} a
predGEC s p  =  (\a -> (a,Hide a)) @| selfGEC s (\(a,Hide oa) -> if (p a) (a,Hide a) (oa,Hide oa)) |@ (\(a,Hide oa) -> a)
/*selfState_GECps :: (A..ps : a -> .(s -> .(*(PSt .ps) -> *(a,s,*(PSt .ps))))) !(!String,!a) s -> ((CGEC (a,(Mode s)) (a,(Mode s))),(a,(Mode s))) | gGEC{|*|} a & gGEC{|*|} s 
selfState_GECps faspst (sa,a) s
	= (%|(gecEdit sa |&| gecIO (\(a,Hide s) pst -> thd3 (faspst a s pst))),(a,Hide s))
*/