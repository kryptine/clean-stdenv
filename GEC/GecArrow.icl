implementation module GecArrow

import StdGECExt

:: GecArr a b = GecArr !.(A. .ps: a (GecSet b ps) (GecGet a ps) *(PSt ps) -> *(b, GecSet a ps, GecGet b ps, *PSt ps))

:: GecGet a ps :== *(PSt ps) -> *(a, *PSt ps)
:: GecSet a ps :== IncludeUpdate a *(PSt ps) -> *PSt ps


(|>>>|) infixl 4 :: !(GecArr a b) !(GecArr b c) -> GecArr a c
(|>>>|) (GecArr l) (GecArr r) = GecArr k
where 
	k a setc geta pst = (c, seta, getc, pst2)
	where
		(c, setb, getc, pst1) = r b setc getb pst
		(b, seta, getb, pst2) = l a setb geta pst1


(|&&&|) infix 5 :: !(GecArr a b) !(GecArr a c) -> GecArr a (b, c)
(|&&&|) (GecArr l) (GecArr r) = GecArr k
where 
	k a setbc geta pst = ((b, c), seta, getbc, pst2)
	where
		(c, setar, getc, pst1) = r a setc geta pst
		(b, setal, getb, pst2) = l a setb geta pst1
       
		getbc pst = ((b, c), pst``) 
		where
			(b, pst`) = getb pst
			(c, pst``) = getc pst`

		setc u c pst = setbc u (b, c) pst`
		where
			(b, pst`) = getb pst

		setb u b pst = setbc u (b, c) pst`
		where
			(c, pst`) = getc pst

		seta u a pst = setal u a (setar u a pst) 


(|***|) infix 5 :: !(GecArr a b) !(GecArr c d) -> GecArr (a, c) (b, d)
(|***|) (GecArr l) (GecArr r) = GecArr k
where 
	k (a, c) setbd getac pst = ((b, d), setac, getbd, pst2)
	where
		(d, setc, getd, pst1) = r c setd getc pst
		(b, seta, getb, pst2) = l a setb geta pst1

		getc pst = (c, pst`)
		where
			((a, c), pst`) = getac pst

		geta pst = (a, pst`)
		where
			((a, c), pst`) = getac pst

		getbd pst = ((b, d), pst``)
		where
			(b, pst`) = getb pst
			(d, pst``) = getd pst`

		setd u d pst = setbd u (b, d) pst`
		where
			(b, pst`) = getb pst

		setb u b pst = setbd u (b, d) pst`
		where
			(d, pst`) = getd pst 

		setac u (a, c) pst = setc u c (seta u a pst)


(@|) infixl 6 :: (a -> b) !(GecArr b c) -> GecArr a c
(@|) f (GecArr g) = GecArr k
where 
	k a setc geta pst = (c, seta, getc, pst1)
	where
		(c, setb, getc, pst1) = g (f a) setc getb pst
       
		getb pst = (f a, pst`)
		where
			(a, pst`) = geta pst
                        
		seta r a pst = setb r (f a) pst
 
(|@) infixl 6 :: !(GecArr a b) (b -> c) -> GecArr a c
(|@) (GecArr g) f = GecArr k
where
	k a setc geta pst = (f b, seta, getc, pst1)
	where
		(b, seta, getb, pst1) = g a setb geta pst

		getc pst = (f b, pst`)
		where
			(b, pst`) = getb pst

		setb r b pst = setc r (f b) pst


gecFix :: !(GecArr a a) -> GecArr a a
gecFix (GecArr g) = GecArr k
where 
	k a seta geta pst = (a`, seta`, geta`, pst1)
	where
		(a`, seta`, geta`, pst1) = g a (\r a pst -> seta r a (seta` NoUpdate a pst)) geta pst


gecEdit :: String -> GecArr a a | gGEC{|*|} a 
gecEdit title = GecArr k
where
	k a seta _ pst = (a, gecSetValue, gecGetValue, pst1)
	where
		({gecGetValue, gecSetValue}, pst1) = createNGEC title Interactive a (\r -> seta (maybeUpdate r)) pst


gecDisplay :: String -> GecArr a a | gGEC{|*|} a 
gecDisplay title = GecArr k
where
	k a seta _ pst = (a, gecSetValue, gecGetValue, pst1)
	where
		({gecGetValue, gecSetValue}, pst1) = createNGEC title OutputOnly a (\r -> seta (maybeUpdate r)) pst


maybeUpdate :: !UpdateReason -> *IncludeUpdate
maybeUpdate Changed = YesUpdate
maybeUpdate _       = NoUpdate
