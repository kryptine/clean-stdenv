implementation module GecArrow

import StdGECExt

:: GecArr a b = GecArr !.(A. .ps: a (GecSet b ps) (GecGet a ps) *(PSt ps) -> *(b, GecSet a ps, GecGet b ps, *PSt ps))

:: GecSet a ps :== IncludeUpdate a *(PSt ps) -> *PSt ps
:: GecGet a ps :== *(PSt ps) -> *(a, *PSt ps)

gecCreate :: !(GecArr a b) a !*(PSt .ps) -> (b, !*PSt .ps)
gecCreate (GecArr g) a pst = (b, pst1)
where
        (b, seta, getb, pst1) = g a setb geta pst

        geta pst = (a, pst)

        setb _ _ pst = pst

gecEdit :: String -> GecArr a a | gGEC{|*|} a 
gecEdit title = GecArr k
where
        k a seta _ pst = (a, gecSetValue, gecGetValue, pst1)
        where
                ({gecGetValue, gecSetValue}, pst1) = createNGEC title Interactive a (\r -> seta (includeUpdate r)) pst

gecDisplay :: String -> GecArr a a | gGEC{|*|} a 
gecDisplay title = GecArr k
where
        k a seta _ pst = (a, gecSetValue, gecGetValue, pst1)
        where
                ({gecGetValue, gecSetValue}, pst1) = createNGEC title OutputOnly a (\r -> seta (includeUpdate r)) pst

instance Arrow GecArr
where
	arr f = GecArr k
	where
	        k a setb geta pst = (f a, seta, getb, pst)
	        where
	                getb pst = (f a, pst1)
	                where
	                        (a, pst1) = geta pst
	                
	                seta u a pst = setb u (f a) pst

	(>>>) (GecArr l) (GecArr r) = GecArr k
	where 
	        k a setc geta pst = (c, seta, getc, pst2)
	        where
	                (c, setb, getc, pst1) = r b setc getb pst
	                (b, seta, getb, pst2) = l a setb geta pst1

	first (GecArr g) = GecArr k
	where
	        k (a, c) setbc getac pst = ((b, c), setac, getbc, pst1)
	        where
	                (b, seta, getb, pst1) = g a setb geta pst
	                
	                geta pst = (a, pst1)
	                where
	                        ((a, _), pst1) = getac pst
	
	                getbc pst = ((b, c), pst2)
	                where
	                        (b, pst1) = getb pst
	                        ((_, c), pst2) = getac pst1
	                
	                setb u b pst = setbc u (b, c) pst1
	                where
	                        ((_, c), pst1) = getac pst
	
	                setac u (a, c) pst = setbc u (b, c) (seta u a pst1)
	                where
	                        (b, pst1) = getb pst

instance ArrowLoop GecArr
where
	loop (GecArr g) = GecArr k
	where 
	        k a setb geta pst = (b, seta, getb, pst1)
	        where
	                ((b, c), setac, getbc, pst1) = g (a, c) setbc getac pst
	                
	                getb pst = (b, pst1)
	                where
	                        ((b, _), pst1) = getbc pst
	
	                getac pst = ((a, c), pst2)
	                where
	                        (a, pst1) = geta pst
	                        ((_, c), pst2) = getbc pst1
	
	                seta u a pst = setac u (a, c) pst1
	                where
	                        ((_, c), pst1) = getbc pst
	
	                setbc u (b, c) pst = setb u b (setac NoUpdate (a, c) pst1)
	                where
	                        (a, pst1) = geta pst
	                


(@>>) infixl 6 :: (a -> b) !(GecArr b c) -> GecArr a c
(@>>) f gec = gecArr f >>> gec
 
(<<@) infixl 6 :: !(GecArr a b) (b -> c) -> GecArr a c
(<<@) gec f = gec >>> gecArr f

gecFix :: !(GecArr a a) -> GecArr a a
gecFix (GecArr g) = GecArr k
where 
        k a seta geta pst = (a`, seta`, geta`, pst1)
        where
                (a`, seta`, geta`, pst1) = g a seta`` geta pst
                
                seta`` r a pst = seta r a (seta` NoUpdate a pst)

includeUpdate :: !UpdateReason -> *IncludeUpdate
includeUpdate Changed = YesUpdate
includeUpdate _ = NoUpdate
