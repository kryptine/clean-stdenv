definition module GecArrow


import StdGECExt


:: GecArr a b


// Initialize GecArr circuit


gecCreate :: !(GecArr a b) a !*(PSt .ps) -> (b, !*PSt .ps)


// Lift visual editors to GecArr's


gecEdit :: String -> GecArr a a | gGEC{|*|} a 
gecDisplay :: String -> GecArr a a | gGEC{|*|} a


// Arrow instance for GecArr


gecArr :: (a -> b) -> GecArr a b
(|>>>|) infixr 1 :: !(GecArr a b) !(GecArr b c) -> GecArr a c
gecFirst :: !(GecArr a b) -> GecArr (a, c) (b, c)


// ArrowLoop instance for GecArr


gecLoop :: !(GecArr (a, c) (b, c)) -> GecArr a b


// Derived arrow members for GecArr


gecSecond :: !(GecArr a b) -> GecArr (c, a) (c, b)
gecReturn :: GecArr a a


(|<<<|) infixr 1 :: !(GecArr b c) !(GecArr a b) -> GecArr a c
(|***|) infixr 3 :: !(GecArr a b) !(GecArr c d) -> GecArr (a, c) (b, d)
(|&&&|) infixr 3 :: !(GecArr a b) !(GecArr a c) -> GecArr a (b, c)


// Other GecArr combinators


(@|) infixl 6 :: (a -> b) !(GecArr b c) -> GecArr a c
(|@) infixl 6 :: !(GecArr a b) (b -> c) -> GecArr a c


gecFix :: !(GecArr a a) -> GecArr a a