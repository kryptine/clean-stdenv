definition module GecArrow


import StdGECExt


:: GecArr a b


(|>>>|) infixl 4 :: !(GecArr a b) !(GecArr b c) -> GecArr a c
(|&&&|) infix 5 :: !(GecArr a b) !(GecArr a c) -> GecArr a (b, c)
(|***|) infix 5 :: !(GecArr a b) !(GecArr c d) -> GecArr (a, c) (b, d)


(@|) infixl 6 :: (a -> b) !(GecArr b c) -> GecArr a c
(|@) infixl 6 :: !(GecArr a b) (b -> c) -> GecArr a c


gecFix :: !(GecArr a a) -> GecArr a a


gecEdit :: String -> GecArr a a | gGEC{|*|} a 
gecDisplay :: String -> GecArr a a | gGEC{|*|} a
