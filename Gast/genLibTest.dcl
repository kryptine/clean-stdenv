definition module genLibTest

/*
Pieter Koopman 2002
Nijmegen University, The Netherlands

GAST: A Generic Automatic Software Test-system
*/

import StdGeneric

import StdClass
instance + String

(@) infixl 2 :: (a->b)  a -> b
(@!)infixl 2 :: (a->b) !a -> b

generic genShow a :: String Bool a [String] -> [String]
generic gEq a  :: a a -> Bool
generic gLess a  :: a a -> Bool

derive genShow	Int, Char, Bool, Real, String, UNIT, PAIR, EITHER, CONS, FIELD, OBJECT, [], (,), (,,), (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,), (,,,,,,,,), (,,,,,,,,,), (->), {}, {!}
derive gEq      Int, Char, Bool, Real, String, UNIT, PAIR, EITHER, CONS, FIELD, OBJECT, [], (,), (,,), (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,), (,,,,,,,,), (,,,,,,,,,) 
derive gLess    Int, Char, Bool, Real, String, UNIT, PAIR, EITHER, CONS, FIELD, OBJECT, [], (,), (,,), (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,), (,,,,,,,,), (,,,,,,,,,) 

show  :: !a -> [String] | genShow{|*|} a
show1 :: !a ->  String  | genShow{|*|} a

(===) infix 4 :: !a !a -> Bool | gEq{|*|} a
(=/=) infix 4 :: !a !a -> Bool | gEq{|*|} a

(-<-) infix 4 :: !a !a -> Bool | gLess{|*|} a
(->-) infix 4 :: !a !a -> Bool | gLess{|*|} a
(-<=) infix 4 :: !a !a -> Bool | gLess{|*|} a
(=>-) infix 4 :: !a !a -> Bool | gLess{|*|} a

