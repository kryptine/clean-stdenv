definition module GenEq

import StdGeneric

generic gEq a  :: a a -> Bool

// base cases
derive gEq Int, Char, Bool, Real, UNIT, PAIR, EITHER, CONS, FIELD, {}, {!} 

// standard types
derive gEq [], (,), (,,), (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)

(===) infix 4 :: a a -> Bool | gEq{|*|} a
(=!=) infix 4 :: a a -> Bool | gEq{|*|} a
