definition module GenLexOrd

import StdGeneric, GenEq

:: LexOrd = LT |EQ | GT
derive gEq LexOrd

generic gLexOrd a b :: a b -> LexOrd

// base cases
derive gLexOrd Char, Bool, Int, Real, UNIT, PAIR, EITHER, FIELD, CONS, [], {}, {!}

// standard types
derive gLexOrd (,), (,,),  (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)

(=?=) infix 4 :: a a -> LexOrd | gLexOrd{|*|} a
