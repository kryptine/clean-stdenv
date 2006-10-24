definition module Gerda

import StdMisc, StdMaybe, StdGeneric

:: Gerda

openGerda :: !String !*World -> (!*Gerda, !*World)
closeGerda :: !*Gerda !*World -> *World
writeGerda :: !String !a !*Gerda -> *Gerda | gerda{|*|} a
readGerda :: !String !*Gerda -> (!Maybe a, !*Gerda) | gerda{|*|} a

:: Binary252 = {binary252 :: !.String}
:: CompactList a = CompactList a .(Maybe (CompactList a))
:: GerdaObject a = {gerdaValue :: !a, 
					gerdaWrite :: a -> *Gerda -> *Gerda,
					gerdaRead :: *Gerda -> *(a, *Gerda)}

gerdaObject x :== {gerdaValue = x, gerdaWrite = undef, gerdaRead = undef}

:: GerdaFunctions a

generic gerda a :: GerdaFunctions a

derive gerda OBJECT, EITHER, CONS, FIELD, PAIR, UNIT
derive gerda Int, Real, Char, Bool, Maybe, Binary252
derive gerda CompactList, String, [], {}, {!}, GerdaObject

derive bimap GerdaFunctions
