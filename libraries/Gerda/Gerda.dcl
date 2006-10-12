definition module Gerda

import StdMaybe, StdGeneric

:: Gerda

openGerda :: !String !*World -> (!*Gerda, !*World)
closeGerda :: !*Gerda !*World -> *World
writeGerda :: !String !a !*Gerda -> *Gerda | gerda{|*|} a
readGerda :: !String !*Gerda -> (!Maybe a, !*Gerda) | gerda{|*|} a

:: GerdaFunctions a

generic gerda a :: GerdaFunctions a

derive gerda OBJECT, EITHER, CONS, FIELD, PAIR, UNIT
derive gerda Int, Real, Char, Bool, Maybe, String, []

derive bimap GerdaFunctions
