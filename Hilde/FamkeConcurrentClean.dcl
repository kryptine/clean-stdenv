definition module FamkeConcurrentClean

import StdGeneric
from FamkeKernel import :: Famke, :: FamkeChannel

:: ProcId

``P`` :: !(*Famke -> *(a, *Famke)) !*Famke -> (a, !*Famke) | SendGraph{|*|}, ReceiveGraph{|*|}, TC a

generic SendGraph a :: !a !*(FamkeChannel String String) !*Famke -> *(!*FamkeChannel String String, !*Famke)
generic ReceiveGraph b :: !*(FamkeChannel String String) !*Famke -> *(!b, *FamkeChannel String String, *Famke)

derive SendGraph UNIT, PAIR, EITHER, CONS, FIELD, OBJECT, String, Char, Int, Real, Bool, Dynamic
derive ReceiveGraph UNIT, PAIR, EITHER, CONS, FIELD, OBJECT, String, Char, Int, Real, Bool, Dynamic
