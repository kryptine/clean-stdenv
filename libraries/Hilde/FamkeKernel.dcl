definition module FamkeKernel

import StdDynamic
from FamkeProcess import :: ProcessId(..)

processId :: !*World -> (!ProcessId, !*World)

:: FamkePort a b
	= FamkeProcessServer
	| FamkeNameServer
	| FamkeAnyServer
	| FamkeServer !.(FamkeId a b)

:: FamkeId a b = {famkeIp :: !Int, famkePort :: !Int}
:: FamkeServer a b 
:: FamkeChannel a b 

famkeOpen :: !(FamkePort .a .b) !*World -> (!Bool, FamkePort .a .b, *FamkeServer .a .b, !*World)
famkeAccept :: !Bool !*(FamkeServer .a .b) !*World -> (!Bool, !*FamkeChannel .b .a, !*FamkeServer .a .b, !*World)
famkeClose :: !*(FamkeServer .a .b) !*World -> *World

famkeConnect :: !Bool !(FamkePort .a .b) !*World -> (!Bool, !*FamkeChannel .a .b, !*World)
famkeDisconnect :: !*(FamkeChannel .a .b) !*World -> *World

famkeSend :: a !*(FamkeChannel a .b) !*World -> (!Bool, !*FamkeChannel a .b, !*World) | TC a
famkeReceive :: !Bool !*(FamkeChannel .a b) !*World -> (!Bool, b, !*FamkeChannel .a b, !*World) | TC b

unsafeFamkeSendDynamic :: !Dynamic !*(FamkeChannel .a .b) !*World -> (!Bool, !*FamkeChannel .a .b, !*World)
unsafeFamkeReceiveDynamic :: !Bool !*(FamkeChannel .a .b) !*World -> (!Bool, !Dynamic, !*FamkeChannel .a .b, !*World)

StartKernel :: !ProcessId !.(*World -> *World) !*World -> *World
