definition module FamkeKernel

import StdDynamic
from FamkeProcess import :: ProcessId(..)
from TcpIp import class TcpIp
from StdFile import class FileSystem

:: Famke

processId :: !*Famke -> (!ProcessId, !*Famke)

:: FamkePort a b
	= FamkeProcessServer
	| FamkeNameServer
	| FamkeAnyServer
	| FamkeServer !.(FamkeId a b)

:: FamkeId a b = {famkeIp :: !Int, famkePort :: !Int}
:: FamkeServer a b 
:: FamkeChannel a b 

famkeOpen :: !(FamkePort .a .b) !*Famke -> (!Bool, FamkePort .a .b, *FamkeServer .a .b, !*Famke)
famkeAccept :: !Bool !*(FamkeServer .a .b) !*Famke -> (!Bool, !*FamkeChannel .b .a, !*FamkeServer .a .b, !*Famke)
famkeClose :: !*(FamkeServer .a .b) !*Famke -> *Famke

famkeConnect :: !Bool !(FamkePort .a .b) !*Famke -> (!Bool, !*FamkeChannel .a .b, !*Famke)
famkeDisconnect :: !*(FamkeChannel .a .b) !*Famke -> *Famke

famkeSend :: a !*(FamkeChannel a .b) !*Famke -> (!Bool, !*FamkeChannel a .b, !*Famke) | TC a
famkeReceive :: !Bool !*(FamkeChannel .a b) !*Famke -> (!Bool, b, !*FamkeChannel .a b, !*Famke) | TC b

unsafeFamkeSendDynamic :: !Dynamic !*(FamkeChannel .a .b) !*Famke -> (!Bool, !*FamkeChannel .a .b, !*Famke)
unsafeFamkeReceiveDynamic :: !Bool !*(FamkeChannel .a .b) !*Famke -> (!Bool, !Dynamic, !*FamkeChannel .a .b, !*Famke)

StartKernel :: !ProcessId !.(*Famke -> *Famke) !*World -> *World

instance TcpIp Famke
instance FileSystem Famke
