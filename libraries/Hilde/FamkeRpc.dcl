definition module FamkeRpc

from FamkeKernel import :: Famke(..), :: FamkePort(..), :: FamkeId(..)

:: RpcId a b :== FamkePort a b

:: RpcServer a b

rpc :: !(RpcId a b) a !*Famke -> (b, !*Famke) | TC a & TC b

rpcOpen :: !(RpcId .a .b) !*Famke -> (!RpcId .a .b, !*RpcServer .a .b, !*Famke)
rpcWait :: !*(RpcServer a b) !*Famke -> (a, !*(b -> *(*Famke -> *Famke)), !*RpcServer a b, !*Famke) | TC a & TC b
rpcClose :: !*(RpcServer .a .b) !*Famke -> *Famke
