implementation module FamkeRpc

import FamkeKernel
import StdBool, StdMisc

:: RpcServer a b
	:== FamkeServer a b

rpc :: !(RpcId a b) a !*Famke -> (b, !*Famke) | TC a & TC b
rpc id request famke
	# (ok, comm, famke) = famkeConnect True id famke
	| not ok = abort "rpc: clientConnect failed"
	# (ok, comm, famke) = famkeSend request comm famke
	| not ok = abort "rpc: famkeSend failed"
	# (ok, reply, comm, famke) = famkeReceive True comm famke
	| not ok = abort "rpc: famkeReceive failed"
	# famke = famkeDisconnect comm famke
	= (reply, famke)

rpcOpen :: !(RpcId .a .b) !*Famke -> (!RpcId .a .b, !*RpcServer .a .b, !*Famke)
rpcOpen id famke 
	# (ok, id, server, famke) = famkeOpen id famke
	| not ok = abort "rpcOpen: famkeOpen failed"
	= (id, server, famke)

rpcWait :: !*(RpcServer a b) !*Famke -> (a, !*(b -> *(*Famke -> *Famke)), !*RpcServer a b, !*Famke) | TC a & TC b
rpcWait server famke
	# (ok, comm, server, famke) = famkeAccept True server famke
	| not ok = abort "rpcWait: serverConnect failed"
	# (ok, request, comm, famke) = famkeReceive True comm famke
	| not ok = abort "rpcWait: famkeReceive failed"
	= (request, rpcReply comm, server, famke)
where
	rpcReply :: !*(FamkeChannel b .a) b !*Famke  -> *Famke | TC b
	rpcReply comm reply famke
		# (ok, comm, famke) = famkeSend reply comm famke
		| not ok = abort "rpcReply: famkeSend failed"
		= famkeDisconnect comm famke

rpcClose :: !*(RpcServer .a .b) !*Famke -> *Famke
rpcClose server famke = famkeClose server famke

rpcHandle :: !.(a -> *(*Famke -> *(b, *Famke))) !*(RpcServer a b) !*Famke -> (!*RpcServer a b, !*Famke) | TC a & TC b
rpcHandle handler server famke
	# (request, return, server, famke) = rpcWait server famke
	  (reply, famke) = handler request famke
	= (server, return reply famke) 
