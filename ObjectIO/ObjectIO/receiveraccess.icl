implementation module receiveraccess


//	Clean Object I/O library, version 1.2


import	id, receiverdefaccess, receiverhandle


newReceiverStateHandle :: !Id .ls !SelectState !(ReceiverFunction m *(.ls,.pst)) -> ReceiverStateHandle .pst
newReceiverStateHandle id localState select f
	= {	rState	= localState
	  ,	rHandle	= newReceiverHandle id select f	
	  }

newReceiverHandle :: !Id !SelectState !(ReceiverFunction m *(.ls,.pst)) -> ReceiverHandle .ls .pst
newReceiverHandle id select f
	= {	rId		= id
	  ,	rASMQ	= []
	  ,	rSelect	= select
	  ,	rOneWay	= True
	  ,	rFun	= onewaytotriple f
	  }

onewaytotriple :: !(ReceiverFunction m *(.ls,.pst)) m !(.ls,.pst) -> (.ls,[r],.pst)
onewaytotriple f m (ls,ps)
	# (ls,ps)	= f m (ls,ps)
	= (ls,[],ps)

newReceiverStateHandle2 :: !Id .ls !SelectState !(Receiver2Function m r *(.ls,.pst)) -> ReceiverStateHandle .pst
newReceiverStateHandle2 id localState select f
	= {	rState	= localState
	  ,	rHandle	= newReceiverHandle2 id select f
	  }

newReceiverHandle2 :: !Id !SelectState !(Receiver2Function m r *(.ls,.pst)) -> ReceiverHandle .ls .pst
newReceiverHandle2 id select f
	= {	rId		= id
	  ,	rASMQ	= []
	  ,	rSelect	= select
	  ,	rOneWay	= False
	  ,	rFun	= twowaytotriple f
	  }

twowaytotriple :: !(Receiver2Function m r *(.ls,.pst)) m !(.ls,.pst) -> (.ls,[r],.pst)
twowaytotriple f m (ls,ps)
	# (r, (ls,ps))	= f m (ls,ps)
	= (ls,[r],ps)
