implementation module receiveraccess


//	Clean Object I/O library, version 1.2


import	id, receiverdefaccess, receiverhandle


// MW11 added connectedIds
newReceiverStateHandle :: !Id .ls !SelectState ![Id] !(ReceiverFunction m *(.ls,.ps)) -> ReceiverStateHandle .ps
newReceiverStateHandle id localState select connectedIds f
	= {	rState	= localState
	  ,	rHandle	= newReceiverHandle id select connectedIds f	
	  }

// MW11 added rInetInfo,rConnected
newReceiverHandle :: !Id !SelectState ![Id] !(ReceiverFunction m *(.ls,.ps)) -> ReceiverHandle .ls .ps
newReceiverHandle id select connectedIds f
	= {	rId			= id
	  ,	rASMQ		= []
	  ,	rSelect		= select
	  ,	rOneWay		= True
	  ,	rFun		= onewaytotriple f
	  , rInetInfo	= Nothing
  	  , rConnected	= connectedIds
	  }

onewaytotriple :: !(ReceiverFunction m *(.ls,.pst)) m !(.ls,.pst) -> (.ls,[r],.pst)
onewaytotriple f m (ls,ps)
	# (ls,ps)	= f m (ls,ps)
	= (ls,[],ps)

// MW11 added connectedIds
newReceiverStateHandle2 :: !Id .ls !SelectState ![Id] !(Receiver2Function m r *(.ls,.ps)) -> ReceiverStateHandle .ps
newReceiverStateHandle2 id localState select connectedIds f
	= {	rState	= localState
	  ,	rHandle	= newReceiverHandle2 id select connectedIds f
	  }

// MW11 added rInetInfo,rConnected
newReceiverHandle2 :: !Id !SelectState ![Id] !(Receiver2Function m r *(.ls,.ps)) -> ReceiverHandle .ls .ps
newReceiverHandle2 id select connectedIds f
	= {	rId			= id
	  ,	rASMQ		= []
	  ,	rSelect		= select
	  ,	rOneWay		= False
	  ,	rFun		= twowaytotriple f
	  , rInetInfo	= Nothing
  	  , rConnected	= connectedIds
	  }

twowaytotriple :: !(Receiver2Function m r *(.ls,.pst)) m !(.ls,.pst) -> (.ls,[r],.pst)
twowaytotriple f m (ls,ps)
	# (r, (ls,ps))	= f m (ls,ps)
	= (ls,[r],ps)
