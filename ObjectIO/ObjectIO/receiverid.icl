implementation module receiverid


//	Clean Object I/O library, version 1.2


import	StdTuple
import	iostate

bindRId :: !Id !SelectState !Id !Device !(IOSt .l .p) -> IOSt .l .p
bindRId rid select deviceid device ioState
	# (pid,ioState)		= IOStGetIOId ioState
	# (rt,ioState)		= IOStGetReceiverTable ioState
	  rl				= {	rlIOId		= pid
	  					  ,	rlDevice	= device
	  					  ,	rlParentId	= deviceid
	  					  ,	rlReceiverId= rid
	  					  }
	  (_,rt)			= addReceiverToReceiverTable {rteLoc=rl,rteSelectState=select,rteASMCount=0} rt
	# ioState			= IOStSetReceiverTable rt ioState
	= ioState

unbindRId :: !Id !(IOSt .l .p) -> IOSt .l .p
unbindRId rid ioState
	# (rt,ioState)		= IOStGetReceiverTable ioState
	  (_,rt)			= removeReceiverFromReceiverTable rid rt
	# ioState			= IOStSetReceiverTable rt ioState
	= ioState

unbindRIds :: ![Id] !(IOSt .l .p) -> IOSt .l .p
unbindRIds ids ioState
	# (rt,ioState)		= IOStGetReceiverTable ioState
	  rt				= unbindRIds` ids rt
	# ioState			= IOStSetReceiverTable rt ioState
	= ioState
where
	unbindRIds` :: ![Id] !ReceiverTable -> ReceiverTable
	unbindRIds` [rid:rids] rt	= unbindRIds` rids (snd (removeReceiverFromReceiverTable rid rt))
	unbindRIds` _          rt	= rt
