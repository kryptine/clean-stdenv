implementation module StdTimerReceiver


//	Clean Object I/O library, version 1.2


import	StdTuple, StdList
import	commondef, id, receiveraccess, receiverdefaccess, StdReceiverAttribute, StdTimerElementClass, timerhandle


instance TimerElements (Receiver m) where
	timerElementToHandles :: !(Receiver m .ls (PSt .l .p)) !(PSt .l .p) -> (![TimerElementState .ls (PSt .l .p)],!PSt .l .p)
	timerElementToHandles (Receiver rid f atts) pState
		= (	[TimerElementHandleToTimerElementState
				(TimerReceiverHandle {	tReceiverHandle	= newReceiverHandle id (getSelectState atts) f
									 ,	tReceiverAtts	= [TimerId id:map ReceiverAttToTimerAtt atts]
									 })
			]
		  ,	pState
		  )
	where
		id	= RIdtoId rid
	
	getTimerElementType :: (Receiver m .ls .ps) -> TimerElementType
	getTimerElementType _ = "Receiver"

instance TimerElements (Receiver2 m r) where
	timerElementToHandles :: !(Receiver2 m r .ls (PSt .l .p)) !(PSt .l .p) -> (![TimerElementState .ls (PSt .l .p)],!PSt .l .p)
	timerElementToHandles (Receiver2 rid f atts) pState
		= (	[TimerElementHandleToTimerElementState
				(TimerReceiverHandle {	tReceiverHandle	= newReceiverHandle2 id (getSelectState atts) f
									 ,	tReceiverAtts	= [TimerId id:map ReceiverAttToTimerAtt atts]
									 })
			]
		  ,	pState
		  )
	where
		id	= R2IdtoId rid
	
	getTimerElementType :: (Receiver2 m r .ls .ps) -> TimerElementType
	getTimerElementType _ = "Receiver2"

getSelectState :: ![ReceiverAttribute .ps] -> SelectState
getSelectState rAtts
	= getReceiverSelectStateAtt (snd (Select isReceiverSelectState (ReceiverSelectState Able) rAtts))

ReceiverAttToTimerAtt :: !(ReceiverAttribute .ps) -> TimerAttribute .ps
ReceiverAttToTimerAtt (ReceiverSelectState s)
	= TimerSelectState s
