implementation module StdTimerReceiver


//	Clean Object I/O library, version 1.2.1


import	StdTuple, StdList
import	commondef, id, receiveraccess, receiverdefaccess, StdReceiverAttribute, StdTimerElementClass, timerhandle


instance TimerElements (Receiver m) where
	timerElementToHandles :: !(Receiver m .ls (PSt .l)) !(PSt .l) -> (![TimerElementState .ls (PSt .l)],!PSt .l)
	timerElementToHandles (Receiver rid f atts) pState
		= (	[TimerElementHandleToTimerElementState
// MW11 was				(TimerReceiverHandle {	tReceiverHandle	= newReceiverHandle id (getSelectState atts) f
				(TimerReceiverHandle {	tReceiverHandle	= newReceiverHandle id (getSelectState atts) (getConnectedIds atts) f
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
	timerElementToHandles :: !(Receiver2 m r .ls (PSt .l)) !(PSt .l) -> (![TimerElementState .ls (PSt .l)],!PSt .l)
	timerElementToHandles (Receiver2 rid f atts) pState
		= (	[TimerElementHandleToTimerElementState
// MW11 was				(TimerReceiverHandle {	tReceiverHandle	= newReceiverHandle2 id (getSelectState atts) f
				(TimerReceiverHandle {	tReceiverHandle	= newReceiverHandle2 id (getSelectState atts) (getConnectedIds atts) f
									 ,	tReceiverAtts	= [TimerId id:map ReceiverAttToTimerAtt atts]
									 })
			]
		  ,	pState
		  )
	where
		id	= R2IdtoId rid
	
	getTimerElementType :: (Receiver2 m r .ls .ps) -> TimerElementType
	getTimerElementType _ = "Receiver2"

/* MW11
getSelectState :: ![ReceiverAttribute .ps] -> SelectState
getSelectState rAtts
	= getReceiverSelectStateAtt (snd (Select isReceiverSelectState (ReceiverSelectState Able) rAtts))
*/

ReceiverAttToTimerAtt :: !(ReceiverAttribute .ps) -> TimerAttribute .ps
ReceiverAttToTimerAtt (ReceiverSelectState s)
	= TimerSelectState s

// MW11..
getSelectState :: ![ReceiverAttribute .ps] -> SelectState
getSelectState rAtts
	= getReceiverSelectStateAtt (snd (Select isReceiverSelectState (ReceiverSelectState Able) rAtts))

getConnectedIds :: ![ReceiverAttribute .ps] -> [Id]
getConnectedIds rAtts
	= getReceiverConnectedReceivers (snd (Select isReceiverConnectedReceivers (ReceiverConnectedReceivers []) rAtts))
// .. MW11
