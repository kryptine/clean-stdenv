implementation module StdMenuReceiver


//	Clean Object I/O library, version 1.2


import	StdTuple, StdList
import	StdReceiverAttribute, StdMenuElementClass, menuhandle
from	commondef			import Select, Cond
from	receiveraccess		import newReceiverHandle, newReceiverHandle2


instance MenuElements (Receiver m) where
	menuElementToHandles :: !(Receiver m .ls (PSt .l .p)) !(PSt .l .p) -> (![MenuElementState .ls (PSt .l .p)],!PSt .l .p)
	menuElementToHandles (Receiver rid f atts) pState
		= (	[MenuElementHandleToMenuElementState
// MW11 was				(MenuReceiverHandle {	mReceiverHandle	= newReceiverHandle id (getSelectState atts) f
				(MenuReceiverHandle {	mReceiverHandle	= newReceiverHandle id (getSelectState atts) (getConnectedIds atts) f
									,	mReceiverAtts	= [MenuId id:map ReceiverAttToMenuAtt atts]
									}
				)
			]
		  ,	pState
		  )
	where
		id	= RIdtoId rid
	
	getMenuElementType :: (Receiver m .ls .ps) -> MenuElementType
	getMenuElementType _ = "Receiver"

instance MenuElements (Receiver2 m r) where
	menuElementToHandles :: !(Receiver2 m r .ls  (PSt .l .p)) !(PSt .l .p) -> (![MenuElementState .ls (PSt .l .p)],!PSt .l .p)
	menuElementToHandles (Receiver2 rid f atts) pState
		= (	[MenuElementHandleToMenuElementState
// MW11 was				(MenuReceiverHandle {	mReceiverHandle	= newReceiverHandle2 id (getSelectState atts) f
				(MenuReceiverHandle {	mReceiverHandle	= newReceiverHandle2 id (getSelectState atts) (getConnectedIds atts) f
									,	mReceiverAtts	= [MenuId id:map ReceiverAttToMenuAtt atts]
									}
				)
			]
		  ,	pState
		  )
	where
		id	= R2IdtoId rid
	
	getMenuElementType :: (Receiver2 m r .ls .ps) -> MenuElementType
	getMenuElementType _ = "Receiver2"

getSelectState :: ![ReceiverAttribute .ps] -> SelectState
getSelectState rAtts
	= getReceiverSelectStateAtt (snd (Select isReceiverSelectState (ReceiverSelectState Able) rAtts))

// MW11..
getConnectedIds :: ![ReceiverAttribute .ps] -> [Id]
getConnectedIds rAtts
	= getReceiverConnectedReceivers (snd (Select isReceiverConnectedReceivers (ReceiverConnectedReceivers []) rAtts))
// .. MW11

ReceiverAttToMenuAtt :: !(ReceiverAttribute .ps) -> MenuAttribute .ps
ReceiverAttToMenuAtt (ReceiverSelectState s)
	= MenuSelectState s
