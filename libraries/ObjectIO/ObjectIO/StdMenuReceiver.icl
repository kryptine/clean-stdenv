implementation module StdMenuReceiver


//	Clean Object I/O library, version 1.2.1


import	StdTuple, StdList
import	StdReceiverAttribute, StdMenuElementClass, menuhandle
from	commondef			import cselect, Cond
from	receiveraccess		import newReceiverHandle, newReceiverHandle2


instance MenuElements (Receiver m) where
	menuElementToHandles :: !(Receiver m .ls (PSt .l)) !(PSt .l) -> (![MenuElementState .ls (PSt .l)],!PSt .l)
	menuElementToHandles (Receiver rid f atts) pState
		= (	[menuElementHandleToMenuElementState
// MW11 was				(MenuReceiverHandle {	mReceiverHandle	= newReceiverHandle id (getSelectState atts) f
				(MenuReceiverHandle {	mReceiverHandle	= newReceiverHandle id (getSelectState atts) (getConnectedIds atts) f
									,	mReceiverAtts	= [MenuId id:map ReceiverAttToMenuAtt atts]
									}
				)
			]
		  ,	pState
		  )
	where
		id	= rIdtoId rid
	
	getMenuElementType :: (Receiver m .ls .ps) -> MenuElementType
	getMenuElementType _ = "Receiver"

instance MenuElements (Receiver2 m r) where
	menuElementToHandles :: !(Receiver2 m r .ls  (PSt .l)) !(PSt .l) -> (![MenuElementState .ls (PSt .l)],!PSt .l)
	menuElementToHandles (Receiver2 rid f atts) pState
		= (	[menuElementHandleToMenuElementState
// MW11 was				(MenuReceiverHandle {	mReceiverHandle	= newReceiverHandle2 id (getSelectState atts) f
				(MenuReceiverHandle {	mReceiverHandle	= newReceiverHandle2 id (getSelectState atts) (getConnectedIds atts) f
									,	mReceiverAtts	= [MenuId id:map ReceiverAttToMenuAtt atts]
									}
				)
			]
		  ,	pState
		  )
	where
		id	= r2IdtoId rid
	
	getMenuElementType :: (Receiver2 m r .ls .ps) -> MenuElementType
	getMenuElementType _ = "Receiver2"

getSelectState :: ![ReceiverAttribute .ps] -> SelectState
getSelectState rAtts
	= getReceiverSelectStateAtt (snd (cselect isReceiverSelectState (ReceiverSelectState Able) rAtts))

// MW11..
getConnectedIds :: ![ReceiverAttribute .ps] -> [Id]
getConnectedIds rAtts
	= getReceiverConnectedReceivers (snd (cselect isReceiverConnectedReceivers (ReceiverConnectedReceivers []) rAtts))
// .. MW11

ReceiverAttToMenuAtt :: !(ReceiverAttribute .ps) -> MenuAttribute .ps
ReceiverAttToMenuAtt (ReceiverSelectState s)
	= MenuSelectState s
