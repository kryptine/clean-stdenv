implementation module StdControlReceiver


//	Clean Object I/O library, version 1.2


import	StdTuple
import	StdControlClass, StdReceiverAttribute, windowhandle
from	commondef			import Select, Cond
from	receiveraccess		import newReceiverHandle, newReceiverHandle2
from	oswindow			import OSNoWindowPtr


instance Controls (Receiver m) where
	controlToHandles :: !(Receiver m .ls (PSt .l)) !(PSt .l) -> (![ControlState .ls (PSt .l)],!PSt .l)
	controlToHandles (Receiver rid f atts) pState
		= (	[WElementHandleToControlState
				(WItemHandle 
				{	wItemId			= Just id
				,	wItemNr			= 0
				,	wItemKind		= IsOtherControl "Receiver"
				,	wItemShow		= False
				,	wItemSelect		= enabled select
// MW11 was				,	wItemInfo		= ReceiverInfo (newReceiverHandle id select f)
				,	wItemInfo		= ReceiverInfo (newReceiverHandle id select [] f)
				,	wItemAtts		= []
				,	wItems			= []
				,	wItemVirtual	= True
				,	wItemPos		= zero
				,	wItemSize		= zero
				,	wItemPtr		= OSNoWindowPtr
				,	wItemLayoutInfo	= LayoutFix
				})
			]
		  ,	pState
		  )
	where
		id		= RIdtoId rid
		select	= getSelectState atts
	
	getControlType :: (Receiver m .ls .ps) -> ControlType
	getControlType _
		= "Receiver"

instance Controls (Receiver2 m r) where
	controlToHandles :: !(Receiver2 m r .ls (PSt .l)) !(PSt .l) -> (![ControlState .ls (PSt .l)],!PSt .l)
	controlToHandles (Receiver2 r2id f atts) pState
		= (	[WElementHandleToControlState
				(WItemHandle 
				{	wItemId			= Just id
				,	wItemNr			= 0
				,	wItemKind		= IsOtherControl "Receiver2"
				,	wItemShow		= False
				,	wItemSelect		= enabled select
// MW11 was				,	wItemInfo		= ReceiverInfo (newReceiverHandle2 id select f)
				,	wItemInfo		= ReceiverInfo (newReceiverHandle2 id select [] f)
				,	wItemAtts		= []
				,	wItems			= []
				,	wItemVirtual	= True
				,	wItemPos		= zero
				,	wItemSize		= zero
				,	wItemPtr		= OSNoWindowPtr
				,	wItemLayoutInfo	= LayoutFix
				})
			]
		  ,	pState
		  )
	where
		id		= R2IdtoId r2id
		select	= getSelectState atts
	
	getControlType :: (Receiver2 m r .ls .ps) -> ControlType
	getControlType _
		= "Receiver2"

getSelectState :: ![ReceiverAttribute .ps] -> SelectState
getSelectState rAtts
	= getReceiverSelectStateAtt (snd (Select isReceiverSelectState (ReceiverSelectState Able) rAtts))
