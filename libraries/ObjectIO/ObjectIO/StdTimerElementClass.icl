implementation module StdTimerElementClass


//	Clean Object I/O library, version 1.2

//	Definition of the TimerElements class for timer elements.


import	StdList
import	StdTimerDef
import	commondef, iostate, timerhandle


class TimerElements t where
	timerElementToHandles	:: !(t .ls (PSt .l)) !(PSt .l)	-> (![TimerElementState .ls (PSt .l)],!PSt .l)
	getTimerElementType		::	(t .ls .pst)				-> TimerElementType

instance TimerElements (AddLS t) | TimerElements t where
	timerElementToHandles :: !(AddLS t .ls (PSt .l)) !(PSt .l)-> (![TimerElementState .ls (PSt .l)],!PSt .l) | TimerElements t
	timerElementToHandles {addLS,addDef} pState
		# (ts,pState)	= timerElementToHandles addDef pState
		= (	[TimerElementHandleToTimerElementState 
				(TimerExtendLSHandle { tExtendLS	= addLS
									 , tExtendItems	= map TimerElementStateToTimerElementHandle ts
									 }
				)
			]
		  ,	pState
		  )
	
	getTimerElementType :: (AddLS t .ls .pst) -> TimerElementType | TimerElements t
	getTimerElementType _ = ""

instance TimerElements (NewLS t) | TimerElements t where
	timerElementToHandles :: !(NewLS t .ls (PSt .l)) !(PSt .l)-> (![TimerElementState .ls (PSt .l)],!PSt .l) | TimerElements t
	timerElementToHandles {newLS,newDef} pState
		# (ts,pState)	= timerElementToHandles newDef pState
		= (	[TimerElementHandleToTimerElementState 
				(TimerChangeLSHandle { tChangeLS	= newLS
									 , tChangeItems	= map TimerElementStateToTimerElementHandle ts
									 }
				)
			]
		  ,	pState
		  )
	
	getTimerElementType :: (NewLS t .ls .pst) -> TimerElementType | TimerElements t
	getTimerElementType _ = ""

instance TimerElements (ListLS t) | TimerElements t where
	timerElementToHandles :: !(ListLS t .ls (PSt .l)) !(PSt .l)-> (![TimerElementState .ls (PSt .l)],!PSt .l) | TimerElements t
	timerElementToHandles (ListLS tDefs) pState
		# (tss,pState)	= StateMap timerElementToHandles tDefs pState
		= (	[TimerElementHandleToTimerElementState 
				(TimerListLSHandle (map TimerElementStateToTimerElementHandle (flatten tss)))
			]
		  ,	pState
		  )
	
	getTimerElementType :: (ListLS t .ls .pst) -> TimerElementType | TimerElements t
	getTimerElementType _ = ""

instance TimerElements NilLS where
	timerElementToHandles :: !(NilLS .ls (PSt .l)) !(PSt .l)-> (![TimerElementState .ls (PSt .l)],!PSt .l)
	timerElementToHandles NilLS pState
		= ([TimerElementHandleToTimerElementState (TimerListLSHandle [])],pState)
	
	getTimerElementType :: (NilLS .ls .pst) -> TimerElementType
	getTimerElementType _ = ""

instance TimerElements ((:+:) t1 t2) | TimerElements t1 & TimerElements t2 where
	timerElementToHandles :: !((:+:) t1 t2 .ls (PSt .l)) !(PSt .l)-> (![TimerElementState .ls (PSt .l)],!PSt .l)
						  |  TimerElements t1 & TimerElements t2
	timerElementToHandles (t1:+:t2) pState
		# (ts1,pState)	= timerElementToHandles t1 pState
		# (ts2,pState)	= timerElementToHandles t2 pState
		= (ts1 ++ ts2,pState)
	
	getTimerElementType :: ((:+:) t1 t2 .ls .pst) -> TimerElementType | TimerElements t1 & TimerElements t2
	getTimerElementType _ = ""
