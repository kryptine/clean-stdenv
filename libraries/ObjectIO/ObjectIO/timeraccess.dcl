definition module timeraccess


//	Clean Object I/O library, version 1.2


import	devicesystemstate, timerhandle
from	id				import IdTable
from	receivertable	import ReceiverTable
from	timertable		import TimerTable



/*	bindTimerElementIds binds all unbound R(2)Ids and Ids that can be located in the list of TimerElementStates.
	The Boolean result is True only if no bound identification was found, otherwise it is False.
*/
bindTimerElementIds		:: !SystemId !Id ![TimerElementHandle .ls .ps] !ReceiverTable !IdTable
							   -> (!Bool,![TimerElementHandle .ls .ps],!ReceiverTable,!IdTable)

/*	unbindTimerElementIds unbinds all bound R(2)Ids and Ids that can be located in the list of TimerElementStates.
*/
unbindTimerElementIds	:: !SystemId ![TimerElementHandle .ls .ps] !(!TimerTable,!ReceiverTable,!IdTable)
																 -> (!TimerTable,!ReceiverTable,!IdTable)

identifyTimerStateHandle		:: !Id !(TimerStateHandle .ps) -> (!Bool,!TimerStateHandle .ps)
