definition module receiveraccess


//	Clean Object I/O library, version 1.2


import	StdReceiverDef, receiverhandle


newReceiverStateHandle	:: !Id .ls !SelectState !(ReceiverFunction  m   *(.ls,.pst)) -> ReceiverStateHandle .pst
newReceiverStateHandle2	:: !Id .ls !SelectState !(Receiver2Function m r *(.ls,.pst)) -> ReceiverStateHandle .pst

newReceiverHandle		:: !Id     !SelectState !(ReceiverFunction  m   *(.ls,.pst)) -> ReceiverHandle .ls .pst
newReceiverHandle2		:: !Id     !SelectState !(Receiver2Function m r *(.ls,.pst)) -> ReceiverHandle .ls .pst

onewaytotriple			:: !(ReceiverFunction  m   *(.ls,.pst)) m !(.ls,.pst) -> (.ls,[r],.pst)
twowaytotriple			:: !(Receiver2Function m r *(.ls,.pst)) m !(.ls,.pst) -> (.ls,[r],.pst)
