definition module receiveraccess


//	Clean Object I/O library, version 1.2


import	StdReceiverDef, receiverhandle

// MW11 added [Id]
newReceiverStateHandle	:: !Id .ls !SelectState ![Id] !(ReceiverFunction  m   *(.ls,.ps)) -> ReceiverStateHandle .ps
newReceiverStateHandle2	:: !Id .ls !SelectState ![Id] !(Receiver2Function m r *(.ls,.ps)) -> ReceiverStateHandle .ps

newReceiverHandle		:: !Id     !SelectState ![Id] !(ReceiverFunction  m   *(.ls,.ps)) -> ReceiverHandle .ls .ps
newReceiverHandle2		:: !Id     !SelectState ![Id] !(Receiver2Function m r *(.ls,.ps)) -> ReceiverHandle .ls .ps
// .. MW11

onewaytotriple			:: !(ReceiverFunction  m   *(.ls,.pst)) m !(.ls,.pst) -> (.ls,[r],.pst)
twowaytotriple			:: !(Receiver2Function m r *(.ls,.pst)) m !(.ls,.pst) -> (.ls,[r],.pst)
