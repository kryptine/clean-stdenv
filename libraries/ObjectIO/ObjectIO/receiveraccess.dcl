definition module receiveraccess


//	Clean Object I/O library, version 1.2


import	StdReceiverDef, receiverhandle

// MW11 added [Id]
newReceiverStateHandle	:: !Id .ls !SelectState ![Id] !(ReceiverFunction  m   *(.ls,.pst)) -> ReceiverStateHandle .pst
newReceiverStateHandle2	:: !Id .ls !SelectState ![Id] !(Receiver2Function m r *(.ls,.pst)) -> ReceiverStateHandle .pst

newReceiverHandle		:: !Id     !SelectState ![Id] !(ReceiverFunction  m   *(.ls,.pst)) -> ReceiverHandle .ls .pst
newReceiverHandle2		:: !Id     !SelectState ![Id] !(Receiver2Function m r *(.ls,.pst)) -> ReceiverHandle .ls .pst
// .. MW11

onewaytotriple			:: !(ReceiverFunction  m   *(.ls,.pst)) m !*(.ls,.pst) -> *(.ls,[r],.pst)
twowaytotriple			:: !(Receiver2Function m r *(.ls,.pst)) m !*(.ls,.pst) -> *(.ls,[r],.pst)
