definition module receiverid

//	Clean Object I/O library, version 1.2

import iostate

/*	bindRId requires:
		the Id of the receiver, 
		the SelectState of the receiver,
		the Id of the parent device instance,
		the Device kind of the parent device instance.
	bindRId adds the receiver in the receiver table.
*/
bindRId		:: !Id !SelectState !Id !Device !(IOSt .l .p) -> IOSt .l .p

/*	unbindRId removes the receiver identified by the Id.
*/
unbindRId	:: !Id !(IOSt .l .p) -> IOSt .l .p

/*	unbindRIds removes the receivers identified by the Ids.
*/
unbindRIds	:: ![Id] !(IOSt .l .p) -> IOSt .l .p
