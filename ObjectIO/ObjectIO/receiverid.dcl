definition module receiverid

//	********************************************************************************
//	Clean Standard Object I/O library, version 1.2.2
//	
//	Author: Peter Achten
//	Modified: 7 September 2001 for Clean 2.0
//	********************************************************************************

import iostate

/*	bindRId requires:
		the Id of the receiver, 
		the SelectState of the receiver,
		the Id of the parent device instance,
		the Device kind of the parent device instance.
	bindRId adds the receiver in the receiver table.
*/
bindRId		:: !Id !SelectState !Id !Device !(IOSt .l) -> IOSt .l

/*	unbindRId removes the receiver identified by the Id.
*/
unbindRId	:: !Id !(IOSt .l) -> IOSt .l

/*	unbindRIds removes the receivers identified by the Ids.
*/
unbindRIds	:: ![Id] !(IOSt .l) -> IOSt .l
