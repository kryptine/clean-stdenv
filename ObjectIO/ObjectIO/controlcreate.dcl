definition module controlcreate

//	********************************************************************************
//	Clean Standard Object I/O library, version 1.2.2
//	
//	Creation of controls
//	Author: Peter Achten
//	Modified: 7 September 2001 for Clean 2.0
//	********************************************************************************

import	ossystem, ostoolbox
import	windowhandle


createControls			:: !OSWindowMetrics          !(Maybe Id) !(Maybe Id) !Bool !OSWindowPtr ![WElementHandle .ls .pst] !*OSToolbox
																							-> (![WElementHandle .ls .pst],!*OSToolbox)
createCompoundControls	:: !OSWindowMetrics !Id !Int !(Maybe Id) !(Maybe Id) !Bool !OSWindowPtr ![WElementHandle .ls .pst] !*OSToolbox
																							-> (![WElementHandle .ls .pst],!*OSToolbox)
/*	create(Compound)Controls create the actual platform components that correspond with the 
	WElementHandle list.
		The first (Maybe Id) argument is the optional Id of the ok (Custom)ButtonControl, the second 
		identifies the cancel (Custom)ButtonControl.
		The Bool argument checks the SelectState (Able) of the context in which the elements are created.
		The OSWindowPtr is the system handle to the parent top-level window. 
	For create(Compound)Control the Id argument identifies the CompoundControl to which the elements
		are supposed to be added, and the Int argument is the number of its elements that have already
		been created (new elements are always added at the end of existing ones). 
*/
