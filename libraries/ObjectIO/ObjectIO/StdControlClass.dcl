definition module StdControlClass


//	********************************************************************************
//	Clean Standard Object I/O library, version 1.2
//	
//	StdControlClass define the standard set of controls instances.
//	********************************************************************************


import	StdIOCommon, StdControlDef
from	windowhandle	import ControlState
from	StdPSt			import PSt, IOSt


class Controls cdef where
	controlToHandles	:: !(cdef      .ls (PSt .l)) !(PSt .l)
					-> (![ControlState .ls (PSt .l)], !PSt .l)
	getControlType		::  (cdef      .ls .pst)
					-> ControlType

instance Controls (AddLS  c)			| Controls c
instance Controls (NewLS  c)			| Controls c
instance Controls (ListLS c)			| Controls c
instance Controls NilLS
instance Controls ((:+:) c1 c2)			| Controls c1 & Controls c2
instance Controls RadioControl
instance Controls CheckControl
instance Controls PopUpControl
instance Controls SliderControl
instance Controls TextControl
instance Controls EditControl
instance Controls ButtonControl
instance Controls CustomButtonControl
instance Controls CustomControl
instance Controls (CompoundControl c)	| Controls c
