definition module systemid


//	********************************************************************************
//	Clean Standard Object I/O library, version 1.2.2
//	
//	Author: Peter Achten
//	Modified: 7 September 2001 for Clean 2.0
//	********************************************************************************


import StdOverloaded


::	SystemId


worldSystemId	:: !Int			->	SystemId
worldChildId	:: !Int			->	SystemId
initSystemId	::	SystemId
nullSystemId	::  SystemId
incrSystemId	:: !SystemId	->	(!SystemId,!SystemId)

instance == SystemId
instance <  SystemId
