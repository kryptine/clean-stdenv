definition module systemid


//	Clean Object I/O library, version 1.2


import StdOverloaded


::	SystemId


worldSystemId	:: !Int			->	SystemId
worldChildId	:: !Int			->	SystemId
initSystemId	::	SystemId
nullSystemId	::  SystemId
incrSystemId	:: !SystemId	->	(!SystemId,!SystemId)

instance == SystemId
instance <  SystemId
