definition module systemid


//	Clean Object I/O library, version 1.2


import StdOverloaded


::	SystemId


WorldSystemId	:: !Int			->	SystemId
WorldChildId	:: !Int			->	SystemId
InitSystemId	::	SystemId
NullSystemId	::  SystemId
IncrSystemId	:: !SystemId	->	(!SystemId,!SystemId)

instance == SystemId
instance <  SystemId
