definition module menuaccess


//	Clean Object I/O library, version 1.2


import	menuhandle


menuStateHandleGetHandle	:: !(MenuStateHandle .pst) -> (!OSMenu,  !MenuStateHandle .pst)
menuStateHandleGetMenuId	:: !(MenuStateHandle .pst) -> (!Id,      !MenuStateHandle .pst)
menuStateHandleGetOSMenuNr	:: !(MenuStateHandle .pst) -> (!OSMenuNr,!MenuStateHandle .pst)
menuStateHandleGetTitle		:: !(MenuStateHandle .pst) -> (!Title,   !MenuStateHandle .pst)
menuStateHandleGetSelect	:: !(MenuStateHandle .pst) -> (!Bool,    !MenuStateHandle .pst)

menuStateHandleSetHandle	:: !OSMenu	!(MenuStateHandle .pst) -> MenuStateHandle .pst
menuStateHandleSetTitle		:: !Title	!(MenuStateHandle .pst) -> MenuStateHandle .pst
menuStateHandleSetSelect	:: !Bool	!(MenuStateHandle .pst) -> MenuStateHandle .pst
