definition module tmdialog

from	StdId	import Ids, Id, R2Id, RId
from	StdPSt	import PSt, IOSt
from	StdIOCommon	import IdFun
import	tm

HelpFile
	:== "TuringHelp"

::	TmIds
	=	{	windowID		:: Id
		,	tapeWdID		:: Id
		,	fileMenuId		:: Id
		,	saveItemId		:: Id
		,	machineMenuId	:: Id
		,	stepItemId		:: Id
		,	haltItemId		:: Id
		}
::	Tm
	=	{	tmstate			:: !TmState
		,	name			:: !String
		,	delay			:: !Int
		,	saved			:: !Bool
		,	tmids			:: !TmIds
		}


openTmIds		:: !*env -> (!TmIds,!*env)	| Ids env

AlterCell		:: Int							(PSt Tm .p) -> PSt Tm .p
AlterTransition	:: Int							(PSt Tm .p) -> PSt Tm .p
AlterState		::								(PSt Tm .p) -> PSt Tm .p
ReDraw			::								(PSt Tm .p) -> PSt Tm .p
Alert			:: String String				(PSt Tm .p) -> PSt Tm .p
SaveBeforeClose	:: String (IdFun (PSt Tm .p))	(PSt Tm .p) -> PSt Tm .p
