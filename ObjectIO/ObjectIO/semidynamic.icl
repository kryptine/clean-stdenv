implementation module semidynamic


/*	This module implements a heavily degraded kind of dynamics. In a version of Clean
	with proper dynamics, this module will disappear.
*/

import	StdBool
import	cast, commondef, id

semidynamicFatalError :: String String -> .x
semidynamicFatalError function error
	= FatalError function "semidynamic" error

::	SemiDynamic					// A SemiDynamic:
	=	E. x:
		{	value	:: x		//	the encapsulated value
		,	id		:: Id		//	the identification key
		}
::	DId m
	:==	Id

openDynamic :: !(DId m) m -> SemiDynamic
openDynamic did x = {value=x,id=did}

matchDynamic :: !(DId m) !SemiDynamic -> Bool
matchDynamic did {id} = did==id

readDynamic :: !(DId m) !SemiDynamic -> Maybe m
readDynamic did {value,id}
	| did==id
		= Just (Cast value)
	| otherwise
		= Nothing

getDynamic :: !Id !SemiDynamic -> Maybe m
getDynamic did {value,id}
	| did==id
		= Just (Cast value)
	| otherwise
		= Nothing

setDynamic :: !Id m !SemiDynamic -> SemiDynamic
setDynamic did x sd=:{id}
	| did==id
		= {sd & value=x}
	| otherwise
		= semidynamicFatalError "setDynamic" "SemiDynamic did not match argument Id"

RIdtoDId :: !(RId m) -> DId m
RIdtoDId rid = RIdtoId rid

R2IdtoDId :: !(R2Id m r) -> DId m
R2IdtoDId r2id = R2IdtoId r2id

R2IdtoDId` :: !(R2Id m r) -> DId r
R2IdtoDId` r2id = R2IdtoId r2id

DIdtoId :: !(DId m) -> Id
DIdtoId did = did
