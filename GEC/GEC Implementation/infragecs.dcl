definition module infragecs

import testable
import parseprint, guigecs

::	GECArgs t env
	=	{	location	:: !(!GUILoc,!OBJECTControlId) 
		,	makeUpValue	:: !MakeUpValue
	    ,   outputOnly	:: !OutputOnly
	    ,	gec_value	:: !.Maybe t
		,	update		:: !(Update t env)
		}

derive bimap GECArgs

/**	In this module the 'standard' GEC infrastructure creation functions are defined. 
	These functions are:
		unitGEC:	defines the infrastructure for UNITs,
		pairGEC:	defines the infrastructure for PAIRs,
		objectGEC:	defines the infrastructure for OBJECTs,
		consGEC:	defines the infrastructure for CONSs,
		fieldGEC:	defines the infrastructure for FIELDs.
		eitherGEC:	defines the infrastructure for EITHERs,
		basicGEC:	defines the infrastructure for parseable/printable types (useful as a leaf-editor),
	The (GECGUIFun t env) argument of each function defines the GECGUI of this particular component (see guigecs.dcl).
*/

::	InfraGEC a env
	:== ((GECArgs a env)
     -> .(env 
     -> *(!GECVALUE a env,!env)))
::	MakeUpValue	:== Bool

unitGEC   ::                         !(GECGUIFun UNIT         (PSt .ps))                         -> InfraGEC UNIT         (PSt .ps)
pairGEC   ::                         !(GECGUIFun (PAIR a b)   (PSt .ps)) !(InfraGEC a (PSt .ps)) 
                                                                         !(InfraGEC b (PSt .ps)) -> InfraGEC (PAIR   a b) (PSt .ps)
objectGEC :: !GenericTypeDefDescriptor 
             !(GECId (OBJECT a))     !(GECGUIFun (OBJECT a)   (PSt .ps)) !(InfraGEC a (PSt .ps)) -> InfraGEC (OBJECT a)   (PSt .ps)
consGEC   :: !GenericConsDescriptor  !(GECGUIFun (CONS   a)   (PSt .ps)) !(InfraGEC a (PSt .ps)) -> InfraGEC (CONS   a)   (PSt .ps)
fieldGEC  :: !GenericFieldDescriptor !(GECGUIFun (FIELD  a)   (PSt .ps)) !(InfraGEC a (PSt .ps)) -> InfraGEC (FIELD  a)   (PSt .ps)
eitherGEC ::                         !(GECGUIFun (EITHER a b) (PSt .ps)) !(InfraGEC a (PSt .ps))
                                                                         !(InfraGEC b (PSt .ps)) -> InfraGEC (EITHER a b) (PSt .ps)
basicGEC  :: !String !(GECId t)      !(GECGUIFun t            (PSt .ps))                         -> InfraGEC t            (PSt .ps)
          |  parseprint    t 
          &  generate{|*|} t
