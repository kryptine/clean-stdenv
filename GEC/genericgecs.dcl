definition module genericgecs

import gec
import infragecs
import StdBimap

/** gGEC guiLocation isExitPoint makeUpValue outputOnly initValue extInterface env -->  (gec,env)
		creates the value-infrastructure of a Visual Editor Component (GEC) that is defined by
		induction on the structure of the type parameter t.
		
		Arguments:
		guiLocation:	a legal value of an existing GUI component in which this GEC is to be created.
		isExitPoint:	for internal purposes. This value must be True.
		makeUpValue:	for internal purposes. This value must be True.
		outputOnly:		the GEC is for output purposes only (True) or can be edited by the user (False).
		initValue:		the optional initial value of type t that will be created.
		extInterface:	the interface function that a program can use to obtain information from
						the GEC at run-time without the need for polling.
		
		Result:
		gec:			the methods of the created GEC that a program can use to obtain information
						from the GEC at run-time in a polling way.
*/
generic gGEC t :: !(GECArgs t (PSt .ps)) !(PSt .ps) -> (!GECVALUE t (PSt .ps),!PSt .ps)
 
derive gGEC Bool, Int, Real, Char, String, UNIT, PAIR, EITHER, CONS, FIELD, OBJECT
