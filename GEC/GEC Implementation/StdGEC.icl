implementation module StdGEC

import genericgecs

openGECVALUE :: !(!GUILoc,!OBJECTControlId) !OutputOnly !(Maybe t) !(Update t (PSt .ps)) !(PSt .ps) -> (!GECVALUE t (PSt .ps),!PSt .ps)
             |  gGEC{|*|} t & bimap{|*|} ps
openGECVALUE position outputOnly initValue valueUpdate pSt
	= gGEC{|*|} {location=position, makeUpValue=True, outputOnly=outputOnly,gec_value=initValue,update=valueUpdate} pSt
