definition module StdGEC

import genericgecs
from   guigecs import defTextWidths, defWindowBackColour

openGECVALUE :: !(!GUILoc,!OBJECTControlId) !OutputOnly !(Maybe t) !(Update t (PSt .ps)) !(PSt .ps) -> (!GECVALUE t (PSt .ps),!PSt .ps) 
             |  gGEC{|*|} t & bimap{|*|} ps