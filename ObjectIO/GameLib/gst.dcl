definition module gst

import ostoolbox
from   StdFunc    import St
from   StdIOBasic import IdFun

::  *GSt gs

fromGSt :: !*(GSt .gs) -> (.gs,!*OSToolbox)
toGSt   :: .gs !*OSToolbox -> *GSt .gs
appGStTb:: !.(IdFun *OSToolbox)    !*(GSt .gs) ->       *GSt .gs
accGStTb:: !.(St    *OSToolbox .x) !*(GSt .gs) -> (!.x,!*GSt .gs)
appGSt  :: !.(IdFun .gs)           !*(GSt .gs) ->       *GSt .gs
accGSt  :: !.(St    .gs .x)        !*(GSt .gs) -> (.x, !*GSt .gs)
