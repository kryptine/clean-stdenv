definition module gst

import ostoolbox

::  *GSt gs

fromGSt :: !*(GSt .gs) -> (.gs,!*OSToolbox)
toGSt   :: .gs !*OSToolbox -> *GSt .gs
appGStTb:: !(*OSToolbox -> *OSToolbox) !*(GSt .gs) -> *GSt .gs
accGStTb:: !(*OSToolbox -> (.x,*OSToolbox)) !*(GSt .gs) -> (!.x,!*GSt .gs)
appGSt  :: !(.gs -> .gs) !*(GSt .gs) -> *GSt .gs
accGSt  :: !(.gs -> (.x,.gs)) !*(GSt .gs) -> (.x,!*GSt .gs)
