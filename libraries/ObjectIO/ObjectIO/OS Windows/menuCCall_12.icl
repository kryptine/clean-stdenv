implementation module menuCCall_12

import ostoolbox
import code from "cCrossCallMenus_121.obj"

WinInitialiseMenus :: !*OSToolbox -> *OSToolbox
WinInitialiseMenus _
	= code
	{
		.inline InstallCrossCallMenus
			ccall InstallCrossCallMenus "I-I"
		.end
	}
