implementation module cursorInternal;

import xtypes,xcursor,deltaIOSystem;
import ioState,windowDevice;

    
SetWidgetCursor :: !Widget !CursorShape -> Widget; 
SetWidgetCursor w StandardCursor	=  XSetWidgetCursor XStandardCursor w;
SetWidgetCursor w BusyCursor 		=  XSetWidgetCursor XBusyCursor w;
SetWidgetCursor w IBeamCursor 	=  XSetWidgetCursor XIBeamCursor w;
SetWidgetCursor w CrossCursor 	=  XSetWidgetCursor XCrossCursor w;
SetWidgetCursor w FatCrossCursor	=  XSetWidgetCursor XFatCrossCursor w;
SetWidgetCursor w ArrowCursor 	=  XSetWidgetCursor XArrowCursor w;
SetWidgetCursor w othercursor		=  w;

SetGlobalCursor :: !CursorShape !(IOState s) -> IOState s;
SetGlobalCursor shape io_state
   #!   strict2=IOStateGetDevice io_state WindowDevice;
   #  (device, io_state`)= strict2;
   | WindowDeviceNotEmpty device
	   #! device`= SetGlobalCursor` shape device;
		= IOStateSetDevice io_state` device`;
		= io_state`;

SetGlobalCursor` :: !CursorShape !(DeviceSystemState s) -> DeviceSystemState s;
SetGlobalCursor` shape (WindowSystemState windows)
   =  WindowSystemState (SetGlobalCursor`` shape windows);

SetGlobalCursor`` :: !CursorShape ![(WindowDef s (IOState s), Window)]
                     -> [(WindowDef s (IOState s), Window)];
SetGlobalCursor`` shape [window=:(def,(win,pic)) : windows]
   #!
		win`=win`;
		strict2=strict2;
		=
		[(def, (win`,pic)) : strict2];
      where {
      win`=: SetWidgetCursor win shape;
      strict2=SetGlobalCursor`` shape windows;
		};
SetGlobalCursor`` shape [] =  [];

ResetCursor :: !(IOState s) -> IOState s;
ResetCursor io_state
   #! strict2=IOStateGetDevice io_state WindowDevice;
   #   (device, io_state`)= strict2;
   | WindowDeviceNotEmpty device #!
      device`= ResetCursor` device;
		=
		IOStateSetDevice io_state` device`;
		=
		io_state`;
 
ResetCursor` :: !(DeviceSystemState s) -> DeviceSystemState s;
ResetCursor` (WindowSystemState windows)
   =  WindowSystemState (ResetCursor`` windows);
 
ResetCursor`` :: ![(WindowDef s (IOState s), Window)]
                 -> [(WindowDef s (IOState s), Window)];
ResetCursor`` [window=:(def,(win,pic)) : windows]
   #!
		win`=win`;
		strict2=strict2;
		=
		[(def, (win`,pic)) : strict2];
      where {
      win`=: SetWidgetCursor win shape;
      shape=: GetCursor (WindowDef_Attributes def);
      strict2=ResetCursor`` windows;
		};
ResetCursor`` [] =  [];

GetCursor :: ![WindowAttribute s (IOState s)] -> CursorShape;
GetCursor [Cursor shape : atts] =  shape;
GetCursor [no_cursor : atts]    =  GetCursor atts;
GetCursor []                    =  StandardCursor;
