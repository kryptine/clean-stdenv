system module xtimer;

InstallXTimer :: !Int -> Int;
ChangeXTimerInterval :: !Int -> Int;
GetTimerInfo :: !Int -> Int;
EnableTheTimer :: !Int -> Int;
DisableTheTimer :: !Int -> Int;
XGetCurrentTime :: !Int -> !(!Int,!Int,!Int);
XGetCurrentDate :: !Int -> !(!Int,!Int,!Int,!Int);
WaitmSeconds :: !Int -> Int;
