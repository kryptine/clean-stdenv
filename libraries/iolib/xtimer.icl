implementation module xtimer;

InstallXTimer :: !Int -> Int;
InstallXTimer _
	= code {
		.inline InstallXTimer
			ccall install_timer "I-I"
		.end
	};

ChangeXTimerInterval :: !Int -> Int;
ChangeXTimerInterval _
	= code {
		.inline ChangeXTimerInterval
			ccall change_timer_interval "I-I"
		.end
	};

GetTimerInfo :: !Int -> Int;
GetTimerInfo _
	= code {
		.inline GetTimerInfo
			ccall get_timer_count "I-I"
		.end
	};

EnableTheTimer :: !Int -> Int;
EnableTheTimer _
	= code {
		.inline EnableTheTimer
			ccall enable_timer "I-I"
		.end
	};

DisableTheTimer :: !Int -> Int;
DisableTheTimer _
	= code {
		.inline DisableTheTimer
			ccall disable_timer "I-I"
		.end
	};

XGetCurrentTime :: !Int -> !(!Int,!Int,!Int);
XGetCurrentTime _
	= code {
		.inline XGetCurrentTime
			ccall get_current_time "I-III"
		.end
	};

XGetCurrentDate :: !Int -> !(!Int,!Int,!Int,!Int);
XGetCurrentDate _
	= code {
		.inline XGetCurrentDate
			ccall get_current_date "I-IIII"
		.end
	};

WaitmSeconds :: !Int -> Int;
WaitmSeconds _
	= code {
		.inline WaitmSeconds
			ccall wait_mseconds "I-I"
		.end
	};

