implementation module xwindow;

XCreateWindow :: !Int !Int !Int !Int !Int !{#Char} !Int !Int !Int !Int !Int !Int !Int !Int !Int !Int -> (!Int,!Int);
XCreateWindow _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
	= code {
		.inline XCreateWindow
			ccall create_window "IIIIISIIIIIIIIII-II"
		.end
	};

GetMouseInfo :: !Int -> (!Int,!Int,!Int,!Int,!Int,!Int,!Int);
GetMouseInfo _
	= code {
		.inline GetMouseInfo
			ccall get_mouse_state "I-IIIIIII"
		.end
	};

GetXExposeArea :: !Int -> (!Int,!Int,!Int,!Int,!Int);
GetXExposeArea _
	= code {
		.inline GetXExposeArea
			ccall get_expose_area "I-IIIII"
		.end
	};

StartXUpdate :: !Int -> Int;
StartXUpdate _
	= code {
		.inline StartXUpdate
			ccall start_update "I-I"
		.end
	};

EndXUpdate :: !Int -> Int;
EndXUpdate _
	= code {
		.inline EndXUpdate
			ccall end_update "I-I"
		.end
	};

GetKeyboardInfo :: !Int -> (!Int,!Int,!Int,!Int,!Int,!Int);
GetKeyboardInfo _
	= code {
		.inline GetKeyboardInfo
			ccall get_key_state "I-IIIIII"
		.end
	};

XScreenSize :: !Int -> (!Int,!Int);
XScreenSize _
	= code {
		.inline XScreenSize
			ccall get_screen_size "I-II"
		.end
	};

GetXWindowEvent :: !Int -> Int;
GetXWindowEvent _
	= code {
		.inline GetXWindowEvent
			ccall get_window_event "I-I"
		.end
	};

XSetScrollBar :: !Int !Int !Int !Int !Int !Int !Int -> (!Int,!Int);
XSetScrollBar _ _ _ _ _ _ _
	= code {
		.inline XSetScrollBar
			ccall set_scrollbars "IIIIIII-II"
		.end
	};

XGetCurrentWindowSize :: !Int -> (!Int,!Int);
XGetCurrentWindowSize _
	= code {
		.inline XGetCurrentWindowSize
			ccall get_window_size "I-II"
		.end
	};

XGetCurrentWindowThumbs :: !Int -> (!Int,!Int);
XGetCurrentWindowThumbs _
	= code {
		.inline XGetCurrentWindowThumbs
			ccall get_current_thumbs "I-II"
		.end
	};

ChangeXWindow :: !Int !Int !Int !Int !Int !Int !Int !Int !Int !Int !Int !Int !Int !Int -> Int;
ChangeXWindow _ _ _ _ _ _ _ _ _ _ _ _ _ _
	= code {
		.inline ChangeXWindow
			ccall change_window "IIIIIIIIIIIIII-I"
		.end
	};

GetFirstUpdateX :: !Int -> (!Int,!Int);
GetFirstUpdateX _
	= code {
		.inline GetFirstUpdateX
			ccall get_first_update "I-II"
		.end
	};

DiscardXUpdates :: !Int -> Int;
DiscardXUpdates _
	= code {
		.inline DiscardXUpdates
			ccall discard_updates "I-I"
		.end
	};

XActivateWindow :: !Int -> Int;
XActivateWindow _
	= code {
		.inline XActivateWindow
			ccall activate_window "I-I"
		.end
	};

XSetWindowTitle :: !Int !{#Char} -> Int;
XSetWindowTitle _ _
	= code {
		.inline XSetWindowTitle
			ccall set_window_title "IS-I"
		.end
	};

XPopDown :: !Int -> Int;
XPopDown _
	= code {
		.inline XPopDown
			ccall popdown "I-I"
		.end
	};

XPopup :: !Int -> Int;
XPopup _
	= code {
		.inline XPopup
			ccall popup "I-I"
		.end
	};

XSetDoubleDownDistance :: !Int -> Int;
XSetDoubleDownDistance _
	= code {
		.inline XSetDoubleDownDistance
			ccall set_dd_distance "I-I"
		.end
	};

XGetWindowPosition :: !Int -> (!Int,!Int);
XGetWindowPosition _
	= code {
		.inline XGetWindowPosition
			ccall get_window_position "I-II"
		.end
	};

