implementation module xcursor;

XSetWidgetCursor :: !Int !Int -> Int;
XSetWidgetCursor _ _
	= code {
		.inline XSetWidgetCursor
			ccall set_window_cursor "II-I"
		.end
	};

