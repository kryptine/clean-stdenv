implementation module xkernel;

InitToplevelX :: !Int -> Int;
InitToplevelX _
	= code {
		.inline InitToplevelX
			ccall init_toplevelx "I-I"
		.end
	};

SetToplevelNameX :: !{#Char} -> Int;
SetToplevelNameX _
	= code {
		.inline SetToplevelNameX
			ccall set_toplevelname "S-I"
		.end
	};

CloseToplevelX :: !Int -> Int;
CloseToplevelX _
	= code {
		.inline CloseToplevelX
			ccall close_toplevelx "I-I"
		.end
	};

OpenToplevelX :: !Int -> Int;
OpenToplevelX _
	= code {
		.inline OpenToplevelX
			ccall open_toplevelx "I-I"
		.end
	};

ShowToplevelX :: !Int -> Int;
ShowToplevelX _
	= code {
		.inline ShowToplevelX
			ccall show_toplevelx "I-I"
		.end
	};

HideToplevelX :: !Int -> Int;
HideToplevelX _
	= code {
		.inline HideToplevelX
			ccall hide_toplevelx "I-I"
		.end
	};

CatchXWidget :: !Int -> !(!Int,!Int);
CatchXWidget _
	= code {
		.inline CatchXWidget
			ccall single_event_catch "I-II"
		.end
	};

DestroyWidget :: !Int -> Int;
DestroyWidget _
	= code {
		.inline DestroyWidget
			ccall destroy_widget "I-I"
		.end
	};

