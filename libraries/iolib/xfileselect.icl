implementation module xfileselect;

XSelectInputFile :: !Int -> (!Int,!{#Char});
XSelectInputFile _
	= code {
		.inline XSelectInputFile
			ccall select_input_file "I-IS"
		.end
	};

XSelectOutputFile :: !{#Char} !{#Char} -> (!Int,!{#Char});
XSelectOutputFile _ _
	= code {
		.inline XSelectOutputFile
			ccall select_output_file "SS-IS"
		.end
	};

