implementation module xpath;

GetHomePath :: !Int -> {#Char};
GetHomePath _
	= code {
		.inline GetHomePath
			ccall get_home_path "I-S"
		.end
	};

GetApplicationPath :: !Int -> {#Char};
GetApplicationPath _
	= code {
		.inline GetApplicationPath
			ccall get_appl_path "I-S"
		.end
	};

