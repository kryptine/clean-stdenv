module stackTraceNow

import code from "printStackNow.obj"

Start = printStackNow 1

printStackNow :: !Int -> Int
printStackNow stackDepth = code
	{	
		ccall printStackNow "I:I"
	}

stackPrintSize :: !Int -> Int
stackPrintSize stackDepth = code
	{	
		ccall stackPrintSize "I:I"
	}
