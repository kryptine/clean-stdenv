implementation module world

//	Clean Object I/O library, version 1.2

loadWorld :: !World -> Int
loadWorld w
	= code {
		pushI_a	0
		pop_a	1
	}

storeWorld :: !Int !World -> *World
storeWorld i w
	= code {
		fillI_b	0 1
		pop_b	1
		pop_a	1
	}
