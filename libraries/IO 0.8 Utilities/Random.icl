implementation module Random


import	StdInt, StdClass
from	deltaTimer	import	GetCurrentTime, :: CurrentTime, :: IOState


::	RandomSeed	:== Int


NullRandomSeed :: RandomSeed
NullRandomSeed = 0

GetNewRandomSeed :: !(IOState *s) -> (!RandomSeed, !IOState *s)
GetNewRandomSeed io
	# ((hours,minutes,seconds), io)	= GetCurrentTime io
	= (1+(hours+minutes+seconds) bitand 65535, io)

Random :: !RandomSeed -> (!Int,!RandomSeed)
Random seed
	= (newSeed,newSeed)
where
	newSeed		= if (nextSeed>=0) nextSeed (nextSeed+65537)
	nextSeed	= (seed75 bitand 65535)-(seed75>>16)
	seed75		= seed*75
