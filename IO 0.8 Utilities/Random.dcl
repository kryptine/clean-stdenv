definition module Random


from	deltaEventIO	import :: IOState


/*
	General utility for random generation.
	This module uses the 0.8 I/O library.
*/

::	RandomSeed

//	NullRandomSeed generates a useless RandomSeed (Random NullRandomSeed = (0,NullRandomSeed)).
NullRandomSeed	:: RandomSeed

//	GetNewRandomSeed generates a useful RandomSeed, using the current time.
GetNewRandomSeed:: !(IOState *s)	-> (!RandomSeed, !IOState *s)

//	Given a RandomSeed, Random generates a random number and a new RandomSeed.
Random			:: !RandomSeed		-> (!Int, !RandomSeed)
