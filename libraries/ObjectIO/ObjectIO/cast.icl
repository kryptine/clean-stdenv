implementation module cast


/*	Cast contains abc code because it can't be typed conventionally.
	The function Cast is required to break the Existential Type abstraction.
*/
Cast :: !.a -> .b
Cast a
	= code
		{
			pop_a 0
		}
