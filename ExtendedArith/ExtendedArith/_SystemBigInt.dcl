definition module _SystemBigInt

:: BigInt	= 	{	_sign_or_number	::	!Int
				,	_limbs			::	!.{#Int}
				}
// For efficiency this type is _not_ abstract.
// Don't try to access the fields of this record.
