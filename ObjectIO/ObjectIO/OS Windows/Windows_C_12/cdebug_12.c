#include "cdebug_12.h"

extern EXPORT_TO_CLEAN OS 
ConsolePrint (CLEAN_STRING cleanstr, OS os)
{
	char *cstr;

	cstr = cstring (cleanstr);
	rprintf (cstr);
	return os;
}

extern EXPORT_TO_CLEAN int 
Rand (void)
{
	static int holdrand;
	static int randinited = 0;

	if (!randinited)
	{
		holdrand = (int) GetTickCount ();
		randinited = -1;
	}

	holdrand = holdrand * 214013 + 2531011;

	return ((holdrand >> 16) & 0x7fff);
}
