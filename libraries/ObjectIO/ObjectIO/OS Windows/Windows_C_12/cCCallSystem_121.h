#include "intrface_121.h"

extern int  WinBeep (int);
extern void WinGetTime (int,int*,int*,int*,int*);
extern void WinGetDate (int,int*,int*,int*,int*,int*);
extern int  WinWait (int,int);
extern void WinGetBlinkTime (int,int*,int*);
extern void WinGetTickCount (OS ios, int *tickCount, OS * oos);
extern void WinPlaySound (CLEAN_STRING clfilename, OS ios, Bool * ook, OS * oos);
