
#include "util_12.h"

extern BOOL CleanThreadRunning( void );
extern BOOL OsThreadRunning( void );

extern void HandleCleanRequest( CrossCallInfo *pcci );

extern void KickCleanThread( CrossCallInfo *pcci );

extern void SendMessageToClean( int mess,
				         int p1,int p2,int p3,
					     int p4,int p5,int p6 );

#define SendMessage0ToClean(mess)                    SendMessageToClean((mess), 0,0,0,0,0,0)
#define SendMessage1ToClean(mess, p1)                SendMessageToClean((mess), (int)(p1),0,0,0,0,0)
#define SendMessage2ToClean(mess, p1,p2)             SendMessageToClean((mess), (int)(p1),(int)(p2),0,0,0,0)
#define SendMessage3ToClean(mess, p1,p2,p3)          SendMessageToClean((mess), (int)(p1),(int)(p2),(int)(p3),0,0,0)
#define SendMessage4ToClean(mess, p1,p2,p3,p4)       SendMessageToClean((mess), (int)(p1),(int)(p2),(int)(p3),(int)(p4),0,0)
#define SendMessage5ToClean(mess, p1,p2,p3,p4,p5)    SendMessageToClean((mess), (int)(p1),(int)(p2),(int)(p3),(int)(p4),(int)(p5),0)
#define SendMessage6ToClean(mess, p1,p2,p3,p4,p5,p6) SendMessageToClean((mess), (int)(p1),(int)(p2),(int)(p3),(int)(p4),(int)(p5),(int)(p6))



extern DWORD OsThreadFunction( DWORD param );

extern BOOL IsReturnCci( CrossCallInfo *pcci );

int nCopyAnsiToWideChar (LPWORD, LPSTR);
