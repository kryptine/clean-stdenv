#include "util_121.h"
#define RECT void*
#define HWND void*

extern OSWindowPtr ghCaretWnd;

extern void DeleteCursors(); /* Delete all created mouse cursors */

/*
 * InstallCrossCallFileSelectors adds the proper cross call procedures to the
 * cross call procedures managed by cCrossCall_121.c.
 */
extern OS InstallCrossCallWindows (OS);

/* GetUpdateRect */
extern int GetUpdateRect(HWND hWin, RECT updateRect, gboolean ok);
