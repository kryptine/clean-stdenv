#ifndef _COSGAMELIB_H
#define _COSGAMELIB_H

/* Clean Game Library by Mike Wiering, Nijmegen */

/* DirectX implementation of the OS specific functions */

/* all BOOL functions return TRUE if success, FALSE if failure */

#include "util_121.h"
#include "intrface_121.h"

#define WIN32_LEAN_AND_MEAN

#include <windowsx.h>
#include <ddraw.h>
#include <dsound.h>
#include "ddutil.h"
#include "dsutil.h"

/* Game Result Codes */
#define GR_OK					 0
#define GR_FAILED               -1
#define GR_OS_ERROR				-2	/* OS function returns an error */
#define GR_INVALID_BITMAP_ID	-3
#define GR_INVALID_SPRITE_ID	-4
#define GR_INVALID_MAP_ID		-5
#define GR_NOT_FOUND			-6	/* file or resource not found */


/* display option bits */
#define DO_BLINK	     		(1 << 0)
#define DO_STRETCH              (1 << 1)
#define DO_MIRROR_LEFT_RIGHT    (1 << 2)
#define DO_MIRROR_UP_DOWN       (1 << 3)
#define DO_ROTATE_90            (1 << 4)
#define DO_ROTATE_180           (1 << 5)
#define DO_ROTATE_270           (1 << 6)

/* PA: all functions changed to extern: */
/* --------------------- window / screen functions --------------------- */

/* set up the game window */
extern BOOL OSInitGameWindow (void);
/* shut down the game window */
extern void OSDeInitGameWindow (void);

/* get game window handle */
extern BOOL OSGetGameWindowHDC (HDC *hdc);
/* release game window handle */
extern void OSReleaseGameWindowHandle (HDC hdc);

/* clear the (visual) screen */
extern void OSClearScreen (void);
/* clear the virtual screen */
extern void OSClearVirtualScreen (COLORREF c);

/* fill an area with black */
extern void OSFillBlack (BOOL vis, RECT r);

/* copy (part of) virtual screen to visual screen */
extern void OSBlit (RECT *r);

/* flip pages */
extern void OSFlip (void);

/* ------------------------- bitmap functions  ------------------------- */

/* initialize a game bitmap */
extern int OSInitGameBitmap (int id, char *name,
							 int bitmapwidth, int bitmapheight,
							 int blockwidth, int blockheight
							);
/* get bitmap info */
extern BOOL OSGetGameBitmapInfo (int id, int *width, int *height,
								 int *blockwidth, int *blockheight,
								 int *blockcountx, int *blockcounty
								);

/* deinit a game bitmap */
extern int OSFreeGameBitmap (int id);
/* deinit all game bitmaps */
extern void OSFreeGameBitmaps (void);

/* set transparent color */
extern int OSSetTransparentColor (int id, int x, int y);

/* initialize a block sequence */
extern int OSInitBlockSequence (int bitmapid, int seqid, char *seq, int len);
/* run block sequences */
extern void OSRunBlockSequences (void);
/* get current block */
extern int OSGetCurrentBlock (int bitmapid, int seqid);

/* draw part of a bitmap to virtual screen */
extern void OSDraw (RECT *dst, int id, RECT *src, BOOL mirlr, BOOL mirud, int flags);

/* -------------------------- sound functions -------------------------- */

/* initialize sound when program starts */
extern BOOL OSInitSound (void);
/* initialize sound before program terminates */
extern void OSDeInitSound (void);

/* initialize a sound sample so it can be played later */
extern BOOL OSInitSoundSample (int id, char *name, int buffers);
/* deinitialize all sound samples */
extern void OSFreeSoundSamples (void);
/* play a sound sample */
extern BOOL OSPlaySoundSample (int id, int volume, int pan, int freq);

/* start playing music in the background */
extern BOOL OSPlayMusic (char *midifile, BOOL restart);
/* stop music */
extern BOOL OSStopMusic (void);

#endif
