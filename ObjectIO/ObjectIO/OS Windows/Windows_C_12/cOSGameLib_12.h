#ifndef _COSGAMELIB_H
#define _COSGAMELIB_H

/* Clean Game Library by Mike Wiering, Nijmegen */

/* DirectX implementation of the OS specific functions */

/* all BOOL functions return TRUE if success, FALSE if failure */

#include "util_12.h"
#include "intrface_12.h"

#define WIN32_LEAN_AND_MEAN

#include <windowsx.h>
#include <ddraw.h>
#include <dsound.h>
#include "ddutil.h"
#include "dsutil.h"


/* display option bits */
#define DO_BLINK	     		(1 << 0)
#define DO_STRETCH              (1 << 1)
#define DO_MIRROR_LEFT_RIGHT    (1 << 2)
#define DO_MIRROR_UP_DOWN       (1 << 3)
#define DO_ROTATE_90            (1 << 4)
#define DO_ROTATE_180           (1 << 5)
#define DO_ROTATE_270           (1 << 6)


/* --------------------- window / screen functions --------------------- */

/* set up the game window */
BOOL OSInitGameWindow ();
/* shut down the game window */
void OSDeInitGameWindow ();

/* get game window handle */
BOOL OSGetGameWindowHDC (HDC *hdc);
/* release game window handle */
void OSReleaseGameWindowHandle (HDC hdc);

/* clear the (visual) screen */
void OSClearScreen ();
/* clear the virtual screen */
void OSClearVirtualScreen (COLORREF c);

/* fill an area with black */
void OSFillBlack (BOOL vis, RECT r);

/* copy (part of) virtual screen to visual screen */
void OSBlit (RECT *r);

/* flip pages */
void OSFlip ();

/* ------------------------- bitmap functions  ------------------------- */

/* initialize a game bitmap */
int OSInitGameBitmap (int id, char *name,
                      int bitmapwidth, int bitmapheight,
                      int blockwidth, int blockheight);
/* get bitmap info */
BOOL OSGetGameBitmapInfo (int id, int *width, int *height,
                          int *blockwidth, int *blockheight,
                          int *blockcountx, int *blockcounty);

/* deinit a game bitmap */
int OSFreeGameBitmap (int id);
/* deinit all game bitmaps */
void OSFreeGameBitmaps ();

/* set transparent color */
int OSSetTransparentColor (int id, int x, int y);

/* initialize a block sequence */
int OSInitBlockSequence (int bitmapid, int seqid, char *seq, int len);
/* run block sequences */
void OSRunBlockSequences ();
/* get current block */
int OSGetCurrentBlock (int bitmapid, int seqid);

/* draw part of a bitmap to virtual screen */
void OSDraw (RECT *dst, int id, RECT *src, BOOL mirlr, BOOL mirud, int flags);

/* -------------------------- sound functions -------------------------- */

/* initialize sound when program starts */
BOOL OSInitSound ();
/* initialize sound before program terminates */
void OSDeInitSound ();

/* initialize a sound sample so it can be played later */
BOOL OSInitSoundSample (int id, char *name, int buffers);
/* deinitialize all sound samples */
void OSFreeSoundSamples ();
/* play a sound sample */
BOOL OSPlaySoundSample (int id, int volume, int pan, int freq);

/* start playing music in the background */
BOOL OSPlayMusic (char *midifile, BOOL restart);
/* stop music */
BOOL OSStopMusic ();

#endif

