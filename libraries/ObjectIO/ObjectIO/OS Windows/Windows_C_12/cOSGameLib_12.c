
/* Clean Game Library by Mike Wiering, Nijmegen */

/* DirectX implementation of the OS specific functions */

#include "cOSGameLib_12.h"

extern HWND ghMainWindow;

int ScreenWidth = 320;
int ScreenHeight = 240;
int BitsPerPixel = 8;

int ActualWidth = 320;
int ActualHeight = 240;

int XShiftScreen = 0;
int YShiftScreen = 0;

BOOL FullScreen = TRUE;
BOOL bGameActive = FALSE;
HWND ghGameWindow = NULL;

static LPDIRECTDRAW lpDD = NULL;
static LPDIRECTDRAWSURFACE lpDDSFront = NULL;
static LPDIRECTDRAWSURFACE lpDDSBack = NULL;
static LPDIRECTDRAWPALETTE lpDDPal = NULL;
static IDirectDrawClipper *clipper = NULL;

typedef struct GAMEBLOCKSEQUENCE
{
    int iSequenceID;
    int iSequenceLength;
    char *sSequence;
    int iPosition;
    int iCounter;
    struct GAMEBLOCKSEQUENCE *gbsNext;
} GAMEBLOCKSEQUENCE;

typedef struct GAMEBITMAPINFO
{
    int iBitmapID;
    LPDIRECTDRAWSURFACE lpDDSBitmap;
    int iBitmapWidth;
    int iBitmapHeight;
    int iBlockWidth;
    int iBlockHeight;
    int iBlockCountX;
    int iBlockCountY;
    BOOL bTransparent;
    struct GAMEBLOCKSEQUENCE *gbsGameBlockSequence;
    struct GAMEBITMAPINFO *gbipNext;
    char *sName;
} GAMEBITMAPINFO;

static GAMEBITMAPINFO *gbipGameBitmapInfo = NULL;

static int iPrevGBIP = 0;
static GAMEBITMAPINFO *gbipPrev = NULL;


/* release DirectDraw object */
void ReleaseDD (void)
{
  if (lpDDPal)
  {
    IDirectDrawSurface_Release (lpDDPal);
    lpDDPal = NULL;
  }

  if (clipper)
  {
     IDirectDrawClipper_Release (clipper);
     clipper = NULL;
  }

  if (lpDD != NULL)
  {
    if (lpDDSFront != NULL)
    {
      IDirectDraw_Release (lpDDSFront);
      lpDDSFront = NULL;
    }
    IDirectDraw_Release (lpDD);
    lpDD = NULL;
  }
}

/* restore directdraw object */
void DDRestoreAll ()
{
    HRESULT ddrval;
    DDSURFACEDESC ddsd;
    GAMEBITMAPINFO *gbip = gbipGameBitmapInfo;

    if (lpDD != NULL)
    {
        ddrval = IDirectDrawSurface_Restore (lpDDSFront);
        if (ddrval == DD_OK)
        {
            ddrval = IDirectDrawSurface_Restore (lpDDSBack);
            if (ddrval == DD_OK)
            {
                while (gbip)
                {
                    ddrval = IDirectDrawSurface_Restore (gbip->lpDDSBitmap);
                    if (ddrval == DD_OK)
                    {
                        DDReLoadBitmap (gbip->lpDDSBitmap, gbip->sName);
                    }
                    gbip = gbip->gbipNext;
                }
            }
        }
    }
}




/* --------------------- window / screen functions --------------------- */

typedef struct {
  GUID *guid;
  GUID DriverGUID;
  CHAR DriverDesc[128];
  CHAR DriverName[128];
} DEVICES;

DEVICES aDDDevs[15];
int MaxDevIndex = 0;
int DevIndex = 0;

HRESULT CALLBACK DDEnumCallback (GUID *lpGUID,
                                 LPSTR DriverDesc,
                                 LPSTR DriverName,
                                 LPVOID lpContext)
{
    if (MaxDevIndex >= sizeof (aDDDevs) / sizeof (aDDDevs[0]))
        return E_FAIL;

    // Msg ("Device: %s (%s)", DriverDesc, DriverName);

    if (lpGUID == NULL || lpGUID == (GUID*) DDCREATE_EMULATIONONLY)
    {
        aDDDevs[MaxDevIndex].guid = lpGUID;
    }
    else
    {
        aDDDevs[MaxDevIndex].DriverGUID = *lpGUID;
        aDDDevs[MaxDevIndex].guid = &aDDDevs[MaxDevIndex].DriverGUID;
    }

    lstrcpyn(aDDDevs[MaxDevIndex].DriverDesc, DriverDesc, 128);
    lstrcpyn(aDDDevs[MaxDevIndex].DriverName, DriverName, 128);

    MaxDevIndex++;
    return DDENUMRET_OK;
}


static void ShowError (char *Msg)
{
    ReleaseDD ();
    if (ghGameWindow)
    {
        DestroyWindow (ghGameWindow);
        ghGameWindow = NULL;
    }
    if (ghMainWindow)
    {
        DestroyWindow (ghMainWindow);
        ghMainWindow = NULL;
    }
    MessageBox (NULL, Msg, NULL, MB_OK);
}


/* set up the game window */
BOOL OSInitGameWindow ()
{
    HRESULT ddrval;
    DDSURFACEDESC ddsd;
    DDSCAPS ddscaps;
    int result;

    result = FALSE;

//    DirectDrawEnumerate ((LPDDENUMCALLBACK) &DDEnumCallback, NULL);
//    DDEnumCallback ((GUID *) DDCREATE_EMULATIONONLY,
//                    "Hardware Emulation Layer", "", NULL);
//    DevIndex = MaxDevIndex - 1;

//    ddrval = DirectDrawCreate (aDDDevs[DevIndex].guid, &lpDD, NULL);
    ddrval = DirectDrawCreate (NULL, &lpDD, NULL);
    if (!(ddrval == DD_OK))
        ShowError ("DirectDrawCreate failed");
    else
    {
        if (FullScreen)
            ddrval = IDirectDraw_SetCooperativeLevel (lpDD, ghGameWindow,
                         DDSCL_EXCLUSIVE |
                         DDSCL_FULLSCREEN |
                         DDSCL_ALLOWMODEX |
                         DDSCL_ALLOWREBOOT);
        else
            ddrval = IDirectDraw_SetCooperativeLevel (lpDD, ghGameWindow,
                DDSCL_NORMAL);
        if (!(ddrval == DD_OK))
            ShowError ("IDirectDraw_SetCooperativeLevel failed");
        else
        {
            ActualWidth = ScreenWidth;
            ActualHeight = ScreenHeight;

            if (FullScreen)
            {
                int oldbpp = BitsPerPixel;

                ShowCursor (FALSE);
                ddrval = IDirectDraw_SetDisplayMode (lpDD, ScreenWidth, ScreenHeight, BitsPerPixel);

                /* added 11-24-99, try 8 bit color if 16/24/32 bit fails */
                if (BitsPerPixel > 8)
                {
                    if (!(ddrval == DD_OK))
                    {
                        BitsPerPixel = 8;
                        ddrval = IDirectDraw_SetDisplayMode (lpDD, ScreenWidth, ScreenHeight, BitsPerPixel);
                    }
                }
                /* added  3-29-00, try 640x480 if lower resolution fails */
                if ((!(ddrval == DD_OK)) && ((ScreenWidth < 640) || (ScreenHeight < 480)))
                {
                  //  MessageBox (NULL, "Screen mode not supported. Trying higher resolution", NULL, MB_OK);

                    ActualWidth = 640;
                    ActualHeight = 480;
                    BitsPerPixel = oldbpp;
                    ddrval = IDirectDraw_SetDisplayMode (lpDD, ActualWidth, ActualHeight, BitsPerPixel);

                    if (BitsPerPixel > 8)
                    {
                        if (!(ddrval == DD_OK))
                        {
                            BitsPerPixel = 8;
                            ddrval = IDirectDraw_SetDisplayMode (lpDD, ScreenWidth, ScreenHeight, BitsPerPixel);
                        }
                    }
                }
            }

            XShiftScreen = (ActualWidth - ScreenWidth) / 2;
            YShiftScreen = (ActualHeight - ScreenHeight) / 2;

            if (!(ddrval == DD_OK))
                ShowError ("IDirectDraw_SetDisplayMode failed");
            else
            {
                memset (&ddsd, 0, sizeof (DDSURFACEDESC));  // set non-used values to zero!
                ddsd.dwSize = sizeof (ddsd);
                if (FullScreen)
                {
                    ddsd.dwFlags = DDSD_CAPS | DDSD_BACKBUFFERCOUNT;
                    ddsd.ddsCaps.dwCaps = DDSCAPS_PRIMARYSURFACE |
                                          DDSCAPS_FLIP |
                                          DDSCAPS_COMPLEX |
                                          DDSCAPS_MODEX;
                }
                else
                {
                    ddsd.dwFlags = DDSD_CAPS;
                    ddsd.ddsCaps.dwCaps = DDSCAPS_PRIMARYSURFACE;
                }

                ddsd.dwBackBufferCount = 1;
                ddrval = IDirectDraw_CreateSurface (lpDD, &ddsd, &lpDDSFront, NULL);

                if (!(ddrval == DD_OK))
                    ShowError ("IDirectDraw_CreateSurface failed (lpDDSFront)");
                else
                {
                    ddrval = IDirectDraw_CreateClipper (lpDD, 0, &clipper, NULL);
                    if (!(ddrval == DD_OK))
                        ShowError ("IDirectDraw_CreateClipper failed");
                    else
                    {
                        ddrval = IDirectDrawClipper_SetHWnd (clipper, 0, ghGameWindow);
                        if (!(ddrval == DD_OK))
                            ShowError ("IDirectDrawClipper_SetHWnd failed");
                        else
                        {
                            ddrval = IDirectDrawSurface_SetClipper (lpDDSFront, clipper);
                            if (!(ddrval == DD_OK))
                                ShowError ("IDirectDrawClipper_SetClipper failed");
                            else
                            {
                                if (!FullScreen)
                                {

                                    memset (&ddsd, 0, sizeof (DDSURFACEDESC));
                                    ddsd.dwSize = sizeof (ddsd);
                                    ddsd.dwFlags = DDSD_CAPS |
                                                   DDSD_WIDTH |
                                                   DDSD_HEIGHT;

                                    ddsd.ddsCaps.dwCaps = DDSCAPS_OFFSCREENPLAIN | DDSCAPS_SYSTEMMEMORY;
                                    ddsd.dwWidth = ScreenWidth;
                                    ddsd.dwHeight = ScreenHeight;
                                    ddrval = IDirectDraw_CreateSurface (lpDD, &ddsd, &lpDDSBack, NULL);
                                    if (!(ddrval == DD_OK))
                                        ShowError ("IDirectDrawClipper_CreateSurface failed (lpDDSBack)");
                                    else
                                        result = TRUE;
                                }
                                else
                                {
                                    ddscaps.dwCaps = DDSCAPS_BACKBUFFER;
                                    ddrval = IDirectDrawSurface_GetAttachedSurface (lpDDSFront, &ddscaps, &lpDDSBack);
                                    if (!(ddrval == DD_OK))
                                        ShowError ("IDirectDrawSurface_GetAttachedSurface failed");
                                    else
                                    {
                                        if (BitsPerPixel == 8)
                                        {
                                           lpDDPal = DDLoadPalette (lpDD, NULL);
                                           if (lpDDPal == NULL)
                                               ShowError ("DDMakeDefaultPalette failed");
                                           else
                                               IDirectDrawSurface_SetPalette (lpDDSFront, lpDDPal);
                                        }

                                        result = TRUE;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    return result;
}

/* shut down the game window */
void OSDeInitGameWindow ()
{
    ReleaseDD ();
}

/* get game window handle */
BOOL OSGetGameWindowHDC (HDC *hdc)
{
   return (IDirectDrawSurface_GetDC (lpDDSBack, hdc) == DD_OK);
}

/* release game window handle */
void OSReleaseGameWindowHandle (HDC hdc)
{
   IDirectDrawSurface_ReleaseDC (lpDDSBack, hdc);
}

/* clear the (visual) screen */
void OSClearScreen ()
{
    DDBLTFX ddbltfx;

    memset (&ddbltfx, 0, sizeof (ddbltfx));
    ddbltfx.dwSize = sizeof (ddbltfx);
    ddbltfx.dwFillColor = DDColorMatch (lpDDSFront, 0);
    IDirectDrawSurface_Blt (lpDDSFront, NULL, NULL, NULL,
        DDBLT_COLORFILL | DDBLT_WAIT, &ddbltfx);
}

/* clear the virtual screen */
void OSClearVirtualScreen (COLORREF c)
{
    DDBLTFX ddbltfx;
    RECT dst;

    dst.left = XShiftScreen;
    dst.top = YShiftScreen;
    dst.right = ScreenWidth + XShiftScreen;
    dst.bottom = ScreenHeight + YShiftScreen;

    memset (&ddbltfx, 0, sizeof (ddbltfx));

    ddbltfx.dwSize = sizeof (ddbltfx);
    ddbltfx.dwFillColor = DDColorMatch (lpDDSBack, c);
    IDirectDrawSurface_Blt (lpDDSBack, &dst, NULL, NULL,
        DDBLT_COLORFILL | DDBLT_WAIT, &ddbltfx);
}

/* fill an area with black */
void OSFillBlack (BOOL vis, RECT dst)
{
    LPDIRECTDRAWSURFACE lpDDS = (vis)? lpDDSFront : lpDDSBack;
    DDBLTFX ddbltfx;

//    if (vis)
//    {
      dst.left += XShiftScreen;
      dst.top += YShiftScreen;
      dst.right += XShiftScreen;
      dst.bottom += YShiftScreen;
//    }

    memset (&ddbltfx, 0, sizeof (ddbltfx));
    ddbltfx.dwSize = sizeof (ddbltfx);
    ddbltfx.dwFillColor = DDColorMatch (lpDDS, 0);
    IDirectDrawSurface_Blt (lpDDS, &dst, NULL, NULL,
        DDBLT_COLORFILL | DDBLT_WAIT, &ddbltfx);
}

/* copy (part of) virtual screen to visual screen */
void OSBlit (RECT *r)
{
    HRESULT ddrval;
    DDBLTFX ddbltfx;
    int flags = DDBLT_WAIT + DDBLT_DDFX;
    RECT sr;

    sr.left = r->left + XShiftScreen;
    sr.top = r->top + YShiftScreen;
    sr.right = r->right + XShiftScreen;
    sr.bottom = r->bottom + YShiftScreen;

    memset (&ddbltfx, 0, sizeof (ddbltfx));
    ddbltfx.dwSize = sizeof (ddbltfx);
    ddbltfx.dwDDFX = DDBLTFX_NOTEARING;

    ddrval = IDirectDrawSurface_Blt
                (lpDDSFront, &sr, lpDDSBack, &sr, flags, &ddbltfx);
    if (ddrval == DDERR_SURFACELOST)
        DDRestoreAll ();
}

/* flip pages */
void OSFlip ()
{
    IDirectDrawSurface_Flip (lpDDSFront, NULL, DDFLIP_WAIT);
}


/* ------------------------- bitmap functions  ------------------------- */

/* get a pointer to the GAMEBITMAPINFO structure with id BID */
static GAMEBITMAPINFO *GetGameBitmapInfo (int BID)
{
    GAMEBITMAPINFO *gbip = gbipGameBitmapInfo;
    BOOL bFound = FALSE;

    if (BID == iPrevGBIP)
        gbip = gbipPrev;
    else
    {
        while (gbip && (!bFound))
        {
            if (gbip->iBitmapID == BID)
            {
                bFound = TRUE;
                iPrevGBIP = BID;
                gbipPrev = gbip;
            }
            else
                gbip = gbip->gbipNext;
        }
    }
    return gbip;
}

/* free members of a GAMEBITMAPINFO node */
static void FreeGameBitmapInfoNode (GAMEBITMAPINFO *gbip)
{
    GAMEBLOCKSEQUENCE *gbs;

    if (gbip->lpDDSBitmap)
    {
        IDirectDraw_Release (gbip->lpDDSBitmap);
        gbip->lpDDSBitmap = NULL;
    }

    if (gbip->sName)
    {
        rfree (gbip->sName);
        gbip->sName = NULL;
    }

    gbs = NULL;
    while (gbip->gbsGameBlockSequence)
    {
        gbs = gbip->gbsGameBlockSequence->gbsNext;
        if (gbip->gbsGameBlockSequence->sSequence)
        {
            rfree (gbip->gbsGameBlockSequence->sSequence);
        }
        rfree (gbip->gbsGameBlockSequence);
        gbip->gbsGameBlockSequence = gbs;
    }
}


/* initialize a game bitmap */
int OSInitGameBitmap (int id, char *name,
                      int bitmapwidth, int bitmapheight,
                      int blockwidth, int blockheight)
{
    HRESULT ddrval;
    DDSURFACEDESC ddsd;
    DDSCAPS ddscaps;
    int resultcode;
    GAMEBITMAPINFO *gbip1;
    GAMEBITMAPINFO *gbip2;
    LPDIRECTDRAWSURFACE lpDDS;

    iPrevGBIP = 0;
    gbipPrev = NULL;

    if (id==0)
    {
        /* find matching id or create new id */
        GAMEBITMAPINFO *gbip = gbipGameBitmapInfo;

        id = 1;
        while (gbip)
        {
            if (lstrcmp (gbip->sName, name) == 0)
            {
                resultcode = gbip->iBitmapID;
                return resultcode;
            }
            else
            {
                if (gbip->iBitmapID >= id)
                    id = gbip->iBitmapID + 1;
                gbip = gbip->gbipNext;
            }
        }
    }

    resultcode = GR_INVALID_BITMAP_ID;
    if (!GetGameBitmapInfo (id))  /* identifier not used yet */
    {
        /* set non-used values to zero! */
        memset (&ddsd, 0, sizeof (DDSURFACEDESC));
        ddsd.dwSize = sizeof (ddsd);
        ddsd.dwFlags = DDSD_CAPS |
                       DDSD_WIDTH |
                       DDSD_HEIGHT;

        /* must always be in systemmemory if size > screensize */
        ddsd.ddsCaps.dwCaps = DDSCAPS_OFFSCREENPLAIN; // | DDSCAPS_SYSTEMMEMORY;
        ddsd.dwWidth = bitmapwidth;
        ddsd.dwHeight = bitmapheight;
        ddrval = IDirectDraw_CreateSurface (lpDD, &ddsd, &lpDDS, NULL);

        resultcode = GR_NOT_FOUND;
        lpDDPal = DDLoadPalette (lpDD, name);
        if (lpDDPal)
        {
            ddrval = DDReLoadBitmap (lpDDS, name);
            if (ddrval == DD_OK)
            {
                resultcode = GR_OS_ERROR;
                if (ddrval == DD_OK)
                {
                    /* find last element of linked list */
                    gbip1 = gbipGameBitmapInfo;
                    gbip2 = NULL;
                    while (gbip1)
                    {
                        gbip2 = gbip1;
                        gbip1 = gbip1->gbipNext;
                    }

                    /* create new node */
                    gbip1 = rmalloc (sizeof (GAMEBITMAPINFO));
                    gbip1->iBitmapID = id;
                    gbip1->lpDDSBitmap = lpDDS;
                    gbip1->bTransparent = FALSE;    /* transparency can be set later */
                    gbip1->iBitmapWidth = bitmapwidth;
                    gbip1->iBitmapHeight = bitmapheight;
                    gbip1->iBlockWidth = blockwidth;
                    gbip1->iBlockHeight = blockheight;
                    gbip1->iBlockCountX = bitmapwidth / blockwidth;
                    gbip1->iBlockCountY = bitmapheight / blockheight;
                    gbip1->gbsGameBlockSequence = NULL;
                    gbip1->gbipNext = NULL;
                    gbip1->sName = rmalloc (strlen (name) + 1);
                    rsncopy (gbip1->sName, name, strlen (name));

                    /* gbip2 points to the last element or is NULL */
                    if (gbip2)
                        gbip2->gbipNext = gbip1;
                    else
                        gbipGameBitmapInfo = gbip1;  /* first element */

                    resultcode = GR_OK;
                }
            }
        }
    }
    if (resultcode == GR_OK)
        return id;
    else
        return resultcode;
}

/* get bitmap info */
BOOL OSGetGameBitmapInfo (int id, int *width, int *height,
                          int *blockwidth, int *blockheight,
                          int *blockcountx, int *blockcounty)
{
    GAMEBITMAPINFO *gbip;

    gbip = GetGameBitmapInfo (id);
    if (gbip)
    {
        *width = gbip->iBitmapWidth;
        *height = gbip->iBitmapHeight;
        *blockwidth = gbip->iBlockWidth;
        *blockheight = gbip->iBlockHeight;
        *blockcountx = gbip->iBlockCountX;
        *blockcounty = gbip->iBlockCountY;
        return TRUE;
    }
    else
        return FALSE;
}


/* deinit a game bitmap */
int OSFreeGameBitmap (int id)
{
    GAMEBITMAPINFO *gbipCurrent = gbipGameBitmapInfo;
    GAMEBITMAPINFO *gbipNext = NULL;
    GAMEBITMAPINFO *gbipPrevious = NULL;
    int result;

    iPrevGBIP = 0;
    gbipPrev = NULL;

    result = GR_INVALID_BITMAP_ID;
    while (gbipCurrent)
    {
        if (gbipCurrent->iBitmapID != id)
        {
            gbipPrevious = gbipCurrent;
            gbipNext = gbipCurrent->gbipNext;
        }
        else
        {
            /* link previous node to next node */
            if (gbipPrevious)
                gbipPrevious->gbipNext = gbipCurrent->gbipNext;
            else
                gbipGameBitmapInfo = gbipCurrent->gbipNext;

            gbipNext = gbipCurrent->gbipNext;
            /* free the current node */

            FreeGameBitmapInfoNode (gbipCurrent);
            rfree (gbipCurrent);

            result = GR_OK;
        }
        gbipCurrent = gbipNext;
    }
    return result;
}

/* deinit all game bitmaps */
void OSFreeGameBitmaps ()
{
    GAMEBITMAPINFO *gbip;

    while (gbipGameBitmapInfo)
    {
        gbip = gbipGameBitmapInfo->gbipNext;
        FreeGameBitmapInfoNode (gbipGameBitmapInfo);
        rfree (gbipGameBitmapInfo);
        gbipGameBitmapInfo = gbip;
    }
    iPrevGBIP = 0;
    gbipPrev = NULL;
}


/* set transparent color */
int OSSetTransparentColor (int id, int x, int y)
{
    HDC hdc;
    COLORREF rgb;
    int result;
    GAMEBITMAPINFO *gbip = GetGameBitmapInfo (id);

    result = GR_INVALID_BITMAP_ID;
    if (gbip)
    {
        if (x < 0) x += gbip->iBitmapWidth;
        if (y < 0) y += gbip->iBitmapHeight;

        result = GR_OS_ERROR;
        if (IDirectDrawSurface_GetDC (gbip->lpDDSBitmap, &hdc) == DD_OK)
        {
            rgb = GetPixel (hdc, x, y);
            IDirectDrawSurface_ReleaseDC (gbip->lpDDSBitmap, hdc);

            DDSetColorKey (gbip->lpDDSBitmap, rgb);
            gbip->bTransparent = TRUE;

            // DDSetColorKey (gbip->lpDDSFront, rgb);
            result = GR_OK;
        }
    }
    return result;
}

/* initialize a block sequence */
int OSInitBlockSequence (int bitmapid, int seqid, char *seq, int len)
{
    GAMEBITMAPINFO *gbip = GetGameBitmapInfo (bitmapid);
    GAMEBLOCKSEQUENCE *gbsNew;
    GAMEBLOCKSEQUENCE *gbs1;
    GAMEBLOCKSEQUENCE *gbs2;
    int resultcode;

    resultcode = GR_INVALID_BITMAP_ID;
    if (gbip)
    {
        gbsNew = rmalloc (sizeof (GAMEBLOCKSEQUENCE));
        gbsNew->iSequenceID = seqid;
        gbsNew->iSequenceLength = len / (2 * sizeof (int));
        gbsNew->sSequence = rmalloc (len + 1);
        rsncopy (gbsNew->sSequence, seq, len);
        gbsNew->iPosition = gbsNew->iSequenceLength;
        gbsNew->iCounter = 0;
        gbsNew->gbsNext = NULL;

        gbs1 = gbip->gbsGameBlockSequence;
        gbs2 = NULL;
        while (gbs1)
        {
            gbs2 = gbs1;
            gbs1 = gbs1->gbsNext;
        }
        if (gbs2)
            gbs2->gbsNext = gbsNew;
        else
            gbip->gbsGameBlockSequence = gbsNew;
        resultcode = GR_OK;
    }
    return resultcode;
}

/* run block sequences */
void OSRunBlockSequences ()
{
    GAMEBITMAPINFO *gbip;
    GAMEBLOCKSEQUENCE *gbs;
    int i;

    gbip = gbipGameBitmapInfo;
    while (gbip)
    {
        gbs = gbip->gbsGameBlockSequence;
        while (gbs)
        {
            if (--gbs->iCounter <= 0)
            {
                gbs->iPosition++;
                if (gbs->iPosition >= gbs->iSequenceLength)
                    gbs->iPosition = 0;
                i = (2 * gbs->iPosition * sizeof (int)) + sizeof (int);
                gbs->iCounter = (*(int *) &gbs->sSequence[i]);
            }
            gbs = gbs->gbsNext;
        }
        gbip = gbip->gbipNext;
    }
}

/* get current block */
int OSGetCurrentBlock (int bitmapid, int seqid)
{
    GAMEBITMAPINFO *gbip;
    GAMEBLOCKSEQUENCE *gbs;
    int result = 0;

    gbip = GetGameBitmapInfo (bitmapid);
    gbs = gbip->gbsGameBlockSequence;
    while ((gbs) && (seqid < 0))
    {
        if (gbs->iSequenceID == seqid)
        {
            int mappos;

            mappos = 2 * sizeof (int) * gbs->iPosition;
            result = (*(int *) &gbs->sSequence[mappos]);
        }
        gbs = gbs->gbsNext;
    }
    return result;
}

/* draw part of a bitmap to virtual screen */
void OSDraw (RECT *dst, int id, RECT *src, BOOL mirlr, BOOL mirud, int flags)
{
    HRESULT ddrval;
    DDBLTFX ddbltfx;
    int bltflags, bltfastflags;
    BOOL fast;
    static int i;
    GAMEBITMAPINFO *gbip = GetGameBitmapInfo (id);
    RECT r;

    r.left = dst->left + XShiftScreen;
    r.top = dst->top + YShiftScreen;
    r.right = dst->right + XShiftScreen;
    r.bottom = dst->bottom + YShiftScreen;

    if (gbip)
    {
        fast = TRUE;

        bltfastflags = DDBLTFAST_WAIT;
        bltflags = DDBLT_WAIT | DDBLT_DDFX;

        if (gbip->bTransparent)
        {
            bltflags |= DDBLT_KEYSRC;
            bltfastflags |= DDBLTFAST_SRCCOLORKEY;
        }

      //  ddbltfx.dwDDFX = 0;
        memset (&ddbltfx, 0, sizeof (ddbltfx));
        ddbltfx.dwSize = sizeof (ddbltfx);

        if (mirlr)
        {
            ddbltfx.dwDDFX |= DDBLTFX_MIRRORLEFTRIGHT;
            fast = FALSE;
        }
        if (mirud)
        {
            ddbltfx.dwDDFX |= DDBLTFX_MIRRORUPDOWN;
            fast = FALSE;
        }

        if (flags & DO_ROTATE_90)
        {
            ddbltfx.dwDDFX |= DDBLTFX_ROTATE90;
            fast = FALSE;
        }
        if (flags & DO_ROTATE_180)
        {
            ddbltfx.dwDDFX |= DDBLTFX_ROTATE180;
            fast = FALSE;
        }
        if (flags & DO_ROTATE_270)
        {
            ddbltfx.dwDDFX |= DDBLTFX_ROTATE270;
            fast = FALSE;
        }

        if (fast)
            ddrval = IDirectDrawSurface_BltFast (lpDDSBack,
                         r.left, r.top, gbip->lpDDSBitmap, src,
                         bltfastflags);
        else
            ddrval = IDirectDrawSurface_Blt (lpDDSBack, &r,
                         gbip->lpDDSBitmap, src, bltflags, &ddbltfx);
        if (ddrval == DDERR_SURFACELOST)
            DDRestoreAll ();
    }
}

/* ------------------------- sound functions ------------------------- */

typedef struct SOUNDSAMPLEINFO
{
    int iSampleID;
    HSNDOBJ hsoSoundBuffer;
    struct SOUNDSAMPLEINFO *ssiNext;
} SOUNDSAMPLEINFO;

static BOOL bSoundEnabled = FALSE;
static LPDIRECTSOUND lpDS = NULL;
static SOUNDSAMPLEINFO *ssiSoundSampleInfo = NULL;


/* get a pointer to the SOUNDSAMPLEINFO structure with id ID */
SOUNDSAMPLEINFO *GetSoundSampleInfo (int ID)
{
    SOUNDSAMPLEINFO *ssi = ssiSoundSampleInfo;
    BOOL bFound = FALSE;

    while (ssi && (!bFound))
    {
        if (ssi->iSampleID == ID)
            bFound = TRUE;
        else
            ssi = ssi->ssiNext;
    }
    return ssi;
}

BOOL OSInitSound ()
{
    bSoundEnabled = FALSE;
    if (!(DirectSoundCreate (NULL, &lpDS, NULL) == DS_OK))
    {
       // MessageBox (NULL, "DirectSoundCreate failed", NULL, MB_OK);
    }
    else
    {
        if (!(IDirectSound_SetCooperativeLevel (lpDS, ghGameWindow,
                 DSSCL_NORMAL) == DS_OK))
        {
           // MessageBox (NULL, "IDirectSound_SetCooperativeLevel failed", NULL, MB_OK);
        }
        else
        {
            bSoundEnabled = TRUE;
        }
    }
    return bSoundEnabled;
}

/* deinitialize sound */
void OSDeInitSound ()
{
    if (lpDS != NULL)
    {
        IDirectSound_Release (lpDS);
        lpDS = NULL;
    }
    bSoundEnabled = FALSE;
}

/* initialize a sound sample */
BOOL OSInitSoundSample (int id, char *name, int buffers)
{
    SOUNDSAMPLEINFO *ssi;

    if (bSoundEnabled)
    {
        ssi = rmalloc (sizeof (SOUNDSAMPLEINFO));
        ssi->iSampleID = id;
        ssi->hsoSoundBuffer = SndObjCreate (lpDS, name, buffers);
        ssi->ssiNext = ssiSoundSampleInfo;
        ssiSoundSampleInfo = ssi;
        return TRUE;
    }
    else
        return FALSE;
}

/* free the SOUNDSAMPLEINFO list */
void OSFreeSoundSamples ()
{
    SOUNDSAMPLEINFO *ssi;

    while (ssiSoundSampleInfo)
    {
        ssi = ssiSoundSampleInfo->ssiNext;
        if (ssiSoundSampleInfo->hsoSoundBuffer)
        {
            SndObjDestroy (ssiSoundSampleInfo->hsoSoundBuffer);
            ssiSoundSampleInfo->hsoSoundBuffer = NULL;
        }

        rfree (ssiSoundSampleInfo);
        ssiSoundSampleInfo = ssi;
    }
}

/* play a sound sample */
BOOL OSPlaySoundSample (int id, int volume, int pan, int freq)
{
    SOUNDSAMPLEINFO *ssi = GetSoundSampleInfo (id);

    if (bSoundEnabled)
    {
        if (ssi)
        {
            IDirectSoundBuffer *pDSB = SndObjGetFreeBuffer (ssi->hsoSoundBuffer);

            if (pDSB)
            {
                IDirectSoundBuffer_SetVolume (pDSB, (LONG) volume);
                IDirectSoundBuffer_SetPan (pDSB, (LONG) pan);
                IDirectSoundBuffer_SetFrequency (pDSB, (LONG) freq);
                IDirectSoundBuffer_Play (pDSB, 0, 0, 0);
            }
        }
    }
    return bSoundEnabled;
}

/* start playing music in the background */
BOOL OSPlayMusic (char *midifile, BOOL restart)
{
    char buf[256];
    BOOL result = FALSE;

    if (midifile)
    {
        wsprintf (buf, "open %s type sequencer alias MUSIC", midifile);
        mciSendString ("close all", NULL, 0, NULL);

        if (mciSendString (buf, NULL, 0, NULL) == 0)
        {
            if (restart)
            {
                if (mciSendString ("play MUSIC from 0 notify", NULL, 0, ghGameWindow) == 0)
                    result = TRUE;
            }
            else
            {
                if (mciSendString ("play MUSIC from 0", NULL, 0, ghGameWindow) == 0)
                    result = TRUE;
            }
        }
    }
    else
        if (mciSendString ("play MUSIC from 0 notify", NULL, 0, ghGameWindow) == 0)
            result = TRUE;

    return result;
}

/* stop music */
BOOL OSStopMusic ()
{
    BOOL result = FALSE;
    if (mciSendString ("close all", NULL, 0, NULL) == 0)
        result = TRUE;
    return result;
}

