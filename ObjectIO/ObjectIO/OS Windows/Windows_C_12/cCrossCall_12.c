#include "cCrossCall_12.h"
#include "cpicture_12.h"

/* Mike... */
#include "cGameLib_12.h"
/* ...Mike */

// MW...
#include "cprinter_12.h"
#include "cTCP.h"
#include "winsock.h"

#ifdef HILT
#include <htmlhelp.h>
int		htmlHelpInitialized = FALSE;
DWORD	htmlHelpCookie;
#endif

#include <commctrl.h>
extern BOOL bUserAbort;
extern HWND   hDlgPrint;						/* MW: hDlgPrint is the handle of the "Cancel Printing" dialog. */
extern HWND   hwndText;							/* MW: hwndText  is the handle of the page count text in the dialog. */
DNSInfo		*DNSInfoList=NULL;
// ...MW
CrossCallInfo gCci;
static BOOL gEventsInited = FALSE;
static BOOL gAppRunning = FALSE;
/* Mike... */
char *gAppName;									/* Mike: should not be static because refered to elsewhere. */
/* ...Mike */
static HANDLE gCLEAN_DONE;
static HANDLE gOS_DONE;
static DWORD gCleanThreadId;
static DWORD gOsThreadId;
static HANDLE ghOSThreadHandle = NULL;
HINSTANCE ghInst;
HWND ghMainWindow = NULL;
/* RWS ... */
static HWND gNextClipboardViewer = NULL;		/* PA: name was not used anymore. Now using it again for storing next clipboardviewer. */
/* ... RWS */
static int gClipboardCount = 0;					/* PA: keeps track of changes of clipboard. */
static HACCEL gAcceleratorTable = NULL;			/* Refers to the accelerator table of the active frame. */
static BOOL gAcceleratorTableIsUpToDate	= TRUE;
static PAINTSTRUCT gPaintStruct;
static HWND ghTopDocWindow = NULL;
static HWND ghActiveFrameWindow  = NULL;		/* The currently active frame window (MDI/SDI). */
static HWND ghActiveClientWindow = NULL;		/* The currently active client window (MDI). */
static HWND gActiveDialog = NULL;
static HWND ghwndLastModalDialog = NULL;		/* Keeps track of last modal dialog. */
static BOOL gPopupIsOpen = FALSE;				/* Only one PopupMenu can be open at a time. */
static BOOL gInMouseDown = FALSE;
static HCURSOR ghFatCrossCursor = NULL;
static HCURSOR ghHiddenCursor = NULL;
static int gProhibitWindowActivation = 0;
static int gDoubleDownDistance = -1;
static int gGlobalCursorCode = -1;
static int gNextItemHandle = 0;					/* PA: this global is now used only for timers; improve name. */
static int gClicks = 0;
static UINT gClTime;
static int gClX, gClY;
static HWND gClHwnd;
static HFONT gDlogFont = NULL;
static HFONT gWinFont  = NULL;					/* The font used for controls in windows. */
static int gComboSelection = -1;
static BOOL gInKey = FALSE;
static int gCurChar;
static LONG gPrevMsgTime = -1;
static BOOL gMessStored = FALSE;
static MSG gStoredMess;
static BOOL gIdleTimerOn = FALSE;
static BOOL gSleeptime = 0;						/* The sleep time required by Clean timers/receivers. */
static LONG stdEditCallback = 0;				/* The standard internal Windows callback routine of edit controls. */
static LONG stdPopUpCallback = 0;				/* The standard internal Windows callback routine of pop up controls. */
static LONG stdMDIClientCallback = 0;			/* The standard internal Windows callback routine of MDI client windows. */
static HFONT gControlFont;						/* The handle to the logical FONT that is used in all controls. */
static HWND ghwndTT = NULL;						/* The tooltip control. */

static CrossCallInfo *MakeQuitCci (CrossCallInfo * pcci);
static CrossCallInfo *MakeReturn0Cci (CrossCallInfo * pcci);
static CrossCallInfo *MakeReturn1Cci (CrossCallInfo * pcci, int v);
static CrossCallInfo *MakeReturn2Cci (CrossCallInfo * pcci, int v1, int v2);
static CrossCallInfo *MakeReturn3Cci (CrossCallInfo * pcci, int v1, int v2, int v3);
static CrossCallInfo *MakeReturn4Cci (CrossCallInfo * pcci, int v1, int v2, int v3, int v4);
static CrossCallInfo *MakeReturn5Cci (CrossCallInfo * pcci, int v1, int v2, int v3, int v4, int v5);
static CrossCallInfo *MakeReturn6Cci (CrossCallInfo * pcci, int v1, int v2, int v3, int v4, int v5, int v6);

/*	Menu(item)IDs are not allowed to exceed OSMenuIDEnd.
	This is because window ids start at (OSMenuIDEnd+5), and need to be distinct from menu ids
	in case of MDI processes.
	The global gMenuItemID (initially 0) is incremented by NextMenuItemID each time a new 
	menu(item)ID is required.
	This implementation does not reuse freed ids and is therefore not adequate!!
*/
#define OSMenuIDEnd		10000
static int gMenuItemID = 0;

static UINT
NextMenuItemID (void)
{
	if (gMenuItemID>=OSMenuIDEnd)
		ErrorExit ("NextMenuItemID exceeded number of internal menu(item)IDs: %d",OSMenuIDEnd);

	gMenuItemID++;

	return gMenuItemID;
}

/*	Implementation of accelerator tables.
	Now for each MDI and SDI frame window, a private accelerator table is stored.
	NDI processes have no frame window, and also don't need an accelerator table.
	
	A global AcceleratorTable is maintained, gAcceleratorTable. It contains the 
	HACCEL of the current frame window. 
	A global BOOL is maintained, gAcceleratorTableIsUpToDate. It is TRUE if
	gAcceleratorTable represents the correct acceleratortable. It is FALSE if not.

	For each accelerator key four entries are stored in the acceleratortable:
		Ctrl          +key
		Ctrl+Shift    +key
		Ctrl      +Alt+key
		Ctrl+Shift+Alt+key
	For this reason a minimum size of 4 (MINSIZEPROCESSSHORTCUTTABLE) has been set. 
	Deflating the table never decreases below this value. 
*/
struct ProcessShortcutTable
{
	int pst_size;			// The current size of the table
	int pst_used;			// The current number of filled items
	ACCEL *pst_shortcuts;	// The current table, implemented as a pointer to ACCELs
};

typedef struct ProcessShortcutTable *ProcessShortcutTable;

#define MINSIZEPROCESSSHORTCUTTABLE 4	// The minimum size of an accelerator table is 4

static void
CopyACCEL (ACCEL *source, ACCEL *dest)
{
	dest->fVirt = source->fVirt;
	dest->key   = source->key;
	dest->cmd   = source->cmd;
}

/*	AllocateProcessShortcutTable (size) creates a shortcut table of the given size. 
*/
static ProcessShortcutTable
AllocateProcessShortcutTable (int size)
{
	ProcessShortcutTable table;
	ACCEL *shortcuts;

	table = (ProcessShortcutTable) rmalloc (sizeof (struct ProcessShortcutTable));
	shortcuts = (ACCEL *) rmalloc (size * sizeof (ACCEL));

	table->pst_size = size;
	table->pst_used = 0;
	table->pst_shortcuts = shortcuts;

	return (table);
}

/*	DestroyProcessShortcutTable (table) frees the memory used by the table. 
*/
static void
DestroyProcessShortcutTable (ProcessShortcutTable table)
{
	rfree (table->pst_shortcuts);
	rfree (table);
}

/*	InflateProcessShortcutTable (table) returns a new table double the size of the argument table. 
*/
static ProcessShortcutTable
InflateProcessShortcutTable (ProcessShortcutTable oldTable)
{
	int i, oldSize, newSize;
	ProcessShortcutTable newTable;

	oldSize = oldTable->pst_size;
	newSize = oldSize*2;

	newTable = (ProcessShortcutTable) AllocateProcessShortcutTable (newSize);

	for (i=0; i<oldSize; i++)
		CopyACCEL (&oldTable->pst_shortcuts[i], &newTable->pst_shortcuts[i]);

	newTable->pst_used = oldTable->pst_used;

	rfree (oldTable);
	return (newTable);
}

/*	DeflateProcessShortcutTable (table) returns a new table half the size of the argument table. 
	In case the table already is at its minimum size (MINSIZEPROCESSSHORTCUTTABLE) the argument
	table is returned.
*/
static ProcessShortcutTable
DeflateProcessShortcutTable (ProcessShortcutTable oldTable)
{
	int i, oldSize, newSize;
	ProcessShortcutTable newTable;

	oldSize = oldTable->pst_size;
	newSize = oldSize/2;

	if (newSize < MINSIZEPROCESSSHORTCUTTABLE)
	{
		return (oldTable);
	}
	else
	{
		newTable = (ProcessShortcutTable) AllocateProcessShortcutTable (newSize);

		for (i=0; i<newSize; i++)
			CopyACCEL (&oldTable->pst_shortcuts[i], &newTable->pst_shortcuts[i]);
		
		newTable->pst_used = min (oldTable->pst_used, newSize);

		rfree (oldTable);
		return (newTable);
	}
}

static void 
SetACCELentry (ACCEL *entry, BYTE flags, int key, int id)
{
	entry->fVirt = flags;
	entry->key   = (WORD) ((BYTE) VkKeyScan ((char) key));
	entry->cmd   = (WORD) id;
}

/*	AddProcessShortcut (key,id,table) returns a new table in which the shortkey (id,key) has been added.
	The new table may have been inflated to accomodate the new shortkey.
	For each shortkey the following four accelerator flags are entered as separate entries:
		FVIRTKEY | FCONTROL
		FVIRTKEY | FCONTROL | FSHIFT
		FVIRTKEY | FCONTROL          | FALT
		FVIRTKEY | FCONTROL | FSHIFT | FALT
*/
static ProcessShortcutTable
AddProcessShortcut (int key, int id, ProcessShortcutTable oldTable)
{
	ACCEL *newentry;
	ProcessShortcutTable newTable;
	int used = oldTable->pst_used;

	// first make sure that the table has the proper size.
	if (used+4 >= oldTable->pst_size)
	{
		newTable = InflateProcessShortcutTable (oldTable);
	}
	else
	{
		newTable = oldTable;
	}

	// Add Ctrl+key:
	newentry = &newTable->pst_shortcuts[used];
	SetACCELentry (newentry, (BYTE) FCONTROL | FVIRTKEY, key, id);
	// Add Ctrl+Shift+key:
	newentry = &newTable->pst_shortcuts[used+1];
	SetACCELentry (newentry, (BYTE) FCONTROL | FSHIFT | FVIRTKEY, key, id);
	// Add Ctrl+Alt+key:
	newentry = &newTable->pst_shortcuts[used+2];
	SetACCELentry (newentry, (BYTE) FCONTROL | FALT | FVIRTKEY, key, id);
	// Add Ctrl+Shift+Alt+key:
	newentry = &newTable->pst_shortcuts[used+3];
	SetACCELentry (newentry, (BYTE) FCONTROL |FSHIFT | FALT | FVIRTKEY, key, id);

	newTable->pst_used = used+4;

	return(newTable);
}

/*	RemoveProcessShortcut (id,table) returns a new table in which the shortkey (id,_) has been removed.
	The new table may have been deflated to use up less memory space.
	In case the entry (id,_) has been located, the consecutive 3 entries are also removed as these
	encode the accelerator flag versions.
*/
static ProcessShortcutTable
RemoveProcessShortcut (int id, ProcessShortcutTable oldTable)
{
	int i;
	int foundat = 0;
	int used    = oldTable->pst_used;

	// search for the element to be deleted.
	while (foundat<used && oldTable->pst_shortcuts[foundat].cmd != id)
	{
		foundat++;
	}

	if (foundat>=used)
	{
		return (oldTable);			// the element was not found, so return argument table
	}
	
	used -= 4;
	for (i=foundat; i<used; i++)	// otherwise shift remaining entries to the left
		CopyACCEL (&oldTable->pst_shortcuts[i+4], &oldTable->pst_shortcuts[i]);
	oldTable->pst_used = used;
	
	if (used < oldTable->pst_size/2 && used > MINSIZEPROCESSSHORTCUTTABLE)	// check if table needs to be deflated
	{
		return (DeflateProcessShortcutTable (oldTable));
	}
	else
	{
		return (oldTable);
	}
}

static void
UpdateAcceleratorTable (void)
{
	ProcessShortcutTable table;

	if (!gAcceleratorTableIsUpToDate)		// The accelerator table is not up to date
	{
		if (gAcceleratorTable!=NULL)		// Destroy the previous version if exists
		{
			DestroyAcceleratorTable (gAcceleratorTable);
		}
		gAcceleratorTable = NULL;

		if (ghActiveFrameWindow!=NULL)		// The current frame is an (MDI/SDI) frame
		{
			table = (ProcessShortcutTable) GetWindowLong (ghActiveFrameWindow, 0);
			if (table!=NULL)
			{
				gAcceleratorTable = CreateAcceleratorTable (table->pst_shortcuts,table->pst_used);
			}
		}
	}
}
/*	End of implementation of accelerator tables.
*/

/*	Local window data structures:
*/
struct LocalWindowData
{
	int  lwd_cursorcode;		/* The cursor shape of the window */
	BOOL lwd_usersizemoving;	/* The user is sizing/moving the window */
};

typedef struct LocalWindowData *LocalWindowData;

static LocalWindowData
AllocateLocalWindowData ()
{
	LocalWindowData lwd_wdata;

	lwd_wdata = (LocalWindowData) rmalloc (sizeof (struct LocalWindowData));
	lwd_wdata->lwd_cursorcode     = 0;
	lwd_wdata->lwd_usersizemoving = (BOOL)FALSE;

	return (lwd_wdata);
}

/*	DestroyLocalWindowData (wdata) frees the memory used by the wdata. 
*/
static void
DestroyLocalWindowData (LocalWindowData wdata)
{
	rfree (wdata);
}


extern EXPORT_TO_CLEAN int
WinBeep (int ios)
{
	MessageBeep (MB_ICONASTERISK);
	return ios;
}

extern EXPORT_TO_CLEAN void
WinGetTime (int ios, int * hour, int * minute, int * second, int * oos)
{
	SYSTEMTIME time;
	GetLocalTime (&time);
	*hour   = time.wHour;
	*minute = time.wMinute;
	*second = time.wSecond;

	*oos    = ios;
}

extern EXPORT_TO_CLEAN void
WinGetDate (int ios, int * year, int * month, int * day, int * weekday, int * oos)
{
	SYSTEMTIME time;
	GetLocalTime (&time);
	*year    = time.wYear;
	*month   = time.wMonth;
	*day     = time.wDay;
	*weekday = time.wDayOfWeek + 1;

	*oos = ios;
}

extern EXPORT_TO_CLEAN int
WinWait (int delay, int ios)
{
	Sleep (delay);
	return ios;
}

extern EXPORT_TO_CLEAN void
WinGetBlinkTime (int ios, int* blinktime, int* oos)
{
	*blinktime = (int) GetCaretBlinkTime ();
	*oos       = ios;
}

/* PA: The following four routines used to be crosscalls. */
extern EXPORT_TO_CLEAN int
WinInvalidateWindow (int hwnd, int ios)
{
	InvalidateRect ((HWND) hwnd, NULL, FALSE);
	return ios;
}

extern EXPORT_TO_CLEAN int
WinInvalidateRect (int hwnd, int left, int top, int right, int bottom, int ios)
{
	RECT rect;

	rect.left   = left;
	rect.top    = top;
	rect.right  = right;
	rect.bottom = bottom;
	InvalidateRect ((HWND) hwnd, &rect, FALSE);
	return ios;
}

extern EXPORT_TO_CLEAN int
WinValidateRect (int hwnd, int left, int top, int right, int bottom, int ios)
{
	RECT rect;

	rect.left   = left;
	rect.top    = top;
	rect.right  = right;
	rect.bottom = bottom;
	ValidateRect ((HWND) hwnd, &rect);
	return ios;
}

extern EXPORT_TO_CLEAN int
WinValidateRgn (int hwnd, int rgn, int ios)
{
	ValidateRgn ((HWND) hwnd, (HRGN) rgn);
	return ios;
}
/* PA: Up to here. */

static void
InitGlobals (void)
{
	LOGFONT lf;
/* Mike... */
    InitGameGlobals ();
/* ...Mike */

	gAppRunning = FALSE;
	ghOSThreadHandle = NULL;
	ghMainWindow = NULL;
/* RWS ... */
	gNextClipboardViewer = NULL;
/* ... RWS */
	gAcceleratorTable = NULL;
	ghTopDocWindow = NULL;
	ghActiveFrameWindow  = NULL;
	ghActiveClientWindow = NULL;
	gActiveDialog = NULL;
	ghwndLastModalDialog = NULL;
	gPopupIsOpen = FALSE;
	gInMouseDown = FALSE;
	ghFatCrossCursor = NULL;
	ghHiddenCursor = NULL;
	gProhibitWindowActivation = 0;
	gDoubleDownDistance = -1;
	gGlobalCursorCode = -1;
	gNextItemHandle = 0;
	gClicks = 0;
	gDlogFont = NULL;
	gWinFont  = NULL;
	gComboSelection = -1;
	gInKey = FALSE;
	gPrevMsgTime = -1;
	gMessStored = FALSE;
	gIdleTimerOn = FALSE;
	gSleeptime = 0;
	
	// Globally, we create a logical font that is used in all controls. 
	SetLogFontData (&lf, "MS Sans Serif", 0, 8);
	gControlFont = CreateFontIndirect (&lf);
}

static char CustomControlClassName[]   = "__CleanCustomControl";
static char CompoundControlClassName[] = "__CleanCompoundControl";	/* Class for CompoundControls */
static char MainWindowClassName[] = "__CleanMainWindow";
static char SDIWindowClassName[]  = "__CleanSDIWindow";	/* Class for SDI windows (must have same length as MDIWindowClassName). */
static char SDIFrameClassName[]   = "__CleanSDIFrame";	/* Class for SDI frames.  */
static char MDIWindowClassName[]  = "__CleanMDIWindow";	/* Class for MDI windows (must have same length as SDIWindowClassName). */
static char MDIFrameClassName[]   = "__CleanMDIFrame";	/* Class for MDI frames.  */

int
nCopyAnsiToWideChar (LPWORD lpWCStr, LPSTR lpAnsiIn)
{
	int nChar = 0;

	do
	{
		*lpWCStr++ = (WORD) * lpAnsiIn;
		nChar++;
	} while (*lpAnsiIn++);

	return nChar;
}

static void
GetAppFileName (void)
{
	char path[MAX_PATH + 1];
	int length, index;
	int start, end;
	BOOL newword;

	length = GetModuleFileName (NULL, path, MAX_PATH);

	for (index = length - 1; path[index] != '.'; index--)
		;
	end = index - 1;

	for (index = end;
		 path[index] != '/' &&
		 path[index] != '\\' &&
		 path[index] != ':';
		 index--)
		;

	start = index + 1;

	if (end - start > 31)
		end = start + 31;

	if (end - start >= 0)
	{
		gAppName = rmalloc (end - start + 2);
		for (index = 0; index <= end - start; index++)
			gAppName[index] = path[start + index];
		gAppName[index] = '\0';
	}
	else
	{
		gAppName = "Clean Application";
	}

	newword = TRUE;
	for (index = 0; gAppName[index] != '\0'; index++)
	{
		if (gAppName[index] >= 'A' && gAppName[index] <= 'Z' && !newword)
			gAppName[index] = gAppName[index] - ('A' - 'a');

		if (gAppName[index] == ' ')
			newword = TRUE;
		else
			newword = FALSE;
	}
}

extern EXPORT_TO_CLEAN PSTR
WinGetAppPath (void)
{
	char *path;
	int idx, length;

	path = rmalloc (261);

	length = GetModuleFileName (NULL, path, 260);

	for (idx = length - 1;
		 path[idx] != '/' &&
		 path[idx] != '\\' &&
		 path[idx] != ':';
		 idx--)
		;

	path[idx + 1] = 0;

	return path;	/* relying on the calling clean function to de-allocate path. */
}

/*	Cursor handling routines. 
*/
static BYTE FatCrossCursorANDmask[128] = {
	0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
	0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
	0xff, 0x80, 0xff, 0xff, 0xff, 0x80, 0x7f, 0xff,
	0xff, 0x80, 0x7f, 0xff, 0xff, 0x80, 0x7f, 0xff,

	0xff, 0x80, 0x7f, 0xff, 0xff, 0x80, 0x7f, 0xff,
	0xe0, 0x00, 0x03, 0xff, 0xe0, 0x3e, 0x01, 0xff,
	0xe0, 0x3e, 0x01, 0xff, 0xe0, 0x3e, 0x01, 0xff,
	0xe0, 0x3e, 0x01, 0xff, 0xe0, 0x3e, 0x01, 0xff,

	0xe0, 0x00, 0x01, 0xff, 0xf0, 0x00, 0x01, 0xff,
	0xff, 0x80, 0x7f, 0xff, 0xff, 0x80, 0x7f, 0xff,
	0xff, 0x80, 0x7f, 0xff, 0xff, 0x80, 0x7f, 0xff,
	0xff, 0x80, 0x7f, 0xff, 0xff, 0xc0, 0x7f, 0xff,

	0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
	0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
	0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
	0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff
};

static BYTE FatCrossCursorXORmask[128] = {
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x3e, 0x00, 0x00,
	0x00, 0x3e, 0x00, 0x00, 0x00, 0x3e, 0x00, 0x00,

	0x00, 0x3e, 0x00, 0x00, 0x00, 0x3e, 0x00, 0x00,
	0x00, 0x3e, 0x00, 0x00, 0x0f, 0xff, 0xf8, 0x00,
	0x0f, 0xe3, 0xf8, 0x00, 0x0f, 0xe3, 0xf8, 0x00,
	0x0f, 0xe3, 0xf8, 0x00, 0x0f, 0xff, 0xf8, 0x00,

	0x00, 0x3e, 0x00, 0x00, 0x00, 0x3e, 0x00, 0x00,
	0x00, 0x3e, 0x00, 0x00, 0x00, 0x3e, 0x00, 0x00,
	0x00, 0x3e, 0x00, 0x00, 0x00, 0x3e, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,

	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
};

static BYTE HiddenCursorANDmask[128] = {
	0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
	0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
	0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
	0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,

	0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
	0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
	0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
	0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,

	0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
	0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
	0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
	0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,

	0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
	0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
	0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
	0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff
};

static BYTE HiddenCursorXORmask[128] = {
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,

	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,

	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,

	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
};

static HCURSOR
GetFatCrossCursor (void)
{
	if (ghFatCrossCursor != NULL)
		return ghFatCrossCursor;

	ghFatCrossCursor = CreateCursor (ghInst,
									 12, 13,	/* Hot spot */
									 32, 32,	/* width, heigth */
									 FatCrossCursorANDmask,
									 FatCrossCursorXORmask
		);

	return ghFatCrossCursor;
}

static HCURSOR
GetHiddenCursor (void)
{
	if (ghHiddenCursor != NULL)
		return ghHiddenCursor;

	ghHiddenCursor = CreateCursor (ghInst,
								   16, 16,		/* Hot spot */
								   32, 32,		/* width, heigth */
								   HiddenCursorANDmask,
								   HiddenCursorXORmask
		);

	return ghHiddenCursor;
}

static void
DeleteCursors (void)
{
	if (ghFatCrossCursor != NULL)
	{
		DestroyCursor (ghFatCrossCursor);
		ghFatCrossCursor = NULL;
	}

	if (ghHiddenCursor != NULL)
	{
		DestroyCursor (ghHiddenCursor);
		ghHiddenCursor = NULL;
	}
}


static int
GetDoubleDownDistance (void)
{
	if (gDoubleDownDistance == -1)
		gDoubleDownDistance = GetSystemMetrics (SM_CXDOUBLECLK) / 2;

	return gDoubleDownDistance;
}

extern EXPORT_TO_CLEAN OS
WinSetDoubleDownDist (int dd, OS ios)
{
	gDoubleDownDistance = dd;
	return ios;
}

static HCURSOR
SetCursorFromCode (int code)
{
	switch (code)
	{
		case CURSARROW:
			return SetCursor (LoadCursor (0, IDC_ARROW));
		case CURSBUSY:
			return SetCursor (LoadCursor (0, IDC_WAIT));
		case CURSIBEAM:
			return SetCursor (LoadCursor (0, IDC_IBEAM));
		case CURSCROSS:
			return SetCursor (LoadCursor (0, IDC_CROSS));
		case CURSFATCROSS:
			return SetCursor (GetFatCrossCursor ());
		case CURSHIDDEN:
			return SetCursor (GetHiddenCursor ());
		default:
			return NULL;
	}
}

static BOOL
IsApplicationWindow (HWND hw)
{
	while (hw != NULL && hw != ghMainWindow)
		hw = GetWindow (hw, GW_OWNER);

	return (hw != NULL);
}

static HWND
ApplicationWindowUnderCursor (void)
{
	POINT p;
	HWND hwin;

	GetCursorPos (&p);
	hwin = WindowFromPoint (p);

	if (IsApplicationWindow (hwin))
		return hwin;
	else
		return NULL;
}

static int CALLBACK
EnumFontNameProc (ENUMLOGFONT FAR * lpelf,		/* pointer to logical-font  data */
				  NEWTEXTMETRIC FAR * lpntm,	/* pointer to physical-font data */
				  int fontType,					/* type of font */
				  LPARAM lParam					/* address of application-defined data	*/
)
{
	SendMessage1ToClean (CcCbFONTNAME, lpelf->elfLogFont.lfFaceName);

	return 1;
}

static int CALLBACK
EnumFontSizeProc (ENUMLOGFONT FAR * lpelf,		/* pointer to logical-font data  */
				  NEWTEXTMETRIC FAR * lpntm,	/* pointer to physical-font data  */
				  int fontType,					/* type of font  */
				  LPARAM lParam					/* address of application-defined data	*/
)
{
	SendMessage2ToClean (CcCbFONTSIZE,
						 lpntm->tmHeight - lpntm->tmInternalLeading,
						 fontType == TRUETYPE_FONTTYPE);

	if (fontType == TRUETYPE_FONTTYPE)
		return 0;
	else
		return 1;
}


/*	Win(M/S)DIClientToOuterSizeDims returns the width and height needed to add/subtract
	from the client/outer size to obtain the outer/client size. 
	These values must be the same as used by W95AdjustClean(M/S)DIWindowDimensions!
*/
extern EXPORT_TO_CLEAN void
WinMDIClientToOuterSizeDims (int styleFlags, OS ios, int *dw, int *dh, OS *oos)
{
	if ((styleFlags&WS_THICKFRAME) != 0)
	{	/* resizable window */
		*dw = 2 * GetSystemMetrics (SM_CXSIZEFRAME);
		*dh = 2 * GetSystemMetrics (SM_CYSIZEFRAME) + GetSystemMetrics (SM_CYCAPTION);
	} else
	{	/* fixed size window */
		*dw = 2 * GetSystemMetrics (SM_CXFIXEDFRAME);
		*dh = 2 * GetSystemMetrics (SM_CYFIXEDFRAME) + GetSystemMetrics (SM_CYCAPTION);
	}

	*oos = ios;
}

extern EXPORT_TO_CLEAN void
WinSDIClientToOuterSizeDims (int styleFlags, OS ios, int *dw, int *dh, OS *oos)
{
	*dw = 2 * GetSystemMetrics (SM_CXSIZEFRAME);
	*dh = 2 * GetSystemMetrics (SM_CYSIZEFRAME) + GetSystemMetrics (SM_CYCAPTION);

	*oos = ios;
}

/*	Adjust the dimensions of a Clean MDI document window with W95 frame dimensions. 
	These values must be the same as returned by WinMDIClientToOuterSizeDims!
*/
static void
W95AdjustCleanMDIWindowDimensions (DWORD styleFlags, POINT * dim)
{
	if ((styleFlags&WS_THICKFRAME) != 0)
	{	/* resizable window */
		dim->x += 2 * GetSystemMetrics (SM_CXSIZEFRAME);
		dim->y += 2 * GetSystemMetrics (SM_CYSIZEFRAME) + GetSystemMetrics (SM_CYCAPTION);
	} else
	{	/* fixed size window */
		dim->x += 2 * GetSystemMetrics (SM_CXFIXEDFRAME);
		dim->y += 2 * GetSystemMetrics (SM_CYFIXEDFRAME) + GetSystemMetrics (SM_CYCAPTION);
	}
}

/*	Adjust the dimensions of a Clean SDI frame window with W95 frame and menu dimensions.
	These values must be the same as returned by WinSDIClientToOuterSizeDims!
*/
static void
W95AdjustCleanSDIWindowDimensions (DWORD styleFlags, POINT * dim)
{
	dim->x += 2 * GetSystemMetrics (SM_CXSIZEFRAME);
	dim->y += 2 * GetSystemMetrics (SM_CYSIZEFRAME) + GetSystemMetrics (SM_CYCAPTION);
}

static int
GetModifiers (void)
{
	int mods;
	mods = 0;

	if (GetKeyState (VK_SHIFT) < 0)
		mods |= SHIFTBIT;
	if (GetKeyState (VK_CONTROL) < 0)
		mods |= CTRLBIT;
	if (GetKeyState (VK_MENU) < 0)
		mods |= ALTBIT;

	return mods;
}

static BOOL 
mystrequal (int length, char *s1, char *s2)
{
	int i = 0;
	while (s1[i] == s2[i])
	{
		if (i >= length)
			return TRUE;
		i++;
	}
	return FALSE;
}


/*	Find the first non CompoundControl parent window of the argument
	hwnd. This procedure assumes that hwnd is the handle of a control. 
*/
static HWND
GetControlParent (HWND hwndControl)
{
	HWND parent;
	char *parentclassname;
	int  classnamelength;

	parent = GetParent (hwndControl);
	classnamelength = lstrlen (CompoundControlClassName) + 1;
	parentclassname = rmalloc (classnamelength);
	GetClassName (parent, parentclassname, classnamelength);

	while (mystrequal (classnamelength, parentclassname, CompoundControlClassName))
	{
		parent = GetParent (parent);
		GetClassName (parent,parentclassname,classnamelength);
	}
	rfree (parentclassname);
	return parent;
}

/*	Return the hwnd of the parent window only if this is
	a Dialog (actually: is neither a SDIWindow or MDIWindow). If the parent is not
	a Dialog then NULL is returned. 
	This procedure assumes that hwnd is the handle of a control.
*/
static HWND
GetControlParentDialog (HWND hwndControl)
{
	HWND parent;
	char *parentclassname;
	int  classnamelength;

	parent = GetControlParent (hwndControl);

	classnamelength = lstrlen (SDIWindowClassName) + 1;
	parentclassname = rmalloc (classnamelength);
	GetClassName (parent, parentclassname, classnamelength);

	if (mystrequal (classnamelength, parentclassname, SDIWindowClassName))
	{
		parent = NULL;
	}
	else if (mystrequal (classnamelength, parentclassname, MDIWindowClassName))
	{
		parent = NULL;
	}
	rfree (parentclassname);

	return parent;
}

/*	IsSDIDocumentWindow (hwnd)
	returns TRUE if the class name of hwnd is SDIWindowClassName.
*/
static BOOL
IsSDIDocumentWindow (HWND hwnd)
{
	char *classname;
	int  classnamelength;
	BOOL isSDI;
	
	classnamelength = lstrlen (SDIWindowClassName) + 1;
	classname       = rmalloc (classnamelength);
	GetClassName (hwnd, classname, classnamelength);

	isSDI = mystrequal (classnamelength, classname, SDIWindowClassName);

	rfree (classname);

	return isSDI;
}

/*	IsMDIDocumentWindow (hwnd)
	returns TRUE if the class name of hwnd is MDIWindowClassName.
*/
static BOOL
IsMDIDocumentWindow (HWND hwnd)
{
	char *classname;
	int  classnamelength;
	BOOL isMDI;
	
	classnamelength = lstrlen (MDIWindowClassName) + 1;
	classname       = rmalloc (classnamelength);
	GetClassName (hwnd, classname, classnamelength);

	isMDI = mystrequal (classnamelength, classname, MDIWindowClassName);

	rfree (classname);

	return isMDI;
}

/*	GetSDIClientWindow finds the first SDI client window of the argument hwnd.
		This procedure assumes that hwnd is the handle of a SDI frame window.
		If no SDI client window could be found then GetSDIClientWindow returns NULL.
*/
static HWND
GetSDIClientWindow (HWND hwndFrame)
{
	HWND client;
	char *clientclassname;
	int  classnamelength;

	client = GetWindow (hwndFrame,GW_CHILD);
	classnamelength = lstrlen (SDIWindowClassName) + 1;
	clientclassname = rmalloc (classnamelength);
	GetClassName (client, clientclassname, classnamelength);

	while (client != NULL && !mystrequal (classnamelength, clientclassname, SDIWindowClassName))
	{
		client = GetWindow (client,GW_HWNDNEXT);
		GetClassName (client,clientclassname,classnamelength);
	}
	rfree (clientclassname);
	return client;
}


/*	Set and get the GWL_USERDATA of a windowhandle.
*/
static void SetGWL_USERDATA (LONG data, HWND hwnd)
{
	SetWindowLong (hwnd, GWL_USERDATA, data);
}

static LONG GetGWL_USERDATA (HWND hwnd)
{
	return GetWindowLong (hwnd, GWL_USERDATA);
}

/*	Access the GWL_STYLE of a windowhandle:
		GetGWL_STYLE (hwnd) returns the GWL_STYLE value of hwnd;
		WindowHasHScroll (hwnd) returns TRUE iff hwnd has a horizontal scrollbar;
		WindowHasVScroll (hwnd) returns TRUE iff hwnd has a vertical scrollbar;
*/
static LONG GetGWL_STYLE (HWND hwnd)
{
	return GetWindowLong (hwnd, GWL_STYLE);
}

static BOOL WindowHasHScroll (HWND hwnd)
{
	LONG hwndStyle = GetGWL_STYLE (hwnd);

	return (hwndStyle & WS_HSCROLL);
}

static BOOL WindowHasVScroll (HWND hwnd)
{
	LONG hwndStyle = GetGWL_STYLE (hwnd);

	return (hwndStyle & WS_VSCROLL);
}


/*	Translate virtual key codes to the codes shared with Clean.
	This procedure has been filtered from RbrtTranslateMessage.
	If the keycode could not be translated, zero is returned.
*/
static int
CheckVirtualKeyCode (int keycode)
{
	int c = 0;
	switch (keycode)
	{
		case VK_UP:
			c = WinUpKey;
			break;
		case VK_DOWN:
			c = WinDownKey;
			break;
		case VK_LEFT:
			c = WinLeftKey;
			break;
		case VK_RIGHT:
			c = WinRightKey;
			break;
		case VK_PRIOR:
			c = WinPgUpKey;
			break;
		case VK_NEXT:
			c = WinPgDownKey;
			break;
		case VK_END:
			c = WinEndKey;
			break;
		case VK_HOME:
			c = WinBeginKey;
			break;
		case VK_BACK:
			c = WinBackSpKey;
			break;
		case VK_DELETE:
			c = WinDelKey;
			break;
		case VK_TAB:
			c = WinTabKey;
			break;
		case VK_RETURN:
			c = WinReturnKey;
			break;
		case VK_ESCAPE:
			c = WinEscapeKey;
			break;
		case VK_HELP:
			c = WinHelpKey;
			break;
		case VK_F1:
			c = WinF1Key;
			break;
		case VK_F2:
			c = WinF2Key;
			break;
		case VK_F3:
			c = WinF3Key;
			break;
		case VK_F4:
			c = WinF4Key;
			break;
		case VK_F5:
			c = WinF5Key;
			break;
		case VK_F6:
			c = WinF6Key;
			break;
		case VK_F7:
			c = WinF7Key;
			break;
		case VK_F8:
			c = WinF8Key;
			break;
		case VK_F9:
			c = WinF9Key;
			break;
		case VK_F10:
			c = WinF10Key;
			break;
		case VK_F11:
			c = WinF11Key;
			break;
		case VK_F12:
			c = WinF12Key;
			break;
	}
	return c;
}

static void
SendKeyDownToClean (HWND hwndParent, HWND hwndChild, int c)
{
	SendMessage5ToClean (CcWmKEYBOARD, hwndParent, hwndChild, c, KEYDOWN, GetModifiers ());
}

static void
SendKeyStillDownToClean (HWND hwndParent, HWND hwndChild, int c)
{
	SendMessage5ToClean (CcWmKEYBOARD, hwndParent, hwndChild, c, KEYREPEAT, GetModifiers ());
}

static void
SendKeyUpToClean (HWND hwndParent, HWND hwndChild, int c)
{
	SendMessage5ToClean (CcWmKEYBOARD, hwndParent, hwndChild, c, KEYUP, GetModifiers ());
}

static void
SendMouseUpToClean (HWND hwndParent, HWND hwndChild, int x, int y)
{
	gInMouseDown = FALSE;
	KillTimer (hwndChild, (UINT) - 1);
	SendMessage6ToClean (CcWmMOUSE, hwndParent, hwndChild, BUTTONUP, x, y, GetModifiers ());
}

static void
SendMouseStillDownToClean (HWND hwndParent, HWND hwndChild, int x, int y)
{
	SendMessage6ToClean (CcWmMOUSE, hwndParent, hwndChild, BUTTONSTILLDOWN, x, y, GetModifiers ());
}

static void
SendMouseStillUpToClean (HWND hwndParent, HWND hwndChild, int x, int y)
{
	SendMessage6ToClean (CcWmMOUSE, hwndParent, hwndChild, BUTTONSTILLUP, x, y, GetModifiers ());
}

static BOOL
inDcSpaceTime (HWND curwin,  int curx,  int cury,  UINT curtime,
			   HWND prevwin, int prevx, int prevy, UINT prevtime)
{
	if (rabs (curx - prevx) > GetDoubleDownDistance ())
		return FALSE;

	if (rabs (cury - prevy) > GetDoubleDownDistance ())
		return FALSE;

	if (curtime - prevtime > GetDoubleClickTime ())
		return FALSE;

	if (curwin != prevwin)
		return FALSE;

	return TRUE;
}

static void
SendMouseDownToClean (HWND hwndParent, HWND hwndChild, int x, int y)
{
	gInMouseDown = TRUE;
	SetCapture (hwndChild);
	SetFocus (hwndChild);		/* Pressing the mouse must also set the keyboard input to that object. */
	SetTimer (hwndChild, (UINT) - 1, (UINT) 1, NULL);

	if (gClicks != 0 && !inDcSpaceTime (hwndChild, x, y, GetMessageTime (), gClHwnd, gClX, gClY, gClTime))
		gClicks = 0;

	if (gClicks == 0)
	{
		gClTime = GetMessageTime ();
		gClX = x;
		gClY = y;
		gClHwnd = hwndChild;
	}

	gClicks++;

	SendMessage6ToClean (CcWmMOUSE, hwndParent, hwndChild, gClicks, x, y, GetModifiers ());
	if (gClicks == 3)
		gClicks = 0;
}


/*	This routine should be applied only in case of WM_DROPFILES messages.
	It will copy a buffer of all involved file names into one string and send it
	to Clean.
*/
static void 
SendDropFilesToClean (HWND hWin,WPARAM wPara)
{
	int nrFiles, fileNr, nrChars, charCount;
	char * filenames;

	/* first retrieve the number of files. */
	nrFiles = DragQueryFile ((HANDLE)wPara, 0xFFFFFFFF, (LPSTR)NULL, 0);

	/* calculate the number of characters. */
	nrChars = 0;
	for (fileNr=0; fileNr<nrFiles; fileNr++)
	{
		nrChars += (int) DragQueryFile ((HANDLE)wPara, (UINT)fileNr, (LPSTR)NULL, 0);
		nrChars += 1;				/* account for newline char */
	}
	nrChars += 1;					/* and add a zero at the end of the string */

	filenames = rmalloc (nrChars);	/* This pointer is passed to and freed in the Clean code. (see also WM_DDE_EXECUTE)*/
	charCount = 0;
	for (fileNr=0; fileNr<nrFiles; fileNr++)
	{
		charCount += (int) DragQueryFile ((HANDLE)wPara, (UINT)fileNr, (LPTSTR)filenames+charCount, (UINT)nrChars-charCount);
		*(filenames+charCount) = '\n';
		charCount += 1;
	}
	*(filenames+nrChars-1) = 0;		/* terminate string with zero character. */

	DragFinish ((HANDLE)wPara);		/* free the internal memory required for DROPFILES. */
	SendMessage2ToClean (CcWmPROCESSDROPFILES, (int) hWin, (int) filenames);
}


static BOOL CALLBACK
SetControlFontProc (HWND hchild,		/* handle to child window */
					LPARAM lParam		/* application-defined value */
)
{
	HFONT hfont;

	hfont = (HFONT) lParam;

	if (hfont)
		SendMessage (hchild, WM_SETFONT, (WPARAM) hfont, MAKELPARAM (TRUE, 0));

	return TRUE;
}

/*	The callback routine for a modal/modeless dialog box.
*/
static BOOL CALLBACK
DialogProcedure (HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{
	printMessage ("Dialog procedure", hwnd, message, wParam, lParam);

	switch (message)
	{
		case WM_CLOSE:
			{
				SendMessage1ToClean (CcWmCLOSE, hwnd);
				return TRUE;
			}
			break;
		case WM_SETCURSOR:
			{
				if (gGlobalCursorCode == -1)
				{
					return FALSE;
				}
				else
				{
					SetCursorFromCode (gGlobalCursorCode);
					SetWindowLong (hwnd, DWL_MSGRESULT, TRUE);
					return TRUE;
				}
			} break;
		/*	WM_ENTERIDLE message is used to let Clean evaluate the initialisation action
			of a modal dialog by sending the CcWmIDLEDIALOG message.
		*/
		case WM_ENTERIDLE:
			{
				HWND hwndModalDialog;
				
				hwndModalDialog = (HWND)lParam;

				if (wParam == MSGF_DIALOGBOX && hwndModalDialog != ghwndLastModalDialog)
				{
					SendMessage1ToClean (CcWmIDLEDIALOG,(int)hwndModalDialog);
					ghwndLastModalDialog = hwndModalDialog;
				}
				else
				{
					SendMessage0ToClean (CcWmIDLETIMER);
				}
				return FALSE;
			} break;
		case WM_ENTERMENULOOP:
			{
				gProhibitWindowActivation++;
				return FALSE;
			} break;
		case WM_EXITMENULOOP:
			{
				gProhibitWindowActivation--;
				return FALSE;
			} break;
		case WM_TIMER:
			{
				SendMessage2ToClean (CcWmTIMER, wParam, GetMessageTime ());
				return FALSE;
			} break;
		/* WM_ACTIVATE:
			*	in case of activate and previously a window was active, 
				send a deactivate message to Clean for the window.
			*	in case of deactivate and new active is a window,
				send an activate message to Clean for the window.
		*/
		case WM_ACTIVATE:
			{
				switch (LOWORD (wParam))
				{
					case WA_ACTIVE:
					case WA_CLICKACTIVE:
						{
							if (gActiveDialog == NULL && ghTopDocWindow != NULL)
							{
								/*	Currently a window is active. 
									Notify Clean of this by sending a deactivate event for this window.
								*/
								SendMessage1ToClean (CcWmDEACTIVATE, ghTopDocWindow);
							}
							SendMessage1ToClean (CcWmACTIVATE, hwnd);	/* Now tell Clean that the dialog is active. */
							gActiveDialog = hwnd;
						}
						break;
					case WA_INACTIVE:
						{
							HWND hwndNewActive = (HWND)lParam;			/* The window handle to be activated. */

							SendMessage1ToClean (CcWmDEACTIVATE, hwnd);
							gActiveDialog = NULL;

							if (hwndNewActive != NULL && hwndNewActive == ghTopDocWindow)
							{
								SendMessage1ToClean (CcWmACTIVATE, hwndNewActive);
							}
							else if (hwndNewActive == NULL && ghTopDocWindow != NULL)
							{
								SendMessage1ToClean (CcWmACTIVATE, ghTopDocWindow);
							}
						}
						break;
				}
				return FALSE;
			}
			break;
		case WM_COMMAND:
			{
				switch (LOWORD (wParam))	// First check if OK or CANCEL button has been pressed
				{
					case IDOK:
						{
							SendMessage2ToClean (CcWmSPECIALBUTTON, hwnd, ISOKBUTTON);
						}
						return TRUE;
					case IDCANCEL:
						{
							SendMessage2ToClean (CcWmSPECIALBUTTON, hwnd, ISCANCELBUTTON);
						}
						return TRUE;
				}
				switch (HIWORD (wParam))
				{
					case BN_CLICKED:
						{
							if (lParam != 0)
							{
								/* Send also modifiers to Clean */
								SendMessage4ToClean (CcWmBUTTONCLICKED, hwnd, lParam, GetModifiers (), LOWORD (wParam));
							}
							else if (LOWORD (wParam) == 2)
							{
								SendMessage1ToClean (CcWmCLOSE, hwnd);
							}
							return TRUE;
						}
						break;
					case CBN_SETFOCUS:
						{
							gComboSelection = SendMessage ((HWND) lParam, CB_GETCURSEL, 0, 0);
							return FALSE;
						}
						break;
					case CBN_KILLFOCUS:
						{
							gComboSelection = -1;
							return FALSE;
						}
						break;
					case CBN_SELENDOK:
						{
							char text[256];
							int newsel;
							HWND combo;

							combo = (HWND) lParam;
							newsel = SendMessage (combo, CB_GETCURSEL, 0, 0);
							SendMessage (combo, CB_GETLBTEXT, newsel, (LPARAM) text);
							if (!SendMessage (combo, CB_GETITEMDATA, newsel, 0))
							{
								SendMessage (combo, CB_SETCURSEL, gComboSelection, (LPARAM) text);
								MessageBeep (0xFFFFFFFF);
							}
							else
							{
								gComboSelection = newsel;
								if (newsel!=CB_ERR)
									SendMessage3ToClean (CcWmCOMBOSELECT, hwnd, combo, newsel);
							}
							return 1;
						}
						break;
				}
				return FALSE;
			} break;
		case WM_INITDIALOG:
			{
				int x, y, w, h;
				HWND defctrl;

				SendMessage1ToClean (CcWmINITDIALOG, hwnd);

				x = gCci.p1;
				y = gCci.p2;
				w = gCci.p3;
				h = gCci.p4;
				defctrl = (HWND) gCci.p5;

				w += 2 * GetSystemMetrics (SM_CXFIXEDFRAME);
				h += 2 * GetSystemMetrics (SM_CXFIXEDFRAME) + GetSystemMetrics (SM_CYCAPTION);

				if (x == -1 && y == -1)
				{
					x = (GetSystemMetrics (SM_CXSCREEN) - w) / 2;
					y = (GetSystemMetrics (SM_CYSCREEN) - h) / 2;
				}

				MoveWindow (hwnd, x, y, w, h, FALSE);

				EnumChildWindows (hwnd, SetControlFontProc, (LPARAM) gDlogFont);

				if (defctrl != NULL)
				{
					SetFocus (defctrl);
					return FALSE;
				}
				else
				{
					return TRUE;		/* allow windows to set focus;	*/
				}
			}
			break;
		case WM_SETFONT:
			{
				HFONT hfont;

				hfont = (HFONT) wParam;

				gDlogFont = hfont;
				return FALSE;
			}
			break;
		case WM_HSCROLL:
		case WM_VSCROLL:
			{
				int nPos,nScrollCode;
				HWND hwndScrollBar;

				nScrollCode = LOWORD (wParam);

				if (nScrollCode != SB_ENDSCROLL)	/* Do not send the SB_ENDSCROLL to Clean. */
				{
					nPos = (short int) HIWORD (wParam);
					hwndScrollBar = (HWND) lParam;
					SendMessage5ToClean (CcWmSCROLLBARACTION, hwnd, hwndScrollBar, SB_CTL, nScrollCode, nPos);
				}
				return TRUE;
			}
			break;
		case WM_DRAWITEM:
			{
				LPDRAWITEMSTRUCT lpdis;
				lpdis = (LPDRAWITEMSTRUCT) lParam;

				switch (lpdis->CtlType)
				{
					case ODT_COMBOBOX:
						{
							char text[256];
							COLORREF forecolor, bkcolor;
							SendMessage (lpdis->hwndItem, CB_GETLBTEXT, lpdis->itemID, (LPARAM) text);
							if (lpdis->itemState & ODS_DISABLED)
							{
								forecolor = SetTextColor (lpdis->hDC, GetSysColor (COLOR_GRAYTEXT));
								bkcolor = SetBkColor (lpdis->hDC, GetSysColor (COLOR_3DFACE));
							}
							else if (lpdis->itemState & ODS_SELECTED)
							{
								if (lpdis->itemData)
								{
									forecolor = SetTextColor (lpdis->hDC, GetSysColor (COLOR_HIGHLIGHTTEXT));
									bkcolor = SetBkColor (lpdis->hDC, GetSysColor (COLOR_HIGHLIGHT));
								}
								else
								{
									forecolor = SetTextColor (lpdis->hDC, GetSysColor (COLOR_GRAYTEXT));
									bkcolor = SetBkColor (lpdis->hDC, GetSysColor (COLOR_WINDOW));
								}
							}
							else
							{
								if (lpdis->itemData)
									forecolor = SetTextColor (lpdis->hDC, GetSysColor (COLOR_WINDOWTEXT));
								else
									forecolor = SetTextColor (lpdis->hDC, GetSysColor (COLOR_GRAYTEXT));
								bkcolor = SetBkColor (lpdis->hDC, GetSysColor (COLOR_WINDOW));
							}

							ExtTextOut (lpdis->hDC,					/* device context         */
										lpdis->rcItem.left + 2,		/* ref point x            */
										lpdis->rcItem.top + 1,		/* ref point y            */
										ETO_CLIPPED | ETO_OPAQUE,	/* options                */
										&lpdis->rcItem,				/* clipping rect          */
										text,						/* text to draw           */
										lstrlen (text),				/* length of text to draw */
										NULL						/* no kerning array       */
								);

							SetTextColor (lpdis->hDC, forecolor);
							SetBkColor (lpdis->hDC, bkcolor);

							if (lpdis->itemState & ODS_FOCUS)
								DrawFocusRect (lpdis->hDC, &lpdis->rcItem);

							return TRUE;
						} break;
					case ODT_BUTTON:
						{
							SendMessage3ToClean (CcWmDRAWCONTROL, hwnd, lpdis->hwndItem, lpdis->hDC);
								 
							if (lpdis->itemState & ODS_SELECTED)
								InvertRect (lpdis->hDC, &lpdis->rcItem);
								
							if (lpdis->itemState & ODS_FOCUS)
								DrawFocusRect (lpdis->hDC, &lpdis->rcItem);
							return TRUE;
						} break;
				}
				return FALSE;
			}
			break;
		/*	WM_SETFOCUS alternative circumvents application-crash when dialog is closed
			from the inside (by an element button or close box).
		*/
		case WM_SETFOCUS:
			{
				return TRUE;
			}
			break;
		default:
			return FALSE;
			break;
	}
	ErrorExit ("Fatal error: case leak in DialogProcedure (%d).",message);
}
/*	End of DialogProcedure callback routine.
*/


// MW...
void lookUpAndRemove(WPARAM dnsHdl,DNSInfo **listPtr,DNSInfo **elPtr)
// This function is used to look up an element with a matching dnsHdl-field in
// the DNSInfoList. This element will then be removed from the list.
{
	if ((WPARAM)(*listPtr)->dnsHdl==dnsHdl)
		{	// the object to look up has been found, so remove it from the list
			// and give it back via elPtr.
			*elPtr = *listPtr;
			*listPtr = (*listPtr)->next;
		}
	  else
		lookUpAndRemove(dnsHdl, &(*listPtr)->next, elPtr);	
}
// ... MW

/*	The callback routine for the main window.
	PA: The WM_CREATE  message registers the main window as a clipboard viewer. 
		The WM_DESTROY message unregisters the main window. 
*/
static LRESULT CALLBACK
MainWindowProcedure (HWND hWin, UINT uMess, WPARAM wPara, LPARAM lPara)
{
	printMessage ("Main Window", hWin, uMess, wPara, lPara);
	switch (uMess)
	{
		case WM_NCPAINT:
			break;
		case WM_ACTIVATEAPP:
			{
				if (wPara)
				{
					gAppRunning = TRUE;
				}
				else
				{
					gAppRunning = FALSE;
				}
			}
			break;
		/*	WM_ENTERIDLE message is used to let Clean evaluate the initialisation action
			of a modal dialog by sending the CcWmIDLEDIALOG message.
		*/
		case WM_ENTERIDLE:
			{
				HWND hwndModalDialog;
				
				hwndModalDialog = (HWND)lPara;

				if (wPara == MSGF_DIALOGBOX && hwndModalDialog != ghwndLastModalDialog)
				{
					SendMessage1ToClean (CcWmIDLEDIALOG,(int)hwndModalDialog);
					ghwndLastModalDialog = hwndModalDialog;
				}
				else
				{
					SendMessage0ToClean (CcWmIDLETIMER);
				}
				return 0;
			} break;
		case WM_TIMER:
			{
				SendMessage2ToClean (CcWmTIMER, wPara, GetMessageTime ());
			}
			break;
		case WM_ENABLE:
			{
				HWND hwin;
				char title[64];

				hwin = GetWindow (ghMainWindow, GW_HWNDFIRST);
				while (hwin != NULL)
				{
					GetWindowText (hwin, title, 63);

					if (GetWindow (hwin, GW_OWNER) == ghMainWindow)
					{
						RECT r;
						GetWindowRect (hwin, &r);
						if (r.top != -1 || r.left != -1 || r.right != 0 || r.bottom != 0)
						{
							EnableWindow (hwin, (BOOL) wPara);
						}
					}
					hwin = GetWindow (hwin, GW_HWNDNEXT);
				}
			}
			break;
		/* PA: The WM_CREATE message registers the ghMainWindow (hWin) as a clipboard viewer.
		*/
		case WM_CREATE:
			{
				gNextClipboardViewer = SetClipboardViewer (hWin);
			}
			break;
		/* PA: The WM_DESTROY message unregisters the ghMainWindow (hWin) as a clipboard viewer.
		*/
		case WM_DESTROY:
			{
				ChangeClipboardChain (hWin, gNextClipboardViewer);
			}
			break;
		/* PA: other clipboard chain management messages:
		*/
		case WM_DRAWCLIPBOARD:
			{
				gClipboardCount += 1;
				if (gNextClipboardViewer != NULL)
					SendMessage (gNextClipboardViewer, uMess, wPara, lPara);

				return 0;
			}
			break;
		case WM_CHANGECBCHAIN:
			{
				if ((HWND)wPara == gNextClipboardViewer)	/* gNextClipboardViewer is no longer a clipboard viewer. */
					gNextClipboardViewer = (HWND)lPara;		/*	then store the new viewer. */
				else if (gNextClipboardViewer != NULL)
					SendMessage (gNextClipboardViewer, uMess, wPara, lPara);

				return 0;
			}
			break;
// MW...
		case PM_SOCKET_EVENT:
			{
				// wPara is the socket handle
				// LOWORD(lPara) is the message
				// HIWORD(lPara) is an error code
				switch (LOWORD(lPara))
				{	case FD_OOB:
					case FD_READ:	SendMessage3ToClean(CcWmINETEVENT,IE_RECEIVED, wPara,
														RChanReceiver);
									break;
					case FD_WRITE:	SendMessage3ToClean(CcWmINETEVENT,IE_SENDABLE, wPara,
														SChanReceiver);
									break;
					case FD_ACCEPT:	SendMessage3ToClean(CcWmINETEVENT,IE_CONNECTREQUEST, wPara,
														ListenerReceiver);
									break;
					case FD_CONNECT:SendMessage3ToClean(
										CcWmINETEVENT,
										HIWORD(lPara)==0 ?	IE_ASYNCCONNECTCOMPLETE :
															IE_ASYNCCONNECTFAILED,
										wPara,
										ConnectReceiver);
									break;
					case FD_CLOSE:	{
									dictitem	*pDictitem;
									pDictitem		= lookup(wPara);
									if (pDictitem) {
										if (pDictitem->hasReceiveNotifier)
											SendMessage3ToClean(CcWmINETEVENT,IE_EOM, wPara,
																RChanReceiver);
									
										if (pDictitem->hasSendableNotifier && HIWORD(lPara)!=0)
											SendMessage3ToClean(CcWmINETEVENT,IE_DISCONNECTED, wPara,
																SChanReceiver);
										};
									};
									break;
				};
			};
			break;
		case PM_DNS_EVENT:
			{ // wPara contains the DNS handle (the handle created by WSAAsyncGetHostByName
			  // The IP-adress of the looked up host will have been written into the
			  // corresponding element of the DNSInfoList. Look it up:

			  struct DNSInfo	*elPtr;
			  int				errCode;

			  errCode = HIWORD(lPara);

			  lookUpAndRemove(wPara,&DNSInfoList,&elPtr);
			  
			  // *elPtr contains the info

			  SendMessage4ToClean(	CcWmINETEVENT,
									errCode ?	IE_IPADDRESSNOTFOUND :
												IE_IPADDRESSFOUND,							,
									elPtr->dnsHdl,
									DNSReceiver,
									errCode ?
										0 :
										ntohl(((int*)(*(elPtr->junion.Hostent.h_addr_list)))[0])
								 );

			  // deallocate unused memory
			  LocalFree(elPtr);
			  
			};
			break;
// ... MW
		case WM_DDE_INITIATE:
			{
				static char apptext[256], topictext[256];
				ATOM aApp, aTopic;
/* RWS ... */
				BOOL handleTopic;
/* ... RWS */
				GlobalGetAtomName (HIWORD (lPara), topictext, 256);

				if (lstrcmp (topictext, "CLEANOPEN") == 0)
/* RWS: compare application name */
				{
					GlobalGetAtomName (LOWORD (lPara), apptext, 256);
					handleTopic	= CompareStringA (LOCALE_USER_DEFAULT, NORM_IGNORECASE,
									apptext, lstrlen (apptext), gAppName, lstrlen (gAppName)) == 2;	/* 2 means they are equal */
				}
				else
					handleTopic	= FALSE;

				if (handleTopic)
				{
/* ... RWS */
					aApp = GlobalAddAtom (apptext);
					aTopic = GlobalAddAtom (topictext);
					SendMessage ((HWND) wPara, WM_DDE_ACK, (WPARAM) hWin, MAKELONG (aApp, aTopic));
					GlobalDeleteAtom (aApp);
					GlobalDeleteAtom (aTopic);
				}
				else
				{
					return DefWindowProc (hWin, uMess, wPara, lPara);
				}
			} break;
		case WM_DDE_EXECUTE:
			{
				char *commandstring;
				char *pcommand;
				int len;
				union
				{
					DDEACK ddeack;
					WORD w;
				}	da;

				pcommand = GlobalLock ((HANDLE) lPara);
				len = lstrlen (pcommand) + 1;
				commandstring = rmalloc (len);	/* this pointer is passed to and freed in the Clean code. */
				lstrcpyn (commandstring, pcommand, len);
				GlobalUnlock ((HANDLE) lPara);

				SendMessage1ToClean (CcWmDDEEXECUTE, commandstring);

				da.ddeack.bAppReturnCode = 0;
				da.ddeack.fBusy = 0;
				da.ddeack.fAck = 1;
				PostMessage ((HWND) wPara, WM_DDE_ACK, (WPARAM) hWin, PackDDElParam (WM_DDE_ACK, (UINT) da.w, lPara));
				return 0;
			} break;
		case WM_DDE_TERMINATE:
			{
				PostMessage ((HWND) wPara, WM_DDE_TERMINATE, (WPARAM) hWin, 0);
			} return 0;
		default:
			return DefWindowProc (hWin, uMess, wPara, lPara);
			break;
	}
	return 0;
}
/*	End of MainWindowProcedure callback routine. 
*/


/*	The callback routine for custom controls.
*/
static LRESULT CALLBACK
CustomControlProcedure (HWND hwnd, UINT uMess, WPARAM wParam, LPARAM lParam)
{
	printMessage ("CustomControlProcedure", hwnd, uMess, wParam, lParam);
	switch (uMess)
	{
		case WM_PAINT:
			{
				HWND parent;
				HDC hdc;
				PAINTSTRUCT ps;

				parent = GetControlParent (hwnd);

				hdc = BeginPaint (hwnd, &ps);
				SendMessage3ToClean (CcWmDRAWCONTROL, parent, hwnd, hdc);
				EndPaint (hwnd, &ps);

				return 0;
			} break;
		case WM_LBUTTONDOWN:
			{
				SendMouseDownToClean (GetControlParent (hwnd), hwnd, SIGNEDLOWORD (lParam), SIGNEDHIWORD (lParam));
				return 0;
			} break;
		case WM_MOUSEMOVE:
			{
				if (gInMouseDown)
				{
					SendMouseStillDownToClean (GetControlParent (hwnd), hwnd, SIGNEDLOWORD (lParam), SIGNEDHIWORD (lParam));
				}
				else
				{
					SendMouseStillUpToClean (GetControlParent (hwnd), hwnd, SIGNEDLOWORD (lParam), SIGNEDHIWORD (lParam));
				}
				return 0;
			} break;
		case WM_LBUTTONUP:
			{
				if (gInMouseDown)
				{
					ReleaseCapture ();	/* rely on WM_CAPTURECHANGED to send the mouseUp event */
				}
				return 0;
			} break;
		case WM_CANCELMODE:
			{
				if (gInMouseDown)
				{
					ReleaseCapture ();	/* rely on WM_CAPTURECHANGED to send the mouseUp event */
				}
				return DefWindowProc (hwnd, uMess, wParam, lParam);
			} break;
		case WM_CAPTURECHANGED:
			{
				if (gInMouseDown)
				{
					POINT p;
					GetCursorPos (&p);
					ScreenToClient (hwnd, &p);
					SendMouseUpToClean (GetControlParent (hwnd), hwnd, p.x, p.y);
				}
				return 0;
			} break;
		case WM_TIMER:
			{
				if ((int) wParam == -1)
				{
					POINT p;
					GetCursorPos (&p);
					ScreenToClient (hwnd, &p);
					SendMouseStillDownToClean (GetControlParent (hwnd), hwnd, p.x, p.y);
				}
				else
				{
					rprintf ("Custom control did not expect normal WM_TIMER message with id = %d.\n", wParam);
				}
				return 0;
			} break;
		case WM_SYSKEYDOWN:
		case WM_KEYDOWN:
			{
				int c = 0;
				HWND hwndParent;

				c = CheckVirtualKeyCode ((int) wParam);

				if (!c)
				/* Ignore non-virtual keys, because they arrive as WM_SYSCHAR and WM_CHAR. */
				{
					return DefWindowProc (hwnd, uMess, wParam, lParam);
				}
				/* Handle virtual keys analogously to keys received as WM_SYSCHAR and WM_CHAR. */
				hwndParent = GetControlParent (hwnd);
				if (gInKey)
				{
					if (gCurChar == c)
						SendKeyStillDownToClean (hwndParent, hwnd, gCurChar);
					else
					{
						SendKeyUpToClean (hwndParent, hwnd, gCurChar);
						gCurChar = c;
						SendKeyDownToClean (hwndParent, hwnd, gCurChar);
					}
				}
				else
				{
					gCurChar = c;
					SendKeyDownToClean (hwndParent, hwnd, gCurChar);
					gInKey = TRUE;
				}
				return 0;
			}
			break;
		case WM_SYSCHAR:
		case WM_CHAR:
			{
				HWND hwndParent = GetControlParent (hwnd);

				if (gInKey)
				{
					if (gCurChar == (int) wParam)
						SendKeyStillDownToClean (hwndParent, hwnd, gCurChar);
					else
					{
						SendKeyUpToClean (hwndParent, hwnd, gCurChar);
						gCurChar = wParam;
						SendKeyDownToClean (hwndParent, hwnd, gCurChar);
					}
				}
				else
				{
					gCurChar = wParam;
					SendKeyDownToClean (hwndParent, hwnd, gCurChar);
					gInKey = TRUE;
				}
				return 0;
			}
			break;
		case WM_SYSKEYUP:
		case WM_KEYUP:
			{
				if (gInKey)
					SendKeyUpToClean (GetControlParent (hwnd), hwnd, gCurChar);
				gInKey = FALSE;
				gCurChar = 0;
				return DefWindowProc (hwnd, uMess, wParam, lParam);
			}
			break;
		case WM_KILLFOCUS:
			{
				HWND hwndParent = GetControlParent (hwnd);
				if (gInKey)
					SendKeyUpToClean (hwndParent, hwnd, gCurChar);
				gInKey = FALSE;
				gCurChar = 0;
				/*	WM_KILLFOCUS now also sends the CcWmKILLFOCUS message to
					Clean (because of the ControlDeactivate attribute). 
				*/
				SendMessage2ToClean (CcWmKILLFOCUS, hwndParent, hwnd);
				return 0;
			}
			break;
		case WM_SETFOCUS:
			{
				/*	WM_SETFOCUS sends the CcWmSETFOCUS message to Clean (because of
					ControlActivate attribute).
				*/
				SendMessage2ToClean (CcWmSETFOCUS, GetControlParent (hwnd), hwnd);
				return 0;
			}
			break;
		case WM_GETDLGCODE:		/*	Inform dialog procedure to pass all keyboard input to the control. */
			return (DLGC_WANTCHARS | DLGC_WANTARROWS);
			break;
		default:
			return DefWindowProc (hwnd, uMess, wParam, lParam);
			break;
	}
	ErrorExit ("Fatal error: case leak in CustomControlProcedure (%d).",uMess);
}
/*	End of CustomControlProcedure callback routine. 
*/


/*	Callback routine for compound controls.
*/
static LRESULT CALLBACK
CompoundControlProcedure (HWND hwnd, UINT uMess, WPARAM wParam, LPARAM lParam)
{
	printMessage ("CompoundControlProcedure", hwnd, uMess, wParam, lParam);
	switch (uMess)
	{
		case WM_COMMAND:
			{
				switch (HIWORD (wParam))
				{
					case BN_CLICKED:
						{
							if (lParam != 0)
							{
								/*	Send also modifiers to Clean */
								SendMessage4ToClean (CcWmBUTTONCLICKED, GetControlParent (hwnd), lParam, GetModifiers (), LOWORD (wParam));
							}
							return 0;
						}
						break;
					case CBN_SETFOCUS:
						{
							gComboSelection = SendMessage ((HWND) lParam, CB_GETCURSEL, 0, 0);
							return 0;
						}
						break;
					case CBN_KILLFOCUS:
						{
							gComboSelection = -1;
							return 0;
						}
						break;
					case CBN_SELENDOK:
						{
							char text[256];
							int newsel;
							HWND combo;

							combo = (HWND) lParam;
							newsel = SendMessage (combo, CB_GETCURSEL, 0, 0);
							SendMessage (combo, CB_GETLBTEXT, newsel, (LPARAM) text);
							if (!SendMessage (combo, CB_GETITEMDATA, newsel, 0))
							{
								SendMessage (combo, CB_SETCURSEL, gComboSelection, (LPARAM) text);
								MessageBeep (0xFFFFFFFF);
								return 0;
							}
							else
							{
								gComboSelection = newsel;
								if (newsel!=CB_ERR)
									SendMessage3ToClean (CcWmCOMBOSELECT, GetControlParent (hwnd), combo, newsel);
								return 1;
							}
						}
						break;
				}
				return 0;
			} break;
		case WM_PAINT:
			{
				HWND parentwindow;
				HDC hdc;
				PAINTSTRUCT ps;

			//	if (GetUpdateRect(hwnd,NULL,FALSE))	// determine if there is really an update area. 
			//	{
				parentwindow = GetControlParent (hwnd);
				
				hdc = BeginPaint (hwnd, &ps);
				SendMessage3ToClean (CcWmDRAWCONTROL, parentwindow, hwnd, hdc);
				EndPaint (hwnd, &ps);
			//	}
				
				return 0;
			} break;
		case WM_HSCROLL:
			{
				int nPos,nScrollCode,controlkind;
				HWND parentwindow, hwndScrollBar;

				nScrollCode = LOWORD (wParam);

				if (nScrollCode != SB_ENDSCROLL)	/* Do not send the SB_ENDSCROLL to Clean. */
				{
					nPos = (short int) HIWORD (wParam);
					parentwindow  = GetControlParent (hwnd);
					hwndScrollBar = (HWND) lParam;

					if (hwndScrollBar==0)
					{
						controlkind = SB_HORZ;		/* lParam==0 in case of Compound scrollbars. */
						hwndScrollBar = hwnd;		/* pass the compound control handle to Clean. */
						UpdateWindow (hwnd);		/* but first ensure that compound control is updated. */
					}
					else
					{
						controlkind = SB_CTL;		/* lParam!==0 in case of SliderControls. */
					}
					SendMessage5ToClean (CcWmSCROLLBARACTION, parentwindow, hwndScrollBar, controlkind, nScrollCode, nPos);
				}
				return 0;
			}
			break;
		case WM_VSCROLL:
			{
				int nPos,nScrollCode,controlkind;
				HWND parentwindow, hwndScrollBar;

				nScrollCode = LOWORD (wParam);

				if (nScrollCode != SB_ENDSCROLL)	/* Do not send the SB_ENDSCROLL to Clean. */
				{
					nPos = (short int) HIWORD (wParam);
					parentwindow  = GetControlParent (hwnd);
					hwndScrollBar = (HWND) lParam;

					if (hwndScrollBar==0)
					{
						controlkind = SB_VERT;		/* lParam==0 in case of Compound scrollbars. */
						hwndScrollBar = hwnd;		/* pass the compound control handle to Clean. */
						UpdateWindow (hwnd);		/* but first ensure that compound control is updated. */
					}
					else
					{
						controlkind = SB_CTL;		/* lParam!==0 in case of SliderControls. */
					}
					SendMessage5ToClean (CcWmSCROLLBARACTION, parentwindow, hwndScrollBar, controlkind, nScrollCode, nPos);
				}
				return 0;
			}
			break;
		/*	The following cases concerning mouse events 
				(WM_LBUTTONDOWN upto WM_TIMER) have been copied from CustomControlProcedure. 
		*/
		case WM_LBUTTONDOWN:
			{
				SendMouseDownToClean (GetControlParent (hwnd), hwnd, SIGNEDLOWORD (lParam), SIGNEDHIWORD (lParam));
				return 0;
			} break;
		case WM_MOUSEMOVE:
			{
				if (gInMouseDown)
				{
					SendMouseStillDownToClean (GetControlParent (hwnd), hwnd, SIGNEDLOWORD (lParam), SIGNEDHIWORD (lParam));
				}
				else
				{
					SendMouseStillUpToClean (GetControlParent (hwnd), hwnd, SIGNEDLOWORD (lParam), SIGNEDHIWORD (lParam));
				}
				return 0;
			} break;
		case WM_LBUTTONUP:
			{
				if (gInMouseDown)
				{
					ReleaseCapture ();	/* rely on WM_CAPTURECHANGED to send the mouseUp event */
				}
				return 0;
			} break;
		case WM_CANCELMODE:
			{
				if (gInMouseDown)
				{
					ReleaseCapture ();	/* rely on WM_CAPTURECHANGED to send the mouseUp event */
				}
				return DefWindowProc (hwnd, uMess, wParam, lParam);
			} break;
		case WM_CAPTURECHANGED:
			{
				if (gInMouseDown)
				{
					POINT p;
					GetCursorPos (&p);
					ScreenToClient (hwnd, &p);
					SendMouseUpToClean (GetControlParent (hwnd), hwnd, p.x, p.y);
				}
				return 0;
			} break;
		case WM_TIMER:
			{
				if ((int) wParam == -1)
				{
					POINT p;
					GetCursorPos (&p);
					ScreenToClient (hwnd, &p);
					SendMouseStillDownToClean (GetControlParent (hwnd), hwnd, p.x, p.y);
				}
				else
				{
					rprintf ("Compound control did not expect normal WM_TIMER message with id = %d.\n", wParam);
				}
				return 0;
			} break;
		/*	The following cases concerning key events and focus events 
				(WM_SYSKEYDOWN upto WM_GETDLGCODE) have been copied from CustomControlProcedure.
		*/
		case WM_SYSKEYDOWN:
		case WM_KEYDOWN:
			{
				int c = 0;
				HWND hwndParent;

				c = CheckVirtualKeyCode ((int) wParam);

				if (!c)
				/* Ignore non-virtual keys, because they arrive as WM_SYSCHAR and WM_CHAR. */
				{
					return DefWindowProc (hwnd, uMess, wParam, lParam);
				}
				/* Handle virtual keys analogously to keys received as WM_SYSCHAR and WM_CHAR. */
				hwndParent = GetControlParent (hwnd);
				if (gInKey)
				{
					if (gCurChar == c)
						SendKeyStillDownToClean (hwndParent, hwnd, gCurChar);
					else
					{
						SendKeyUpToClean (hwndParent, hwnd, gCurChar);
						gCurChar = c;
						SendKeyDownToClean (hwndParent, hwnd, gCurChar);
					}
				}
				else
				{
					gCurChar = c;
					SendKeyDownToClean (hwndParent, hwnd, gCurChar);
					gInKey = TRUE;
				}
				return 0;
			}
			break;
		case WM_SYSCHAR:
		case WM_CHAR:
			{
				HWND hwndParent = GetControlParent (hwnd);

				if (gInKey)
				{
					if (gCurChar == (int) wParam)
						SendKeyStillDownToClean (hwndParent, hwnd, gCurChar);
					else
					{
						SendKeyUpToClean (hwndParent, hwnd, gCurChar);
						gCurChar = wParam;
						SendKeyDownToClean (hwndParent, hwnd, gCurChar);
					}
				}
				else
				{
					gCurChar = wParam;
					SendKeyDownToClean (hwndParent, hwnd, gCurChar);
					gInKey = TRUE;
				}
				return 0;
			}
			break;
		case WM_SYSKEYUP:
		case WM_KEYUP:
			{
				if (gInKey)
					SendKeyUpToClean (GetControlParent (hwnd), hwnd, gCurChar);
				gInKey = FALSE;
				gCurChar = 0;
				return DefWindowProc (hwnd, uMess, wParam, lParam);
			}
			break;
		case WM_KILLFOCUS:
			{
				HWND hwndParent = GetControlParent (hwnd);
				if (gInKey)
					SendKeyUpToClean (hwndParent, hwnd, gCurChar);
				gInKey = FALSE;
				gCurChar = 0;
				/*	WM_KILLFOCUS now also sends the CcWmKILLFOCUS message to
					Clean (because of the ControlDeactivate attribute).
				*/
				SendMessage2ToClean (CcWmKILLFOCUS, hwndParent, hwnd);
				return 0;
			}
			break;
		case WM_SETFOCUS:
			{
				/*	WM_SETFOCUS sends the CcWmSETFOCUS message to Clean because
					of the ControlActivate attribute.
				*/
				SendMessage2ToClean (CcWmSETFOCUS, GetControlParent (hwnd), hwnd);
				return 0;
			}
			break;
		/*	The WM_CLOSE event is generated when a user presses escape inside an EditControl that exists
			within the CompoundControl which exists within a Dialog.
		*/
		case WM_CLOSE:
			{
				SendMessage1ToClean (CcWmCLOSE, GetControlParent (hwnd));
				return 0;
			}
			break;
		case WM_GETDLGCODE:		/*	Inform dialog procedure to pass all keyboard input to the control. */
			return (DLGC_WANTCHARS | DLGC_WANTARROWS);
			break;
		case WM_DRAWITEM:
			{
				LPDRAWITEMSTRUCT lpdis;
				lpdis = (LPDRAWITEMSTRUCT) lParam;

				switch (lpdis->CtlType)
				{
					case ODT_COMBOBOX:
						{
							char text[256];
							COLORREF forecolor, bkcolor;
							SendMessage (lpdis->hwndItem, CB_GETLBTEXT, lpdis->itemID, (LPARAM) text);
							if (lpdis->itemState & ODS_DISABLED)
							{
								forecolor = SetTextColor (lpdis->hDC, GetSysColor (COLOR_GRAYTEXT));
								bkcolor = SetBkColor (lpdis->hDC, GetSysColor (COLOR_3DFACE));
							}
							else if (lpdis->itemState & ODS_SELECTED)
							{
								if (lpdis->itemData)
								{
									forecolor = SetTextColor (lpdis->hDC, GetSysColor (COLOR_HIGHLIGHTTEXT));
									bkcolor = SetBkColor (lpdis->hDC, GetSysColor (COLOR_HIGHLIGHT));
								}
								else
								{
									forecolor = SetTextColor (lpdis->hDC, GetSysColor (COLOR_GRAYTEXT));
									bkcolor = SetBkColor (lpdis->hDC, GetSysColor (COLOR_WINDOW));
								}
							}
							else
							{
								if (lpdis->itemData)
									forecolor = SetTextColor (lpdis->hDC, GetSysColor (COLOR_WINDOWTEXT));
								else
									forecolor = SetTextColor (lpdis->hDC, GetSysColor (COLOR_GRAYTEXT));
								bkcolor = SetBkColor (lpdis->hDC, GetSysColor (COLOR_WINDOW));
							}

							ExtTextOut (lpdis->hDC,					/* device context         */
										lpdis->rcItem.left + 2,		/* ref point x            */
										lpdis->rcItem.top + 1,		/* ref point y            */
										ETO_CLIPPED | ETO_OPAQUE,	/* options                */
										&lpdis->rcItem,				/* clipping rect          */
										text,						/* text to draw           */
										lstrlen (text),				/* length of text to draw */
										NULL						/* no kerning array       */
								);

							SetTextColor (lpdis->hDC, forecolor);
							SetBkColor (lpdis->hDC, bkcolor);

							if (lpdis->itemState & ODS_FOCUS)
								DrawFocusRect (lpdis->hDC, &lpdis->rcItem);
							return 0;
						} break;
					case ODT_BUTTON:
						{
							HWND parentwindow;
							parentwindow  = GetControlParent (hwnd);

							SendMessage3ToClean (CcWmDRAWCONTROL, parentwindow, lpdis->hwndItem, lpdis->hDC);
							 
							if (lpdis->itemState & ODS_SELECTED)
								InvertRect (lpdis->hDC, &lpdis->rcItem);
							
							if (lpdis->itemState & ODS_FOCUS)
								DrawFocusRect (lpdis->hDC, &lpdis->rcItem);
							return 0;
						} break;
				}
				return 0;
			}
			break;
		default:
			return DefWindowProc (hwnd, uMess, wParam, lParam);
			break;
	}
	ErrorExit ("Fatal error: case leak in CompoundControlProcedure (%d).",uMess);
}
/*	End of CompoundControlProcedure callback routine.
*/


/*	Event handler for subclassing the client window of a MDI frame window. 
	This routine catches only WM_WINDOWPOSCHANGING event.
*/
static LRESULT CALLBACK
MDIClientProcedure (HWND hwnd,UINT uMess,WPARAM wParam,LPARAM lParam)
{
	printMessage ("Clean MDI Client", hwnd, uMess, wParam, lParam);

	switch (uMess)
	{
		case WM_WINDOWPOSCHANGING:
			{
				WINDOWPOS* wp;
				RECT tbRect;
				int tbHeight;
				HWND hwndFrame, hwndToolbar;

				wp = (LPWINDOWPOS)lParam;

				hwndFrame = GetParent (hwnd);
				hwndToolbar = (HWND)GetGWL_USERDATA (hwndFrame);

				if (hwndToolbar==0)
				{
					tbHeight = 0;
				} else
				{
					GetWindowRect (hwndToolbar,&tbRect);
					tbHeight = tbRect.bottom - tbRect.top;
				}
				wp->y  = tbHeight;
				wp->cy = wp->cy - tbHeight;

				return 0;
			}
			break;
	}
	return CallWindowProc ((WNDPROC) stdMDIClientCallback, hwnd, uMess, wParam, lParam);
}
/*	End of MDIClientProcedure callback routine.
*/


/*	There are two event handlers for subclassing edit controls:
	*	EditControlProcedure:
		This routine should be used for key sensitive edit controls. 
		It processes tab advance if in a dialog, activate/deactivate, keyboard input.
	*	SimpleEditControlProcedure:
		This routine should be used for non key sensitive edit controls.
		It processes tab advance if in a dialog, activate/deactivate.
*/
/*	EditControlProcedure. 
	This routine catches all keyboard input and handles it as a standard windows edit control would, but in addition
	it also sends each keyboard input to Clean. The only exception are tab key presses. For this keyboard input
	EditControlProcedure first checks if it is inside a Dialog. If so, then the keyfocus is advanced to the next
	control; otherwise the tab key is also sent to Clean.
	SetFocus/KillFocus events are passed to Clean as Activate and Deactivate events. 
	All other events are handled as a standard windows edit control. 
*/
static LRESULT CALLBACK
EditControlProcedure (HWND hwnd,UINT uMess,WPARAM wParam,LPARAM lParam)
{
	LRESULT stdresult;

	printMessage ("Clean Edit Control", hwnd, uMess, wParam, lParam);

	/* First preprocess all messages as if you are a standard Windows edit control. */
	stdresult = CallWindowProc ((WNDPROC) stdEditCallback, hwnd, uMess, wParam, lParam);

	switch (uMess)
	{
		case WM_SYSKEYDOWN:
		case WM_KEYDOWN:
			{
				int c = 0;
				HWND hwndParent;

				c = CheckVirtualKeyCode ((int) wParam);

				/* Check if the keyboard input is a tab key. If we're in a Dialog then move focus to next control. */
				if (c==WinTabKey)
				{
					hwndParent = GetControlParentDialog (hwnd);
					if (hwndParent!=NULL)	/* The EditControl is in a Dialog */
					{
						int modifiers;
						BOOL bPrevious;
						HWND hwndNextCtrl;

						modifiers = GetModifiers ();
						bPrevious = (BOOL) modifiers & SHIFTBIT;
						hwndNextCtrl = GetNextDlgTabItem (hwndParent,hwnd,bPrevious);

						if (hwndNextCtrl != hwnd)		/* You're not the only focusable item. */
						{
							SetFocus (hwndNextCtrl);	/* Advance key focus to next control. */
						}
						return stdresult;
					}
				}
				/* Check if the keyboard input is a escape key. If we're in a Dialog then further processing should halt,
				   because the message has been passed along the parent hierarchy to cause a WM_CLOSE event. 
				*/
				if (c==WinEscapeKey)
				{
					hwndParent = GetControlParentDialog (hwnd);
					if (hwndParent!=NULL)	/* The EditControl is in a Dialog */
					{
						return stdresult;
					}
				}

				if (!c)
				/* Ignore non-virtual keys, because they arrive as WM_SYSCHAR and WM_CHAR. */
				{
					return stdresult;
				}
				/* Handle virtual keys analogously to keys received as WM_SYSCHAR and WM_CHAR. */
				hwndParent = GetControlParent (hwnd);
				if (gInKey)
				{
					if (gCurChar == c)
						SendKeyStillDownToClean (hwndParent, hwnd, gCurChar);
					else
					{
						SendKeyUpToClean (hwndParent, hwnd, gCurChar);
						gCurChar = c;
						SendKeyDownToClean (hwndParent, hwnd, gCurChar);
					}
				}
				else
				{
					gCurChar = c;
					SendKeyDownToClean (hwndParent, hwnd, gCurChar);
					gInKey = TRUE;
				}
				return stdresult;
			}
			break;
		case WM_SYSCHAR:
		case WM_CHAR:
			{
				HWND hwndParent;
				
				/* First check if tab/escape key should be suppressed inside Dialog. */
				if (((int)wParam == WinTabKey || (int)wParam == WinEscapeKey) && GetControlParentDialog (hwnd) != NULL)
				{
					return stdresult;
				}

				hwndParent = GetControlParent (hwnd);

				if (gInKey)
				{
					if (gCurChar == (int) wParam)
						SendKeyStillDownToClean (hwndParent, hwnd, gCurChar);
					else
					{
						SendKeyUpToClean (hwndParent, hwnd, gCurChar);
						gCurChar = wParam;
						SendKeyDownToClean (hwndParent, hwnd, gCurChar);
					}
				}
				else
				{
					gCurChar = wParam;
					SendKeyDownToClean (hwndParent, hwnd, gCurChar);
					gInKey = TRUE;
				}
				return stdresult;
			}
			break;
		case WM_SYSKEYUP:
		case WM_KEYUP:
			{
				/* First check if tab/escape key should be suppressed inside Dialog. */
				if (((int)wParam == WinTabKey || (int)wParam == WinEscapeKey) && GetControlParentDialog (hwnd) != NULL)
				{
					return stdresult;
				}

				if (gInKey)
					SendKeyUpToClean (GetControlParent (hwnd), hwnd, gCurChar);
				gInKey = FALSE;
				gCurChar = 0;
				return stdresult;
			}
			break;
		case WM_KILLFOCUS:
			{
				HWND hwndParent = GetControlParent (hwnd);
				if (gInKey)
					SendKeyUpToClean (hwndParent, hwnd, gCurChar);
				gInKey = FALSE;
				gCurChar = 0;
				/*	WM_KILLFOCUS now also sends the CcWmKILLFOCUS message to
					Clean (because of the ControlDeactivate attribute). 
				*/
				SendMessage2ToClean (CcWmKILLFOCUS, hwndParent, hwnd);
				return stdresult;
			}
			break;
		case WM_SETFOCUS:
			{
				/*	WM_SETFOCUS sends the CcWmSETFOCUS message to Clean because
					of the ControlActivate attribute.
				*/
				SendMessage2ToClean (CcWmSETFOCUS, GetControlParent (hwnd), hwnd);
				return stdresult;
			}
			break;
	}
	return stdresult;
}
/*	End of EditControlProcedure callback routine.
*/
/*	SimpleEditControlProcedure. 
	This routine catches all keyboard input and handles it as a standard windows edit control would. 
	The only exception are tab key presses. For this keyboard input SimpleEditControlProcedure first 
	checks if it is inside a Dialog. If so, then the keyfocus is advanced to the next
	control.
	SetFocus/KillFocus events are passed to Clean as Activate and Deactivate events. 
	All other events are handled as a standard windows edit control. 
*/
static LRESULT CALLBACK
SimpleEditControlProcedure (HWND hwnd,UINT uMess,WPARAM wParam,LPARAM lParam)
{
	LRESULT stdresult;

	printMessage ("Clean Simple Edit Control", hwnd, uMess, wParam, lParam);

	/* First preprocess all messages as if you are a standard Windows edit control. */
	stdresult = CallWindowProc ((WNDPROC) stdEditCallback, hwnd, uMess, wParam, lParam);

	switch (uMess)
	{
		case WM_SYSKEYDOWN:
		case WM_KEYDOWN:
			{
				int c = 0;
				HWND hwndParent;

				c = CheckVirtualKeyCode ((int) wParam);

				/* Check if the keyboard input is a tab key. If we're in a Dialog then move focus to next control. */
				if (c==WinTabKey)
				{
					hwndParent = GetControlParentDialog (hwnd);
					if (hwndParent!=NULL)	/* The EditControl is in a Dialog */
					{
						int modifiers;
						BOOL bPrevious;
						HWND hwndNextCtrl;

						modifiers = GetModifiers ();
						bPrevious = (BOOL) modifiers & SHIFTBIT;
						hwndNextCtrl = GetNextDlgTabItem (hwndParent,hwnd,bPrevious);

						if (hwndNextCtrl != hwnd)		/* You're not the only focusable item. */
						{
							SetFocus (hwndNextCtrl);	/* Advance key focus to next control. */
						}
					}
				}
				return stdresult;
			}
			break;
		case WM_KILLFOCUS:
			{
				HWND hwndParent = GetControlParent (hwnd);
				if (gInKey)
					SendKeyUpToClean (hwndParent, hwnd, gCurChar);
				gInKey = FALSE;
				gCurChar = 0;
				/*	WM_KILLFOCUS now also sends the CcWmKILLFOCUS message to
					Clean (because of the ControlDeactivate attribute). 
				*/
				SendMessage2ToClean (CcWmKILLFOCUS, hwndParent, hwnd);
				return stdresult;
			}
			break;
		case WM_SETFOCUS:
			{
				/*	WM_SETFOCUS sends the CcWmSETFOCUS message to Clean because
					of the ControlActivate attribute.
				*/
				SendMessage2ToClean (CcWmSETFOCUS, GetControlParent (hwnd), hwnd);
				return stdresult;
			}
			break;
	}
	return stdresult;
}
/*	End of SimpleEditControlProcedure callback routine.
*/


/*	Event handler for subclassing editable pop up controls. 
	This routine catches all keyboard input and handles it as a standard windows pop up control would, but in addition
	it also sends each keyboard input to Clean.
	All other events are handled as a standard windows pop up control. 
*/
static LRESULT CALLBACK
PopUpControlProcedure (HWND hwnd,UINT uMess,WPARAM wParam,LPARAM lParam)
{
	LRESULT stdresult;

	printMessage ("Clean PopUp Control", hwnd, uMess, wParam, lParam);

	/* First preprocess all messages as if you are a standard Windows edit control of a combobox. */
	stdresult = CallWindowProc ((WNDPROC) stdPopUpCallback, hwnd, uMess, wParam, lParam);

	switch (uMess)
	{
		case WM_SYSKEYDOWN:
		case WM_KEYDOWN:
			{
				int c = 0;
				HWND hwndParent;
				HWND hwndCombo;

				c = CheckVirtualKeyCode ((int) wParam);

				if (!c)
				/* Ignore non-virtual keys, because they arrive as WM_SYSCHAR and WM_CHAR. */
				{
					return stdresult;
				}
				/* Handle virtual keys analogously to keys received as WM_SYSCHAR and WM_CHAR. */
				hwndCombo  = GetParent (hwnd);
				hwndParent = GetControlParent (hwndCombo);
				if (gInKey)
				{
					if (gCurChar == c)
						SendKeyStillDownToClean (hwndParent, hwndCombo, gCurChar);
					else
					{
						SendKeyUpToClean (hwndParent, hwndCombo, gCurChar);
						gCurChar = c;
						SendKeyDownToClean (hwndParent, hwndCombo, gCurChar);
					}
				}
				else
				{
					gCurChar = c;
					SendKeyDownToClean (hwndParent, hwndCombo, gCurChar);
					gInKey = TRUE;
				}
				return stdresult;
			}
			break;
		case WM_SYSCHAR:
		case WM_CHAR:
			{
				HWND hwndCombo  = GetParent (hwnd);
				HWND hwndParent = GetControlParent (hwndCombo);

				if (gInKey)
				{
					if (gCurChar == (int) wParam)
						SendKeyStillDownToClean (hwndParent, hwndCombo, gCurChar);
					else
					{
						SendKeyUpToClean (hwndParent, hwndCombo, gCurChar);
						gCurChar = wParam;
						SendKeyDownToClean (hwndParent, hwndCombo, gCurChar);
					}
				}
				else
				{
					gCurChar = wParam;
					SendKeyDownToClean (hwndParent, hwndCombo, gCurChar);
					gInKey = TRUE;
				}
				return stdresult;
			}
			break;
		case WM_SYSKEYUP:
		case WM_KEYUP:
			{
				HWND hwndCombo  = GetParent (hwnd);
				HWND hwndParent = GetControlParent (hwndCombo);
				if (gInKey)
					SendKeyUpToClean (hwndParent, hwndCombo, gCurChar);
				gInKey = FALSE;
				gCurChar = 0;
				return stdresult;
			}
			break;
		case WM_KILLFOCUS:
			{
				HWND hwndCombo  = GetParent (hwnd);
				HWND hwndParent = GetControlParent (hwndCombo);
				if (gInKey)
					SendKeyUpToClean (hwndParent, hwndCombo, gCurChar);
				gInKey = FALSE;
				gCurChar = 0;
				/*	WM_KILLFOCUS now also sends the CcWmKILLFOCUS message to 
					Clean (because of the ControlDeactivate attribute).
				*/
				SendMessage2ToClean (CcWmKILLFOCUS, hwndParent, hwndCombo);
				return stdresult;
			}
			break;
		/*	Send CcWmGETFOCUS message to Clean because of the ControlActivate attribute. 
		*/
		case WM_SETFOCUS:
			{
				HWND hwndCombo  = GetParent (hwnd);
				HWND hwndParent = GetControlParent (hwndCombo);
				SendMessage2ToClean (CcWmSETFOCUS, hwndParent, hwndCombo);
				return stdresult;
			}
			break;
	}

	return stdresult;
}
/*	End of PopUpControlProcedure callback routine.
*/


/*	Callback routine for SDI client windows.
	The callback routine handles all events for the client window of a SDI window.
	The accelerator table is now managed by its parent SDI frame window callback routine.
*/
static LRESULT CALLBACK
SDIWindowProcedure (HWND hWin,UINT uMess,WPARAM wPara,LPARAM lPara)
{
	printMessage ("Clean SDI Window", hWin, uMess, wPara, lPara);
	switch (uMess)
	{
		case WM_COMMAND:
			{
				switch (HIWORD (wPara))
				{
					case 0:		/*	0: message originates from a menu or equals BN_CLICKED */
						{
							if (lPara != 0)		/*	It was BN_CLICKED. */
							{
								/*	Send also modifiers to Clean */
								SendMessage4ToClean (CcWmBUTTONCLICKED, hWin, lPara, GetModifiers (), LOWORD (wPara));
							}
							else				/*	It was from a menu. */
							{
								SendMessage2ToClean (CcWmCOMMAND, LOWORD (wPara), GetModifiers ());
							}
						}
						break;
					case 1:		/*	1: message originates from an accelerator */
						{
							SendMessage2ToClean (CcWmCOMMAND, LOWORD (wPara), GetModifiers ());
						}
						break;
					case CBN_SETFOCUS:
						{
							gComboSelection = SendMessage ((HWND) lPara, CB_GETCURSEL, 0, 0);
						}
						break;
					case CBN_KILLFOCUS:
						{
							gComboSelection = -1;
						}
						break;
					case CBN_SELENDOK:
						{
							char text[256];
							int newsel;
							HWND combo;

							combo = (HWND) lPara;
							newsel = SendMessage (combo, CB_GETCURSEL, 0, 0);
							SendMessage (combo, CB_GETLBTEXT, newsel, (LPARAM) text);
							if (!SendMessage (combo, CB_GETITEMDATA, newsel, 0))
							{
								SendMessage (combo, CB_SETCURSEL, gComboSelection, (LPARAM) text);
								MessageBeep (0xFFFFFFFF);
								return 0;
							}
							else
							{
								gComboSelection = newsel;
								if (newsel!=CB_ERR)
									SendMessage3ToClean (CcWmCOMBOSELECT, hWin, combo, newsel);
								return 1;
							}
						}
						break;
				}
			} break;
		case WM_PAINT:
			{
				RECT updaterect;
				HDC  hdc;
				PAINTSTRUCT ps;
				
				if (GetUpdateRect (hWin, &updaterect, FALSE))
				{
					hdc = BeginPaint (hWin, &ps);
					if (updaterect.left != updaterect.right && updaterect.top != updaterect.bottom)
						SendMessage6ToClean (CcWmPAINT, hWin, updaterect.left,updaterect.top,updaterect.right,updaterect.bottom,hdc);
					EndPaint (hWin, &ps);
				}
				else
				{	
					GetClientRect (hWin, &updaterect);
					SendMessage6ToClean (CcWmPAINT, hWin, updaterect.left,
									 updaterect.top,
									 updaterect.right,
									 updaterect.bottom,wPara);
				}
				return 0;		//PAPAPA
			}
			break;
		case WM_SETCURSOR:
			{
				int cursorcode;
				LocalWindowData wdata;

				if (gGlobalCursorCode == -1)
				{
					if ((HWND) wPara != hWin || LOWORD (lPara) != HTCLIENT)
					{
						return DefWindowProc (hWin, uMess, wPara, lPara);
					}
					wdata = (LocalWindowData) GetWindowLong (hWin,0);
					cursorcode = wdata->lwd_cursorcode;
				}
				else
				{
					cursorcode = gGlobalCursorCode;
				}
				SetCursorFromCode (cursorcode);
			}
			break;
		case WM_SYSCHAR:
		case WM_CHAR:
			{
				if (gInKey)
				{
					if (gCurChar == (int) wPara)
						SendKeyStillDownToClean (hWin, hWin, gCurChar);
					else
					{
						SendKeyUpToClean (hWin, hWin, gCurChar);
						gCurChar = wPara;
						SendKeyDownToClean (hWin, hWin, gCurChar);
					}
				}
				else
				{
					gCurChar = wPara;
					SendKeyDownToClean (hWin, hWin, gCurChar);
					gInKey = TRUE;
				}
			}
			break;
		case WM_SYSKEYUP:
		case WM_KEYUP:
			{
				if (gInKey)
					SendKeyUpToClean (hWin, hWin, gCurChar);
				gInKey = FALSE;
				gCurChar = 0;
				return (DefWindowProc (hWin, uMess, wPara, lPara));
			}
			break;
		case WM_KILLFOCUS:
			{
				if (gInKey)
					SendKeyUpToClean (hWin, hWin, gCurChar);
				gInKey = FALSE;
				gCurChar = 0;
			}
			break;
		case WM_LBUTTONDOWN:
			{
				SendMouseDownToClean (hWin, hWin, SIGNEDLOWORD (lPara), SIGNEDHIWORD (lPara));
			}
			break;
		case WM_MOUSEMOVE:
			{
				if (gInMouseDown)
				{
					SendMouseStillDownToClean (hWin, hWin, SIGNEDLOWORD (lPara), SIGNEDHIWORD (lPara));
				}
				else
				{
					SendMouseStillUpToClean (hWin, hWin, SIGNEDLOWORD (lPara), SIGNEDHIWORD (lPara));
				}
			}
			break;
		case WM_LBUTTONUP:
			{
				if (gInMouseDown)
				{
					ReleaseCapture ();	/* rely on WM_CAPTURECHANGED to send the mouseUp event */
				}
			}
			break;
		case WM_CANCELMODE:
			{
				if (gInMouseDown)
				{
					ReleaseCapture ();	/* rely on WM_CAPTURECHANGED to send the mouseUp event */
				}
				return DefWindowProc (hWin, uMess, wPara, lPara);
			}
			break;
		case WM_CAPTURECHANGED:
			{
				if (gInMouseDown)
				{
					POINT p;
					GetCursorPos (&p);
					ScreenToClient (hWin, &p);
					SendMouseUpToClean (hWin, hWin, p.x, p.y);
				}
			}
			break;
		case WM_TIMER:
			{
				if ((int) wPara == -1)
				{
					POINT p;
					GetCursorPos (&p);
					ScreenToClient (hWin, &p);
					SendMouseStillDownToClean (hWin, hWin, p.x, p.y);
				}
				else
				{
					rprintf ("Clean Window did not expect normal WM_TIMER message with id = %d.\n", wPara);
				}
			}
			break;
		/*	The WM_CREATE message should cause the SDI client window to notify Clean that its controls
			can be built. The accelerator table is maintained by the SDI frame window.
			In addition, the LocalWindowData is created and stored in the local memory of the SDI window.
		*/
		case WM_CREATE:
			{
				LocalWindowData wdata;

				wdata = AllocateLocalWindowData ();			// create the LocalWindowData struct
				SetWindowLong (hWin, 0, (long) wdata);		//	and store it in the local memory of the window

				SendMessage1ToClean (CcWmCREATE, hWin);

				/*	After creation of the window controls, their HFONT should be set to 8pt "MS Sans Serif" */
				EnumChildWindows (hWin, SetControlFontProc, (LPARAM) gControlFont);
			}
			break;
		/*	The WM_DESTROY message should free the local SDI window memory.
		*/
		case WM_DESTROY:
			{
				LocalWindowData wdata;

				wdata = (LocalWindowData) GetWindowLong (hWin,0);	//	get the local SDI window data
				DestroyLocalWindowData (wdata);						//	and destroy it.

				return 0;
			}
			break;
		/*	The cases WM_ENTERSIZEMOVE and WM_EXITSIZEMOVE flag the lwd_usersizemove field
			of the LocalWindowData of the window. This is used to determine whether the window
			should be redrawn in case of resizing. 
		*/
		case WM_ENTERSIZEMOVE:
			{
				LocalWindowData wdata;

				wdata = (LocalWindowData) GetWindowLong (hWin,0);
				wdata->lwd_usersizemoving = (BOOL)TRUE;
				SetWindowLong (hWin, 0, (long)wdata);
			}
			break;
		case WM_EXITSIZEMOVE:
			{
				LocalWindowData wdata;

				wdata = (LocalWindowData) GetWindowLong (hWin,0);
				wdata->lwd_usersizemoving = (BOOL)FALSE;
				SetWindowLong (hWin, 0, (long)wdata);
			}
			break;
		/*	The WM_SIZE message informs Clean about the new size.
		*/
		case WM_SIZE:
			{
				if (wPara != SIZE_MAXHIDE && wPara != SIZE_MAXSHOW)
				{
					int width,height;
					LocalWindowData wdata;

					width  = LOWORD (lPara);		// Width  of window excluding vertical scrollbar 
					height = HIWORD (lPara);		// Height of window excluding horizontal scrollbar 
					wdata  = (LocalWindowData) GetWindowLong (hWin,0);
					UpdateWindow (hWin);			// But first update the window
					SendMessage4ToClean (CcWmSIZE, hWin, width, height, (int)wdata->lwd_usersizemoving);
				}
			}
			break;
		case WM_HSCROLL:
			{
				int nPos,nScrollCode,controlkind;
				HWND hwndScrollBar;

				nScrollCode = LOWORD (wPara);

				if (nScrollCode != SB_ENDSCROLL)	/* Do not send the SB_ENDSCROLL to Clean. */
				{
					nPos = (short int) HIWORD (wPara);
					hwndScrollBar = (HWND) lPara;

					if (hwndScrollBar==0)
					{
						controlkind = SB_HORZ;		/* lPara==0 in case of Window scrollbars. */
						hwndScrollBar = hWin;		/* pass the compound control handle to Clean. */
						UpdateWindow (hWin);		/* but first ensure that window is updated. */
					}
					else
					{
						controlkind = SB_CTL;		/* lPara!=0 in case of SliderControls. */
					}
					SendMessage5ToClean (CcWmSCROLLBARACTION, hWin, hwndScrollBar, controlkind, nScrollCode, nPos);
				}
			}
			break;
		case WM_VSCROLL:
			{
				int nPos,nScrollCode,controlkind;
				HWND hwndScrollBar;

				nScrollCode = LOWORD (wPara);

				if (nScrollCode != SB_ENDSCROLL)	/* Do not send the SB_ENDSCROLL to Clean. */
				{
					nPos = (short int) HIWORD (wPara);
					hwndScrollBar = (HWND) lPara;

					if (hwndScrollBar==0)
					{
						controlkind = SB_VERT;		/* lPara==0 in case of Window scrollbars. */
						hwndScrollBar = hWin;		/* pass the window handle to Clean. */
						UpdateWindow (hWin);		/* but first ensure that window is updated. */
					}
					else
					{
						controlkind = SB_CTL;		/* lPara!=0 in case of SliderControls. */
					}
					SendMessage5ToClean (CcWmSCROLLBARACTION, hWin, hwndScrollBar, controlkind, nScrollCode, nPos);
				}
			}
			break;
		case WM_DRAWITEM:
			{
				LPDRAWITEMSTRUCT lpdis;
				lpdis = (LPDRAWITEMSTRUCT) lPara;

				switch (lpdis->CtlType)
				{
					case ODT_COMBOBOX:
						{
							char text[256];
							COLORREF forecolor, bkcolor;
							SendMessage (lpdis->hwndItem, CB_GETLBTEXT, lpdis->itemID, (LPARAM) text);
							if (lpdis->itemState & ODS_DISABLED)
							{
								forecolor = SetTextColor (lpdis->hDC, GetSysColor (COLOR_GRAYTEXT));
								bkcolor = SetBkColor (lpdis->hDC, GetSysColor (COLOR_3DFACE));
							}
							else if (lpdis->itemState & ODS_SELECTED)
							{
								if (lpdis->itemData)
								{
									forecolor = SetTextColor (lpdis->hDC, GetSysColor (COLOR_HIGHLIGHTTEXT));
									bkcolor = SetBkColor (lpdis->hDC, GetSysColor (COLOR_HIGHLIGHT));
								}
								else
								{
									forecolor = SetTextColor (lpdis->hDC, GetSysColor (COLOR_GRAYTEXT));
									bkcolor = SetBkColor (lpdis->hDC, GetSysColor (COLOR_WINDOW));
								}
							}
							else
							{
								if (lpdis->itemData)
									forecolor = SetTextColor (lpdis->hDC, GetSysColor (COLOR_WINDOWTEXT));
								else
									forecolor = SetTextColor (lpdis->hDC, GetSysColor (COLOR_GRAYTEXT));
								bkcolor = SetBkColor (lpdis->hDC, GetSysColor (COLOR_WINDOW));
							}

							ExtTextOut (lpdis->hDC,					/* device context */
										lpdis->rcItem.left + 2,		/* ref point x */
										lpdis->rcItem.top + 1,		/* ref point y */
										ETO_CLIPPED | ETO_OPAQUE,	/* options */
										&lpdis->rcItem,				/* clipping rect */
										text,						/* text to draw */
										lstrlen (text),				/* length of text to draw */
										NULL						/* no kerning array */
								);

							SetTextColor (lpdis->hDC, forecolor);
							SetBkColor (lpdis->hDC, bkcolor);

							if (lpdis->itemState & ODS_FOCUS)
								DrawFocusRect (lpdis->hDC, &lpdis->rcItem);

						} break;
					case ODT_BUTTON:
						{
							{
								SendMessage3ToClean (CcWmDRAWCONTROL, hWin, lpdis->hwndItem, lpdis->hDC);
								 
								if (lpdis->itemState & ODS_SELECTED)
									InvertRect (lpdis->hDC, &lpdis->rcItem);
								
								if (lpdis->itemState & ODS_FOCUS)
									DrawFocusRect (lpdis->hDC, &lpdis->rcItem);
							};
							return TRUE;
						} break;
				}
			} break;
	}
	return DefWindowProc (hWin,uMess,wPara,lPara);
}
/*	End of SDIWindowProcedure callback routine.
*/


/*	Callback routine for SDI frame window procedure.
	This routine handles the SDI frame events. These concern menus, toolbar, resizes.
	Also the accelerator table is kept at the frame window (analogous to MDI frame window).
	When resized, also the SDI client window (see previous callback routine) is notified.
	Note that whenever Clean is informed about an event, the GetSDIClientWindow(hWin) value
	should be passed to Clean which identifies the Clean SDI client window! The only exception
	is the CcWmPROCESSCLOSE message. 
*/
static LRESULT CALLBACK
SDIFrameProcedure (HWND hWin,UINT uMess,WPARAM wPara,LPARAM lPara)
{
	printMessage ("Clean SDIFrameWindow",hWin,uMess,wPara,lPara);
	switch (uMess)
	{
		case WM_COMMAND:
			{
				switch (HIWORD (wPara))
				{
					case 0:		/*	0: message originates from a menu or equals BN_CLICKED */
						{
							if (lPara != 0)		/* PA: it was BN_CLICKED. */
							{
								/*	hwndClient can't be NULL, because a button has been pressed. */
								HWND hwndClient = GetSDIClientWindow (hWin);
								/*	Send also modifiers to Clean */
								SendMessage4ToClean (CcWmBUTTONCLICKED, hwndClient, lPara, GetModifiers (), LOWORD (wPara));
							}
							else				/*	It was from a menu. */
							{
								SendMessage2ToClean (CcWmCOMMAND, LOWORD (wPara), GetModifiers ());
							}
						}
						break;
					case 1:		/*	1: message originates from an accelerator */
						{
							SendMessage2ToClean (CcWmCOMMAND, LOWORD (wPara), GetModifiers ());
						}
						break;
					case CBN_SETFOCUS:
						{
							gComboSelection = SendMessage ((HWND) lPara, CB_GETCURSEL, 0, 0);
						}
						break;
					case CBN_KILLFOCUS:
						{
							gComboSelection = -1;
						}
						break;
					case CBN_SELENDOK:
						{
							char text[256];
							int newsel;
							HWND combo;

							combo = (HWND) lPara;
							newsel = SendMessage (combo, CB_GETCURSEL, 0, 0);
							SendMessage (combo, CB_GETLBTEXT, newsel, (LPARAM) text);
							if (!SendMessage (combo, CB_GETITEMDATA, newsel, 0))
							{
								SendMessage (combo, CB_SETCURSEL, gComboSelection, (LPARAM) text);
								MessageBeep (0xFFFFFFFF);
								return 0;
							}
							else
							{
								gComboSelection = newsel;
								if (newsel!=CB_ERR)
								{
									HWND hwndClient = GetSDIClientWindow (hWin);
									SendMessage3ToClean (CcWmCOMBOSELECT, hwndClient, combo, newsel);
								}
								return 1;
							}
						}
						break;
				}
			} break;
		/*	WM_NOTIFY is handled identically as for MDI client windows (see MDIClientProcedure).
		*/
		case WM_NOTIFY:
			{
				LPNMHDR pnmh        = (LPNMHDR) lPara;
				LPTOOLTIPTEXT lpttt = (LPTOOLTIPTEXT) lPara;
				UINT from, flags;
				
				from = lpttt->hdr.idFrom;
				flags= lpttt->uFlags;

				if (pnmh->code == TTN_NEEDTEXT && from != 0 && flags != TTF_IDISHWND)
				{
					HWND hwndToolbar;

					hwndToolbar = (HWND)GetGWL_USERDATA (hWin);

					//	get tooltip text from Clean
					SendMessage2ToClean (CcWmGETTOOLBARTIPTEXT, hwndToolbar, from);
					lstrcpy (lpttt->szText,(LPSTR)gCci.p1);
					if (gCci.p1 != 0)
						rfree ((HGLOBAL) gCci.p1);

					return 0;
				}
				return (DefWindowProc (hWin, uMess, wPara, lPara));
			}
			break;
		case WM_CLOSE:		/*	The SDI frame window is requested to be closed. */
			{
				SendMessage1ToClean (CcWmPROCESSCLOSE, hWin);
				return 0;
			}
			break;
		case WM_DESTROY:	/*	The SDI frame window is in the act of being closed. */
			{
				ProcessShortcutTable shortcuts;

				ghTopDocWindow=NULL;

				shortcuts = (ProcessShortcutTable) GetWindowLong (hWin, 0);	// get the local shortcut table
				DestroyProcessShortcutTable (shortcuts);					// and destroy it.
				gAcceleratorTableIsUpToDate = FALSE;	// The active global accelerator table is not up to date

				DragAcceptFiles (hWin,FALSE);			/* Unregister for WM_DROPFILES events. */

				return 0;
			}
			break;
		/*	WM_ENTERIDLE message is used to let Clean evaluate the initialisation action
			of a modal dialog by sending the CcWmIDLEDIALOG message.
		*/
		case WM_ENTERIDLE:
			{
				HWND hwndModalDialog;
				
				hwndModalDialog = (HWND)lPara;

				if (wPara == MSGF_DIALOGBOX && hwndModalDialog != ghwndLastModalDialog)
				{
					SendMessage1ToClean (CcWmIDLEDIALOG,(int)hwndModalDialog);
					ghwndLastModalDialog = hwndModalDialog;
				}
				else
				{
					SendMessage0ToClean (CcWmIDLETIMER);
				}
				return 0;
			} break;
		case WM_ENTERMENULOOP:
			{
				gProhibitWindowActivation++;
			}
			break;
		case WM_EXITMENULOOP:
			{
				gProhibitWindowActivation--;
			}
			break;
		case WM_SYSCHAR:
		case WM_CHAR:
			{
				HWND hwndClient = GetSDIClientWindow (hWin);

				if (hwndClient != NULL)
				{
					if (gInKey)
					{
						if (gCurChar == (int) wPara)
							SendKeyStillDownToClean (hwndClient, hwndClient, gCurChar);
						else
						{
							SendKeyUpToClean (hwndClient, hwndClient, gCurChar);
							gCurChar = wPara;
							SendKeyDownToClean (hwndClient, hwndClient, gCurChar);
						}
					}
					else
					{
						gCurChar = wPara;
						SendKeyDownToClean (hwndClient, hwndClient, gCurChar);
						gInKey = TRUE;
					}
				}
			}
			break;
		case WM_SYSKEYUP:
		case WM_KEYUP:
			{
				HWND hwndClient = GetSDIClientWindow (hWin);
				if (hwndClient != NULL && gInKey)
				{
					SendKeyUpToClean (hwndClient, hwndClient, gCurChar);
				}
				gInKey = FALSE;
				gCurChar = 0;
				return (DefWindowProc (hWin, uMess, wPara, lPara));
			}
			break;
		case WM_KILLFOCUS:
			{
				HWND hwndClient = GetSDIClientWindow (hWin);
				if (hwndClient != NULL && gInKey)
				{
					SendKeyUpToClean (hwndClient, hwndClient, gCurChar);
				}
				gInKey = FALSE;
				gCurChar = 0;
			}
			break;
		case WM_LBUTTONDOWN:
			{
				HWND hwndClient = GetSDIClientWindow (hWin);
				if (hwndClient != NULL)
				{
					SendMouseDownToClean (hwndClient, hwndClient, SIGNEDLOWORD (lPara), SIGNEDHIWORD (lPara));
				}
			}
			break;
		case WM_MOUSEMOVE:
			{
				HWND hwndClient = GetSDIClientWindow (hWin);
				if (hwndClient != NULL)
				{
					if (gInMouseDown)
					{
						SendMouseStillDownToClean (hwndClient, hwndClient, SIGNEDLOWORD (lPara), SIGNEDHIWORD (lPara));
					}
					else
					{
						SendMouseStillUpToClean (hwndClient, hwndClient, SIGNEDLOWORD (lPara), SIGNEDHIWORD (lPara));
					}
				}
			}
			break;
		case WM_LBUTTONUP:
			{
				if (gInMouseDown)
				{
					ReleaseCapture ();	/* rely on WM_CAPTURECHANGED to send the mouseUp event */
				}
			}
			break;
		case WM_CANCELMODE:
			{
				if (gInMouseDown)
				{
					ReleaseCapture ();	/* rely on WM_CAPTURECHANGED to send the mouseUp event */
				}
				return DefWindowProc (hWin, uMess, wPara, lPara);
			}
			break;
		case WM_CAPTURECHANGED:
			{
				HWND hwndClient = GetSDIClientWindow (hWin);
				if (hwndClient != NULL && gInMouseDown)
				{
					POINT p;
					GetCursorPos (&p);
					ScreenToClient (hwndClient, &p);
					SendMouseUpToClean (hwndClient, hwndClient, p.x, p.y);
				}
			}
			break;
		case WM_TIMER:
			{
				HWND hwndClient = GetSDIClientWindow (hWin);
				if (hwndClient != NULL && (int) wPara == -1)
				{
					POINT p;
					GetCursorPos (&p);
					ScreenToClient (hwndClient, &p);
					SendMouseStillDownToClean (hwndClient, hwndClient, p.x, p.y);
				}
				else
				{
					rprintf ("Clean Window did not expect normal WM_TIMER message with id = %d.\n", wPara);
				}
			}
			break;
		case WM_ACTIVATE:
			{
				HWND hwndClient = GetSDIClientWindow (hWin);

				if (LOWORD (wPara) != WA_INACTIVE)
				{
					if (hwndClient != NULL)
						SendMessage1ToClean (CcWmACTIVATE, hwndClient);
					ghTopDocWindow       = hWin;		// PA: shouldn't this be hwndClient?
					ghActiveFrameWindow  = hWin;
					ghActiveClientWindow = NULL;		// PA: shouldn't this be hwndClient?
				}
				else if (LOWORD (wPara) == WA_INACTIVE)
				{
					if (hwndClient != NULL)
						SendMessage1ToClean (CcWmDEACTIVATE, hwndClient);
					ghTopDocWindow       = NULL;
					ghActiveFrameWindow  = NULL;
					ghActiveClientWindow = NULL;
					gAcceleratorTableIsUpToDate = FALSE; // The active global accelerator table is not up to date
				}
				return DefWindowProc (hWin, uMess, wPara, lPara);
			}
			break;
		/*	The WM_CREATE message should cause the SDI frame window only to create the accelerator table.
			The crosscall request from Clean to create the SDI window will create the SDI client. The
			SDI client will notify Clean that the controls can be created.
		*/
		case WM_CREATE:
			{
				ProcessShortcutTable shortcuts;

				shortcuts = AllocateProcessShortcutTable (MINSIZEPROCESSSHORTCUTTABLE);	// create a new shortcut table
				SetWindowLong (hWin, 0, (long) shortcuts);		//     and store it in the local memory of the window
				gAcceleratorTableIsUpToDate = FALSE;			// The active global accelerator table is not up to date
				
				ghActiveFrameWindow  = hWin;					// Keep track of the active frame window
				ghActiveClientWindow = NULL;					//  and client window
			}
			break;
		/*	The WM_SIZE message resizes the toolbar if present and makes sure that the SDI client 
			window is also resized by sending it the same size message, but with the toolbar height
			subtracted. 
		*/
		case WM_SIZE:
			{
				HWND hwndToolbar,hwndClient;
				RECT toolbarRect;
				int  toolbarHeight = 0;

				/*	Also resize the toolbar if present. */
				hwndToolbar = (HWND)GetGWL_USERDATA (hWin);
				
				if (hwndToolbar != NULL)
				{
					SendMessage (hwndToolbar, TB_AUTOSIZE, (WPARAM)0, (LPARAM)0);
					UpdateWindow (hwndToolbar);

					if (!GetWindowRect(hwndToolbar,&toolbarRect))
						rMessageBox (NULL,MB_APPLMODAL,"SDIFrameProcedure","GetWindowRect (hwndToolbar,_) failed");
					toolbarHeight = toolbarRect.bottom - toolbarRect.top;
				}

				hwndClient = GetSDIClientWindow (hWin);
				if (hwndClient != NULL)
				{
					SetWindowPos (hwndClient,												/* the SDI client */
								  HWND_BOTTOM,												/* this value is ignored (SWP_NOZORDER)  */
								  0,0,														/* these values are ignored (SWP_NOMOVE) */
								  (int)LOWORD (lPara),(int)HIWORD (lPara)-toolbarHeight,	/* new width and height */
								  SWP_NOACTIVATE | SWP_NOMOVE | SWP_NOZORDER
								 );
				}
			}
			break;
		/*	Accept the user dropping file(s) in the frame window.
		*/
		case WM_DROPFILES:
			{
				SendDropFilesToClean (hWin,wPara);
			}
			return 0;
			break;
	}
	return DefWindowProc (hWin,uMess,wPara,lPara);
}
/*	End of SDIFrameProcedure callback routine.
*/


/*	Callback routine for MDI document window procedure. 
	Copied almost straight from SDIWindowProcedure. Differences are:
	-	WM_COMMAND can not originate from menu selections (handled by MDIFrameProcedure);
	-	DefWindowProc must be DefMDIChildProc;
	-	Instead of WM_ACTIVATE, a MDI document window receives WM_MDIACTIVATE messages. However, these do not
		work well when dialogs are involved. Instead WM_NCACTIVATE messages are checked.
*/
static LRESULT CALLBACK
MDIWindowProcedure (HWND hWin,UINT uMess,WPARAM wPara,LPARAM lPara)
{
	printMessage ("Clean MDI Doc Window",hWin,uMess,wPara,lPara);
	switch (uMess)
	{
		case WM_COMMAND:
			{
				switch (HIWORD (wPara))
				{
					case BN_CLICKED:
						{
							if (lPara != 0)
							{
								/*	Send modifiers also to Clean */
								SendMessage4ToClean (CcWmBUTTONCLICKED, hWin, lPara, GetModifiers (), LOWORD (wPara));
							}
						}
						break;
					case 1:		/*	1: message originates from an accelerator */
						{
							SendMessage2ToClean (CcWmCOMMAND, LOWORD (wPara), GetModifiers ());
						}
						break;
					case CBN_SETFOCUS:
						{
							gComboSelection = SendMessage ((HWND) lPara, CB_GETCURSEL, 0, 0);
						}
						break;
					case CBN_KILLFOCUS:
						{
							gComboSelection = -1;
						}
						break;
					case CBN_SELENDOK:
						{
							char text[256];
							int newsel;
							HWND combo;

							combo = (HWND) lPara;
							newsel = SendMessage (combo, CB_GETCURSEL, 0, 0);
							SendMessage (combo, CB_GETLBTEXT, newsel, (LPARAM) text);
							if (!SendMessage (combo, CB_GETITEMDATA, newsel, 0))
							{
								SendMessage (combo, CB_SETCURSEL, gComboSelection, (LPARAM) text);
								MessageBeep (0xFFFFFFFF);
								return 0;
							}
							else
							{
								gComboSelection = newsel;
								if (newsel!=CB_ERR)
									SendMessage3ToClean (CcWmCOMBOSELECT, hWin, combo, newsel);
								return 1;
							}
						}
						break;
				}
			} break;
		case WM_PAINT:
			{
				RECT updaterect;
				HDC  hdc;
				PAINTSTRUCT ps;
				
				if (GetUpdateRect (hWin, &updaterect, FALSE))
				{
					hdc = BeginPaint (hWin, &ps);
					if (updaterect.left != updaterect.right && updaterect.top != updaterect.bottom)
						SendMessage6ToClean (CcWmPAINT, hWin, updaterect.left,updaterect.top,updaterect.right,updaterect.bottom,hdc);
					EndPaint (hWin, &ps);
				}
				else
				{	
					GetClientRect (hWin, &updaterect);
					SendMessage6ToClean (CcWmPAINT, hWin, updaterect.left,updaterect.top,updaterect.right,updaterect.bottom,wPara);
				}
				return 0;
			}
		case WM_CLOSE:		/*	The window is requested to be closed. */
			{
				SendMessage1ToClean (CcWmCLOSE, hWin);
				return 0;
			}
		case WM_DESTROY:	/*	The window is in the act of being closed. */
			{
				LocalWindowData wdata;

				wdata = (LocalWindowData) GetWindowLong (hWin,0);	// Get the local MDI window data
				DestroyLocalWindowData (wdata);						//	and destroy it.
				
				ghTopDocWindow=NULL;
				return 0;
			}
		case WM_ENTERMENULOOP:
			{
				gProhibitWindowActivation++;
			} break;
		case WM_EXITMENULOOP:
			{
				gProhibitWindowActivation--;
			} break;
		case WM_SETCURSOR:
			{
				int cursorcode;
				LocalWindowData wdata;

				if (gGlobalCursorCode == -1)
				{
					if ((HWND) wPara != hWin || LOWORD (lPara) != HTCLIENT)
					{
						return DefMDIChildProc (hWin, uMess, wPara, lPara);
					}
					wdata = (LocalWindowData) GetWindowLong (hWin,0);
					cursorcode = wdata->lwd_cursorcode;
				}
				else
				{
					cursorcode = gGlobalCursorCode;
				}
				SetCursorFromCode (cursorcode);
			}
			break;
		case WM_SYSCHAR:
		case WM_CHAR:
			{
				if (gInKey)
				{
					if (gCurChar == (int) wPara)
						SendKeyStillDownToClean (hWin, hWin, gCurChar);
					else
					{
						SendKeyUpToClean (hWin, hWin, gCurChar);
						gCurChar = wPara;
						SendKeyDownToClean (hWin, hWin, gCurChar);
					}
				}
				else
				{
					gCurChar = wPara;
					SendKeyDownToClean (hWin, hWin, gCurChar);
					gInKey = TRUE;
				}
				return 0;
			}
			break;
		case WM_SYSKEYUP:
		case WM_KEYUP:
			{
				if (gInKey)
					SendKeyUpToClean (hWin, hWin, gCurChar);
				gInKey = FALSE;
				gCurChar = 0;
				return (DefMDIChildProc (hWin, uMess, wPara, lPara));
			}
			break;
		case WM_KILLFOCUS:
			{
				if (gInKey)
					SendKeyUpToClean (hWin, hWin, gCurChar);
				gInKey = FALSE;
				gCurChar = 0;
			}
			break;
		case WM_LBUTTONDOWN:
			{
				SendMouseDownToClean (hWin, hWin, SIGNEDLOWORD (lPara), SIGNEDHIWORD (lPara));
				return 0;
			}
			break;
		case WM_MOUSEMOVE:
			{
				if (gInMouseDown)
				{
					SendMouseStillDownToClean (hWin, hWin, SIGNEDLOWORD (lPara), SIGNEDHIWORD (lPara));
				}
				else
				{
					SendMouseStillUpToClean (hWin, hWin, SIGNEDLOWORD (lPara), SIGNEDHIWORD (lPara));
				}
				return 0;
			}
			break;
		case WM_LBUTTONUP:
			{
				if (gInMouseDown)
				{
					ReleaseCapture ();	/* rely on WM_CAPTURECHANGED to send the mouseUp event */
				}
			}
			break;
		case WM_CANCELMODE:
			{
				if (gInMouseDown)
				{
					ReleaseCapture ();	/* rely on WM_CAPTURECHANGED to send the mouseUp event */
				}
				return DefMDIChildProc (hWin, uMess, wPara, lPara);
			}
			break;
		case WM_CAPTURECHANGED:
			{
				if (gInMouseDown)
				{
					POINT p;
					GetCursorPos (&p);
					ScreenToClient (hWin, &p);
					SendMouseUpToClean (hWin, hWin, p.x, p.y);
				}
			}
			break;
		case WM_TIMER:
			{
				if ((int) wPara == -1)
				{
					POINT p;
					GetCursorPos (&p);
					ScreenToClient (hWin, &p);
					SendMouseStillDownToClean (hWin, hWin, p.x, p.y);
				}
				else
				{
					rprintf ("Clean Window did not expect normal WM_TIMER message with id = %d.\n", wPara);
				}
			}
			break;
		/*	Instead of using WM_MDIACTIVATE to know the activation state of a MDI document window, 
			we use WM_NCACTIVATE. This is because WM_MDIACTIVATE events are  not generated when dialogs
			are involved. The WM_NCACTIVATE is more robust because it is used by the system to change
			the active state of the window title bar. Incidentally to many WM_NCACTIVATE message can be
			generated. This has to be dealt with on the Clean side.
		*/
		case WM_NCACTIVATE:
			{
				BOOL fActive = (BOOL)wPara;

				if (fActive)
				{
					SendMessage1ToClean (CcWmACTIVATE, hWin);
					ghTopDocWindow       = hWin;
					ghActiveClientWindow = GetParent (hWin);
					ghActiveFrameWindow  = GetParent (ghActiveClientWindow);
					UpdateWindow (hWin);		// enforce update at Clean side (necessary for setActiveWindow)
				}
				else
				{
					SendMessage1ToClean (CcWmDEACTIVATE, hWin);
					ghTopDocWindow       = NULL;
					ghActiveFrameWindow  = NULL;
					ghActiveClientWindow = NULL;
				}
				return DefMDIChildProc (hWin, uMess, wPara, lPara);
			}
			break;
		/*	The WM_CREATE message notifies Clean that its controls can be built. 
			In addition, the LocalWindowData is created and stored in the local memory of the MDI window.
		*/
		case WM_CREATE:
			{
				LocalWindowData wdata;

				wdata = AllocateLocalWindowData ();			// create the LocalWindowData struct
				SetWindowLong (hWin, 0, (long) wdata);		//	and store it in the local memory of the window

				SendMessage1ToClean (CcWmCREATE, hWin);

				/*	After creation of the window controls, their HFONT should be set to 8pt "MS Sans Serif" */
				EnumChildWindows (hWin, SetControlFontProc, (LPARAM) gControlFont);
			}
			break;
		/*	The cases WM_ENTERSIZEMOVE and WM_EXITSIZEMOVE flag the lwd_usersizemove field
			of the LocalWindowData of the window. This is used to determine whether the window
			should be redrawn in case of resizing. 
		*/
		case WM_ENTERSIZEMOVE:
			{
				LocalWindowData wdata;

				wdata = (LocalWindowData) GetWindowLong (hWin,0);
				wdata->lwd_usersizemoving = (BOOL)TRUE;
				SetWindowLong (hWin, 0, (long)wdata);
			}
			break;
		case WM_EXITSIZEMOVE:
			{
				LocalWindowData wdata;

				wdata = (LocalWindowData) GetWindowLong (hWin,0);
				wdata->lwd_usersizemoving = (BOOL)FALSE;
				SetWindowLong (hWin, 0, (long)wdata);
			}
			break;
		case WM_SIZE:
			{
				HWND hwndToolbar;

				hwndToolbar = (HWND) GetGWL_USERDATA (hWin);
				// First resize the toolbar if present
				if (hwndToolbar!=NULL)
					SendMessage (hwndToolbar, TB_AUTOSIZE, (WPARAM)0, (LPARAM)0);

				if (wPara != SIZE_MAXHIDE && wPara != SIZE_MAXSHOW)
				{
					int width,height;
					LocalWindowData wdata;

					width  = LOWORD (lPara);		// Width  of window excluding vertical scrollbar
					height = HIWORD (lPara);		// Height of window excluding horizontal scrollbar
					wdata  = (LocalWindowData) GetWindowLong (hWin,0);
					UpdateWindow (hWin);			// But first update the window
					SendMessage4ToClean (CcWmSIZE, hWin, width, height, (int)wdata->lwd_usersizemoving);
				}
			}
			break;
		case WM_HSCROLL:
			{
				int nPos,nScrollCode,controlkind;
				HWND hwndScrollBar;

				nScrollCode = LOWORD (wPara);

				if (nScrollCode != SB_ENDSCROLL)	/* Do not send the SB_ENDSCROLL to Clean. */
				{
					nPos = (short int) HIWORD (wPara);
					hwndScrollBar = (HWND) lPara;

					if (hwndScrollBar==0)
					{
						controlkind = SB_HORZ;		/* lPara==0 in case of Window scrollbars. */
						hwndScrollBar = hWin;		/* pass the window handle to Clean. */
						UpdateWindow (hWin);		/* but first ensure that window is updated. */
					}
					else
					{
						controlkind = SB_CTL;		/* lPara!=0 in case of SliderControls. */
					}
					SendMessage5ToClean (CcWmSCROLLBARACTION, hWin, hwndScrollBar, controlkind, nScrollCode, nPos);
				}
			}
			break;
		case WM_VSCROLL:
			{
				int nPos,nScrollCode,controlkind;
				HWND hwndScrollBar;

				nScrollCode = LOWORD (wPara);

				if (nScrollCode != SB_ENDSCROLL)	/* Do not send the SB_ENDSCROLL to Clean. */
				{
					nPos = (short int) HIWORD (wPara);
					hwndScrollBar = (HWND) lPara;

					if (hwndScrollBar==0)
					{
						controlkind = SB_VERT;		/* lPara==0 in case of Window scrollbars. */
						hwndScrollBar = hWin;		/* pass the window handle to Clean. */
						UpdateWindow (hWin);		/* but first ensure that window is updated. */
					}
					else
					{
						controlkind = SB_CTL;		/* lPara!=0 in case of SliderControls. */
					}
					SendMessage5ToClean (CcWmSCROLLBARACTION, hWin, hwndScrollBar, controlkind, nScrollCode, nPos);
				}
			}
			break;
		case WM_DRAWITEM:
			{
				LPDRAWITEMSTRUCT lpdis;
				lpdis = (LPDRAWITEMSTRUCT) lPara;

				switch (lpdis->CtlType)
				{
					case ODT_COMBOBOX:
						{
							char text[256];
							COLORREF forecolor, bkcolor;
							SendMessage (lpdis->hwndItem, CB_GETLBTEXT, lpdis->itemID, (LPARAM) text);
							if (lpdis->itemState & ODS_DISABLED)
							{
								forecolor = SetTextColor (lpdis->hDC, GetSysColor (COLOR_GRAYTEXT));
								bkcolor = SetBkColor (lpdis->hDC, GetSysColor (COLOR_3DFACE));
							}
							else if (lpdis->itemState & ODS_SELECTED)
							{
								if (lpdis->itemData)
								{
									forecolor = SetTextColor (lpdis->hDC, GetSysColor (COLOR_HIGHLIGHTTEXT));
									bkcolor = SetBkColor (lpdis->hDC, GetSysColor (COLOR_HIGHLIGHT));
								}
								else
								{
									forecolor = SetTextColor (lpdis->hDC, GetSysColor (COLOR_GRAYTEXT));
									bkcolor = SetBkColor (lpdis->hDC, GetSysColor (COLOR_WINDOW));
								}
							}
							else
							{
								if (lpdis->itemData)
									forecolor = SetTextColor (lpdis->hDC, GetSysColor (COLOR_WINDOWTEXT));
								else
									forecolor = SetTextColor (lpdis->hDC, GetSysColor (COLOR_GRAYTEXT));
								bkcolor = SetBkColor (lpdis->hDC, GetSysColor (COLOR_WINDOW));
							}

							ExtTextOut (lpdis->hDC,					/* device context */
										lpdis->rcItem.left + 2,		/* ref point x */
										lpdis->rcItem.top + 1,		/* ref point y */
										ETO_CLIPPED | ETO_OPAQUE,	/* options */
										&lpdis->rcItem,				/* clipping rect */
										text,						/* text to draw */
										lstrlen (text),				/* length of text to draw */
										NULL						/* no kerning array */
								);

							SetTextColor (lpdis->hDC, forecolor);
							SetBkColor (lpdis->hDC, bkcolor);

							if (lpdis->itemState & ODS_FOCUS)
								DrawFocusRect (lpdis->hDC, &lpdis->rcItem);

						} break;
					case ODT_BUTTON:
						{
							{
								SendMessage3ToClean (CcWmDRAWCONTROL, hWin, lpdis->hwndItem, lpdis->hDC);
								 
								if (lpdis->itemState & ODS_SELECTED)
									InvertRect (lpdis->hDC, &lpdis->rcItem);
								
								if (lpdis->itemState & ODS_FOCUS)
									DrawFocusRect (lpdis->hDC, &lpdis->rcItem);
							};
							return TRUE;
						} break;
				}
			} break;
	}
	return DefMDIChildProc (hWin,uMess,wPara,lPara);
}
/*	End of MDIWindowProcedure callback routine.
*/

/*	Callback routine for MDI frame window procedure. */
static LRESULT CALLBACK
MDIFrameProcedure (HWND hWin,UINT uMess,WPARAM wPara,LPARAM lPara)
{
	printMessage ("Clean MDIFrameWindow",hWin,uMess,wPara,lPara);
	switch (uMess)
	{
		/*	WM_ENTERIDLE message is used to let Clean evaluate the initialisation action
			of a modal dialog by sending the CcWmIDLEDIALOG message.
		*/
		case WM_ENTERIDLE:
			{
				HWND hwndModalDialog;
				
				hwndModalDialog = (HWND)lPara;

				if (wPara == MSGF_DIALOGBOX && hwndModalDialog != ghwndLastModalDialog)
				{
					SendMessage1ToClean (CcWmIDLEDIALOG,(int)hwndModalDialog);
					ghwndLastModalDialog = hwndModalDialog;
				}
				else
				{
					SendMessage0ToClean (CcWmIDLETIMER);
				}
				return 0;
			} break;
		case WM_ENTERMENULOOP:
			{
				gProhibitWindowActivation++;
			} break;
		case WM_EXITMENULOOP:
			{
				gProhibitWindowActivation--;
			} break;
		case WM_COMMAND:
			{
				if (HIWORD (wPara)==0 && lPara!=0)
				{
					HWND hwndToolbar;

					hwndToolbar = (HWND)GetGWL_USERDATA (hWin);	// Obtain the toolbar handle
					if (hwndToolbar != 0)
						SendMessage4ToClean (CcWmBUTTONCLICKED, hWin, lPara, GetModifiers (), LOWORD (wPara));
				}
				else
				{
					switch (wPara)
					{
						case (OSMenuIDEnd+1):
							{
								SendMessage (GetWindow (hWin,GW_CHILD),WM_MDIICONARRANGE,0,0);
							} break;
						case (OSMenuIDEnd+2):
							{
								SendMessage (GetWindow (hWin,GW_CHILD),WM_MDITILE,(WPARAM) (UINT) MDITILE_VERTICAL,0);
							} break;
						case (OSMenuIDEnd+3):
							{
								SendMessage (GetWindow (hWin,GW_CHILD),WM_MDITILE,(WPARAM) (UINT) MDITILE_HORIZONTAL,0);
							} break;
						case (OSMenuIDEnd+4):
							{
								SendMessage (GetWindow (hWin,GW_CHILD),WM_MDICASCADE,0,0);
							} break;
						default:
							SendMessage2ToClean (CcWmCOMMAND, LOWORD (wPara), GetModifiers ()); 
					}
				}
			} break;
		/* WM_CREATE should create the client window, the menu bar, and the "Window" menu. */
		case WM_CREATE:
			{
				CLIENTCREATESTRUCT clientcreate;
				HMENU menuBar, windowMenu;				// The handle to the menu bar and the "Window" menu
				ProcessShortcutTable shortcuts;			// New
				HWND hwndClient;						// New

				menuBar = CreateMenu ();				// Create the menu bar
				SetMenu (hWin,menuBar);					// and associate it with the frame window
				windowMenu = CreatePopupMenu ();		// Create the "Window" menu
				InsertMenu (menuBar,					// add it to the menuBar
							0xFFFFFFFF,					// at the end
							MF_BYPOSITION | MF_POPUP,	// Flags
							(UINT) windowMenu,			// the "Window" menu
							"&Window"					// and set its title
					);
				InsertMenu (windowMenu,0,MF_BYPOSITION | MF_STRING,OSMenuIDEnd+1,"Arrange &Icons");		// Add "Arrange Icons" command
				InsertMenu (windowMenu,0,MF_BYPOSITION | MF_STRING,OSMenuIDEnd+2,"&Tile Vertically");	// Add "Tile Vertically" command
				InsertMenu (windowMenu,0,MF_BYPOSITION | MF_STRING,OSMenuIDEnd+3,"Tile &Horizontally");	// Add "Tile Horizontally" command
				InsertMenu (windowMenu,0,MF_BYPOSITION | MF_STRING,OSMenuIDEnd+4,"&Cascade");			// Add "Cascade" command

				clientcreate.hWindowMenu  = windowMenu;
				clientcreate.idFirstChild = OSMenuIDEnd+5;	// Window ids must be generated from OSMenuIDEnd+5

				hwndClient = CreateWindow (	"MDICLIENT",								// The MDICLIENT window class
											NULL,										// The window name
											WS_CHILD | WS_CLIPCHILDREN | WS_VISIBLE,	// Style parameters
											0,0,										// position (x,y)
											0,0,										// size (w,h)
											hWin,										// The frame window is the parent
											NULL,										// The menu (none at the moment)
											(HANDLE) ghInst,							// Instance that owns the window
											(LPSTR) &clientcreate						// The CLIENTCREATESTRUCT
											);

				shortcuts = AllocateProcessShortcutTable (MINSIZEPROCESSSHORTCUTTABLE);	// create a new shortcut table
				SetWindowLong (hWin, 0, (long) shortcuts);		//     and store it in the local memory of the window
				gAcceleratorTableIsUpToDate = FALSE;			// The active global accelerator table is not up to date

				ghActiveFrameWindow  = hWin;					// Keep track of the active frame window
				ghActiveClientWindow = hwndClient;				//	and client window

			} return 0;
		case WM_CLOSE:
			{
				SendMessage1ToClean (CcWmPROCESSCLOSE, hWin);
				return 0;
			}
			break;
		case WM_DESTROY:	/*	The frame is in the act of being closed. */
			{
				ProcessShortcutTable shortcuts;
			
				shortcuts = (ProcessShortcutTable) GetWindowLong (hWin, 0);	// get the local shortcut table
				DestroyProcessShortcutTable (shortcuts);					// and destroy it.
				gAcceleratorTableIsUpToDate = FALSE;	// The active global accelerator table is not up to date

				DragAcceptFiles (hWin,FALSE);			/* Unregister for WM_DROPFILES events. */
			}
			break;
		/*
		case WM_ACTIVATE:	// This alternative should only administer the current active frame/client window. 
			{
				if (LOWORD (wPara)!=WA_INACTIVE)
				{
					ghActiveFrameWindow  = hWin;
					ghActiveClientWindow = GetWindow (ghActiveFrameWindow,GW_CHILD);
					ghTopDocWindow       = GetWindow (ghActiveClientWindow,GW_CHILD);
				}
				else
				{
					ghTopDocWindow       = NULL;
					ghActiveFrameWindow  = NULL;
					ghActiveClientWindow = NULL;
					gAcceleratorTableIsUpToDate = FALSE;	// The active global accelerator table is not up to date
				}
			}
			break;
		*/
		case WM_NOTIFY:
			{
				LPNMHDR pnmh        = (LPNMHDR) lPara;
				LPTOOLTIPTEXT lpttt = (LPTOOLTIPTEXT) lPara;
				UINT from, flags;
				
				from = lpttt->hdr.idFrom;
				flags= lpttt->uFlags;

				if (pnmh->code == TTN_NEEDTEXT && from != 0 && flags != TTF_IDISHWND)
				{
					HWND hwndToolbar;

					hwndToolbar = (HWND)GetGWL_USERDATA (hWin);

					//	get tooltip text from Clean
					SendMessage2ToClean (CcWmGETTOOLBARTIPTEXT, hwndToolbar, from);
					lstrcpy (lpttt->szText,(LPSTR)gCci.p1);
					if (gCci.p1 != 0)
						rfree ((HGLOBAL) gCci.p1);
				}
			}
			break;
		case WM_SIZE:
			{
				HWND hwndToolbar;

				hwndToolbar = (HWND)GetGWL_USERDATA (hWin);
				if (hwndToolbar != NULL)
					SendMessage ((HWND)GetGWL_USERDATA (hWin), TB_AUTOSIZE, (WPARAM)0, (LPARAM)0);
			}
			break;
		/*	Accept the user dropping file(s) in the frame window. */
		case WM_DROPFILES:
			{
				SendDropFilesToClean (hWin,wPara);
			}
			break;
		default:
			return DefFrameProc (hWin, GetWindow (hWin,GW_CHILD), uMess, wPara, lPara);
			break;
	}
	return DefFrameProc (hWin, GetWindow (hWin,GW_CHILD), uMess, wPara, lPara);
}
/*	End of MDIFrameProcedure callback routine.
*/


static BOOL
RbrtTranslateMessage (MSG * pmsg)
{
	int c;
	UINT msg;

	c   = 0;
	msg = pmsg->message;
	if (msg==WM_SYSKEYDOWN || msg==WM_KEYDOWN)
	{
		c = CheckVirtualKeyCode ((int) pmsg->wParam);
	}
	if (c)
	{
		gStoredMess = *pmsg;
		gStoredMess.wParam = (WPARAM) c;

		if (pmsg->message == WM_SYSKEYDOWN)
			gStoredMess.message = WM_SYSCHAR;
		else
			gStoredMess.message = WM_CHAR;

		gMessStored = TRUE;
		return TRUE;
	}
	else
		return TranslateMessage (pmsg);
}

/*	EndSuspendTimerProc is the parameter of the SetTimer procedure used 
	in RbrtGetMessage to set a positive timer interval (gSleepTime).
	The WaitMessage routine suspends the OS thread until an interesting
	event has occurred or the timer has been triggered. In that case
	EndSuspendTimerProc is called, which kills the timer and informs
	Clean about the timer event. 
*/
static VOID CALLBACK EndSuspendTimerProc (HWND hwnd, UINT uMsg, UINT idEvent, DWORD dwTime )
{
	KillTimer (ghMainWindow, (UINT) -2);
	SendMessage0ToClean (CcWmIDLETIMER);
};

static int
RbrtGetMessage (MSG * pmsg)
{
	if (gMessStored)
	{
		*pmsg = gStoredMess;
		gMessStored = FALSE;
		return TRUE;
	}

	if (gSleeptime==0 && !PeekMessage (pmsg, NULL, 0, 0, PM_NOREMOVE))
	{
		POINT p;
		GetCursorPos (&p);
		pmsg->hwnd    = ghMainWindow;
		pmsg->message = WM_ENTERIDLE;
		pmsg->wParam  = MSGF_USER;
		pmsg->lParam  = (LPARAM) ghMainWindow;
		pmsg->time    = GetTickCount ();
		pmsg->pt      = p;
		return TRUE;
	}
	if (!gIdleTimerOn)
	{
		return GetMessage (pmsg, NULL, 0, 0);
	}
	else
	{
		if (PeekMessage (pmsg, NULL, 0, 0, PM_REMOVE))
			return (pmsg->message != WM_QUIT);
		else
		{
			POINT p;
			
			/*	The following code has been inserted to reduce the crosscall traffic.
				A timer is set to suspend this thread until atleast the timer interval
				has elapsed. 
			*/
			if (SetTimer (ghMainWindow, (UINT) -2, (UINT)gSleeptime, &EndSuspendTimerProc))
			{
				WaitMessage ();
			}
			else
			{
				rMessageBox (NULL,MB_APPLMODAL,"RbrtGetMessage","SetTimer failed to create timer");
			}
			/*	End of insertion.
			*/

			GetCursorPos (&p);

			pmsg->hwnd    = ghMainWindow;
			pmsg->message = WM_ENTERIDLE;
			pmsg->wParam  = MSGF_USER;
			pmsg->lParam  = (LPARAM) ghMainWindow;
			pmsg->time    = GetTickCount ();
			pmsg->pt      = p;
			return TRUE;
		}
	}
}

static UINT APIENTRY FileSelectorHook (HWND hdlg, UINT uiMsg, WPARAM wParam, LPARAM lParam)
{
	if (uiMsg == WM_INITDIALOG)
	{
	//	HWND	window_handle;
		RECT	rect;
		int		x, y;

	//	window_handle	= GetParent (hdlg);
		
	//	GetWindowRect (window_handle, &rect);
		GetWindowRect (hdlg, &rect);
		
		x	= (GetSystemMetrics (SM_CXSCREEN)>>1) - ((rect.right-rect.left)>>1);
		y	= (GetSystemMetrics (SM_CYSCREEN)>>1) - ((rect.bottom-rect.top)>>1);
	//	x	= GetSystemMetrics (SM_CXSCREEN)>>1;
	//	y	= GetSystemMetrics (SM_CYSCREEN)>>1;

	//	SetWindowPos (window_handle, NULL, x, y, 0, 0, SWP_NOACTIVATE | SWP_NOSIZE | SWP_NOZORDER);
		SetWindowPos (hdlg, NULL, x, y, 0, 0, SWP_NOACTIVATE | SWP_NOSIZE | SWP_NOZORDER);
	}

	return 0;
}

/*	UpdateWindowScrollbars updates any window scrollbars and non-client area if present.
*/
static void 
UpdateWindowScrollbars (HWND hwnd)
{
	int w,h;
	RECT rect;

	GetWindowRect (hwnd, &rect);
	w = rect.right -rect.left;
	h = rect.bottom-rect.top;

	if (WindowHasHScroll (hwnd))
	{
		rect.left   = 0;
		rect.top    = h-GetSystemMetrics (SM_CYHSCROLL);
		rect.right  = w;
		rect.bottom = h;
		InvalidateRect (hwnd,&rect,FALSE);
		RedrawWindow (hwnd,&rect,NULL,RDW_FRAME | RDW_VALIDATE | RDW_UPDATENOW | RDW_NOCHILDREN);
		ValidateRect (hwnd,&rect);
	}
	if (WindowHasVScroll (hwnd))
	{
		rect.left   = w-GetSystemMetrics (SM_CXVSCROLL);
		rect.top    = 0;
		rect.right  = w;
		rect.bottom = h;
		InvalidateRect (hwnd,&rect,FALSE);
		RedrawWindow (hwnd,&rect,NULL,RDW_FRAME | RDW_VALIDATE | RDW_UPDATENOW | RDW_NOCHILDREN);
		ValidateRect (hwnd,&rect);
	}
}


void
HandleCleanRequest (CrossCallInfo * pcci)
{
	switch (pcci->mess)
	{
/* Mike... */
		case CcRqRUNGAME:         	/* int int int  (obsolete)
										   no result. */
            {
                RunGame ();
			    MakeReturn0Cci (pcci);
            }
            break;

        case CcRqPLAYSOUNDSAMPLE:   // id, vol, pan, freq, delay
                                    // no result
            {
             	int id = pcci->p1;
                int vol = pcci->p2;
                int pan = pcci->p3;
                int freq = pcci->p4;
                int delay = pcci->p5;

                PlaySoundSample (id, vol, pan, freq, delay);

			    MakeReturn0Cci (pcci);
            }
            break;

        case CcRqCREATEGAMEOBJECT:   /* mapval, above, x, y
                                        no result */
            {
             	int mapval = pcci->p1;
                int above  = pcci->p2;
                int x      = pcci->p3;
                int y      = pcci->p4;
                int result = 0;

                CreateGameObject (mapval, above, x, y, &result);

			    MakeReturn0Cci (pcci);
            }
            break;

        case CcRqUSERGAMEEVENT:   /* event, par1, par2, target, subtarget, time
                                     no result */
            {
                int event = pcci->p1;
                int par1 = pcci->p2;
                int par2 = pcci->p3;
                int target = pcci->p4;
              /* added 01/11/99... */
                int subtarget = pcci->p5;
                int time = pcci->p6;

                ScheduleUserGameEvent (event, par1, par2, target, subtarget, time);
              /* ...added 01/11/99 */

			    MakeReturn0Cci (pcci);
            }
            break;

		case CcRqCREATEGAMEWINDOW:	/*	width, height, bpp, fullscreen
										HWND result. */
			{
                int w = pcci->p1;
                int h = pcci->p2;
                int bpp = pcci->p3;
                int fs = (BOOL) pcci->p4;
                int result;

                result = CreateGameWindow (w, h, bpp, fs);

            	MakeReturn1Cci (pcci, result);
			}
			break;
/* ...Mike */
		case CcRqBEGINPAINT:	/* hwnd; HDC result. */
			{
				HDC hdc;
				hdc = BeginPaint ((HWND) pcci->p1, &gPaintStruct);
				MakeReturn1Cci (pcci, (int) hdc);
			}
			break;
		case CcRqENDPAINT:		/* hwnd; no result.  */
			{
				EndPaint ((HWND) pcci->p1, &gPaintStruct);
				MakeReturn0Cci (pcci);
			}
			break;
		case CcRqFAKEPAINT:		/* hwnd; no result. */
			{
				HWND hwnd = (HWND) pcci->p1;

				BeginPaint (hwnd, &gPaintStruct);
				EndPaint (hwnd,&gPaintStruct);
				InvalidateRect (hwnd, NULL, FALSE);

				MakeReturn0Cci (pcci);
			}
			break;
		case CcRqDOMESSAGE: 	// idleTimerOn, sleeptime; no result. 
			{
				MSG ms;
				int msgresult;

				gIdleTimerOn = (BOOL) pcci->p1;
				gSleeptime   = (int) pcci->p2;
				msgresult = RbrtGetMessage (&ms);

				if (msgresult == -1)
					ErrorExit ("Fatal error: CcRqDoMessage, GetMessage result is -1");
				else if (msgresult == FALSE)
					ErrorExit ("Fatal error: CcRqDoMessage, GetMessage result is FALSE (WM_QUIT)");
				else
				{
					LONG msgtime = GetMessageTime ();

					gPrevMsgTime = msgtime;

					if (!gActiveDialog || !IsDialogMessage (gActiveDialog, &ms))
					{
						if (ghActiveClientWindow==NULL || !TranslateMDISysAccel (ghActiveClientWindow, &ms))
						{
							UpdateAcceleratorTable ();	// Verify the correctness of gAcceleratorTable
						
							if (gAcceleratorTable==NULL || !TranslateAccelerator (ghActiveFrameWindow, gAcceleratorTable, &ms))
							{
#ifdef HILT
								if (!htmlHelpInitialized || !HtmlHelp (NULL,NULL,HH_PRETRANSLATEMESSAGE,(DWORD)&ms))
#endif
								{	
										RbrtTranslateMessage (&ms);
										DispatchMessage (&ms);
									}
							}
						}
					}
					MakeReturn0Cci (pcci);
				}
			}
			break;
		/*	Add a shortkey to a framewindow shortkey table. */
		case CcRqADDMENUSHORTKEY:	/* frameptr, cmd, key; no result. */
			{
				ProcessShortcutTable table;
				HWND frameptr;
				int cmd, key;

				frameptr = (HWND) pcci->p1;
				cmd      = pcci->p2;
				key      = pcci->p3;

				table = (ProcessShortcutTable) GetWindowLong (frameptr,0);
				table = AddProcessShortcut (key, cmd, table);
				SetWindowLong (frameptr, 0, (long)table);

				if (gAcceleratorTableIsUpToDate)
				{
					gAcceleratorTableIsUpToDate == !(ghActiveFrameWindow==frameptr);
				}

				MakeReturn0Cci (pcci);
			}
			break;
		/*	Remove a shortkey from a framewindow shortkey table. */
		case CcRqREMOVEMENUSHORTKEY:	/* frameptr, cmd; no result. */
			{
				ProcessShortcutTable table;
				HWND frameptr;
				int cmd;

				frameptr = (HWND) pcci->p1;
				cmd      = pcci->p2;

				table    = (ProcessShortcutTable) GetWindowLong (frameptr,0);
				table    = RemoveProcessShortcut (cmd, table);
				SetWindowLong (frameptr, 0, (long)table);

				if (gAcceleratorTableIsUpToDate)
				{
					gAcceleratorTableIsUpToDate == !(ghActiveFrameWindow==frameptr);
				}

				MakeReturn0Cci (pcci);
			}
			break;
		case CcRqMODIFYMENUITEM:	/* hitem, hmenu, textptr; no result.	*/
			{
				MENUITEMINFO info;

				info.cbSize        = (UINT) sizeof (MENUITEMINFO);
				info.fMask         = MIIM_TYPE;
				info.fType         = MFT_STRING;
				info.fState        = (UINT) 0;
				info.wID           = (UINT) pcci->p1;
				info.hSubMenu      = (HMENU) NULL;
				info.hbmpChecked   = NULL;
				info.hbmpUnchecked = NULL;
				info.dwItemData    = (DWORD) 0;
				info.dwTypeData    = (LPTSTR) pcci->p3;//correcttext;
				info.cch           = rstrlen ((LPTSTR)pcci->p3);//(correcttext);

				SetMenuItemInfo ((HMENU) pcci->p2, (UINT) pcci->p1, FALSE, &info);

				MakeReturn0Cci (pcci);
			} break;
		case CcRqINSERTMENUITEM:		/* on/off, hmenu, textptr, marked, pos;    HITEM result. */
			{
				UINT graystate, checkstate;
				HITEM hitem;

				if (pcci->p1)
				{
					graystate = MF_ENABLED;
				}
				else
				{
					graystate = MF_GRAYED;
				}

				if (pcci->p4)
				{
					checkstate = MF_CHECKED;
				}
				else
				{
					checkstate = MF_UNCHECKED;
				}

				hitem = NextMenuItemID ();		/*	PA: replaced NextItemHandle by NextMenuItemID. */

				InsertMenu ((HMENU) pcci->p2,			/* hMenu	  */
							(UINT) pcci->p5,			/* position   */
							MF_BYPOSITION | MF_STRING | graystate | checkstate,
														/* Flags	  */
							(UINT) hitem,				/* id		  */
							(LPCTSTR) pcci->p3
					);
				MakeReturn1Cci (pcci, hitem);
			}
			break;
		case CcRqITEMENABLE:	/* parent, HITEM, onoff; no result.  */
			{
				UINT greystate;

				if (pcci->p3)
				{
					greystate = MF_ENABLED;
				}
				else
				{
					greystate = MF_GRAYED;
				}
				EnableMenuItem ((HMENU) pcci->p1,			/* parent menu	*/
								(UINT) pcci->p2,			/* menu item id */
								MF_BYCOMMAND | greystate);	/* flags		*/
				MakeReturn0Cci (pcci);
			}
			break;
		/*	Destroy a menu 'physically' */
		case CcRqDESTROYMENU:			/* HMENU; no result. */
			{
				DestroyMenu ((HMENU) pcci->p1);
				MakeReturn0Cci (pcci);
			}
			break;
		/*	Remove a menu logically */
		case CcRqDELETEMENU:			/* HMENU, HITEM; no result. */
			{
				DeleteMenu ((HMENU) pcci->p1,		/* parent menu  */
							(UINT) pcci->p2,		/* menu item id */
							MF_BYCOMMAND);
				MakeReturn0Cci (pcci);
			}
			break;
		case CcRqREMOVEMENUITEM:		/* menu, HITEM; no result. */
			{
				RemoveMenu ((HMENU) pcci->p1,
							(UINT) pcci->p2,
							MF_BYCOMMAND);
				MakeReturn0Cci (pcci);
			}
			break;
		case CcRqINSERTSEPARATOR:		/* hmenu, pos no result. */
			{
				InsertMenu ((HMENU) pcci->p1,				/* hMenu				 */
							(UINT) pcci->p2,				/* position 			 */
							MF_BYPOSITION | MF_SEPARATOR,	/* Flags				 */
							0,								/* no id for separator	 */
							NULL);							/* no text for separator */
				MakeReturn0Cci (pcci);
			}
			break;
		case CcRqMODIFYMENU:	/* hitem, hmenu, textptr; no result.	*/
			{
				MENUITEMINFO info;

				info.cbSize        = (UINT) sizeof (MENUITEMINFO);
				info.fMask         = MIIM_TYPE;
				info.fType         = MFT_STRING;
				info.fState        = (UINT) 0;
				info.wID           = (UINT) pcci->p1;
				info.hSubMenu      = (HMENU) pcci->p2;
				info.hbmpChecked   = NULL;
				info.hbmpUnchecked = NULL;
				info.dwItemData    = (DWORD) 0;
				info.dwTypeData    = (LPTSTR) pcci->p3;
				info.cch           = rstrlen ((LPTSTR)pcci->p3);

				SetMenuItemInfo ((HMENU) pcci->p2, (UINT) pcci->p1, FALSE, &info);

				MakeReturn0Cci (pcci);
			} break;
		/*	Insert a menu into the menu bar. */
		case CcRqINSERTMENU:	/* on/off, hmenu, textptr, hsubmenu, pos; no result */
			{
				UINT graystate;

				graystate = 0;
				if (pcci->p1)
				{
					graystate = MF_ENABLED;
				}
				else
				{
					graystate = MF_GRAYED;
				}

				InsertMenu ((HMENU) pcci->p2,						/* hMenu		*/
							(UINT) pcci->p5,						/* position		*/
							MF_BYPOSITION | MF_POPUP | graystate,	/* flags		*/
							(UINT) pcci->p4,						/* hSubMenu		*/
							(LPCTSTR) pcci->p3
					);
				MakeReturn0Cci (pcci);
			} break;
		case CcRqMENUENABLE:	/* parent, zero based position of menu, onoff; no result. */
			{
				UINT greystate;

				if (pcci->p3)
				{
					greystate = MF_ENABLED;
				}
				else
				{
					greystate = MF_GRAYED;
				}
				EnableMenuItem ((HMENU) pcci->p1,			/* parent menu	*/
								(UINT) pcci->p2,			/* submenu	 	*/
								MF_BYPOSITION | greystate);	/* flags		*/
				MakeReturn0Cci (pcci);
			}
			break;
		case CcRqDRAWMBAR:		/* framePtr, clientPtr; no result. */
			{
				HWND framePtr  = (HWND) pcci->p1;
				HWND clientPtr = (HWND) pcci->p2;

				if (clientPtr != 0)
					SendMessage (clientPtr,WM_MDIREFRESHMENU,(WPARAM)0,(LPARAM)0);
				
				DrawMenuBar (framePtr);
				MakeReturn0Cci (pcci);
			}
			break;
		/*	Track pop up menu. */
		case CcRqTRACKPOPMENU:	/* popupmenu,framePtr; BOOL result. */
			{
				HMENU popupmenu;
				HWND  framePtr;
				UINT  flags;
				POINT mousePos;
				BOOL  ok;

				popupmenu = (HMENU) pcci->p1;
				framePtr  = (HWND)  pcci->p2;
				flags     = TPM_CENTERALIGN | TPM_RIGHTBUTTON;
				GetCursorPos (&mousePos);

				ok = TrackPopupMenu (popupmenu,flags,mousePos.x,mousePos.y,0,framePtr,NULL);

				MakeReturn1Cci (pcci,(int)ok);
			}
			break;
		case CcRqCREATEPOPMENU: /* no params; MENU result.   */
			{
				MakeReturn1Cci (pcci, (int) CreatePopupMenu ());
			}
			break;
		case CcRqCHECKMENUITEM: /* menu, HITEM, on/off; no result.	*/
			{
				UINT check;
				if (pcci->p3)
				{
					check = MF_CHECKED;
				}
				else
				{
					check = MF_UNCHECKED;
				}
				CheckMenuItem ((HMENU) pcci->p1, (UINT) pcci->p2, MF_BYCOMMAND | check);
				MakeReturn0Cci (pcci);
			}
			break;
		case CcRqDESTROYMODALDIALOG: /* hwnd; no result. */
			{
				EndDialog ((HWND)pcci->p1,0);
				ghwndLastModalDialog = NULL;	/*	Create only one zero timer for modal dialog stack. */
				MakeReturn0Cci (pcci);
			}
			break;
		case CcRqDESTROYWINDOW: /* hwnd; no result. */
			{
				BOOL noError;

				noError = DestroyWindow ((HWND) pcci->p1);
				if (!noError)
				{
					rMessageBox (NULL,MB_APPLMODAL,"CcRqDESTROYWINDOW","DestroyWindow failed");
				}
				MakeReturn0Cci (pcci);
			}
			break;
		case CcRqDESTROYMDIDOCWINDOW:	/* hwndFrame, hwndClient, wPtr; no result. */
			{
				HWND hwndFrame, hwndClient, hwndDocWindow, newActiveDocWindow;

				hwndFrame     = (HWND) pcci->p1;
				hwndClient    = (HWND) pcci->p2;
				hwndDocWindow = (HWND) pcci->p3;

				// First destroy the given MDI document window. 
				SendMessage (hwndClient, WM_MDIDESTROY, (WPARAM) hwndDocWindow, 0);

				// Obtain the new active MDI document window.
				newActiveDocWindow = (HWND) SendMessage (hwndClient, WM_MDIGETACTIVE, (WPARAM) 0, (LPARAM) NULL);
				if (newActiveDocWindow==NULL)			// If all document windows have been closed
				{
					ghActiveFrameWindow  = hwndFrame;	// then keep track of the MDI frame window
					ghActiveClientWindow = hwndClient;	// and client window
				}

				MakeReturn0Cci (pcci);
			}
			break;
		/*	Create a SDI frame window. */
		case CcRqCREATESDIFRAMEWINDOW:	/* accept file open; frame ptr, menubar results. */
			{
				HWND    hwndFrame;
				BOOL    acceptFileOpen;
				DWORD   styleFlags;
				HMENU   menuBar;

				acceptFileOpen = (BOOL) pcci->p1;	/*	respond to file open events. */

				/* The frame style flags equal the styleFlags with the scrollbar flags masked out. */
				styleFlags   = WS_SYSMENU | WS_OVERLAPPED | WS_MINIMIZEBOX | WS_MAXIMIZEBOX | WS_THICKFRAME;

				/* Create the menubar. */
				menuBar = CreateMenu ();

				/* Create the window. */
				hwndFrame = CreateWindow (	SDIFrameClassName,				/* Class name						*/
											NULL,							/* No title yet						*/
											styleFlags,						/* SDI frame style flags			*/
											CW_USEDEFAULT,CW_USEDEFAULT,	/* No position yet					*/
											CW_USEDEFAULT,CW_USEDEFAULT, 	/* no size yet						*/
											NULL,							/* Parent window					*/
											menuBar,						/* menu handle						*/
											(HANDLE) ghInst,				/* Instance that owns the window	*/
											0);
				ShowWindow (hwndFrame, SW_SHOWNORMAL);
				UpdateWindow (hwndFrame);

				if (acceptFileOpen)
					DragAcceptFiles (hwndFrame,TRUE);		/* register for WM_DROPFILES events. */

				MakeReturn2Cci (pcci, (int) hwndFrame, (int) menuBar);
			}
			break;
		/*	Create a SDI document window. */
		case CcRqCREATESDIDOCWINDOW:	/* textptr, frameptr, packed pos, w,h, flags; client ptr result. */
			{
				HWND    hwndFrame, hwndClient, hwndToolbar;
				POINT   clientDims, frameDims, winpos;
				LPCTSTR pwintitle;
				DWORD   styleFlags;
				RECT    tbRect;
				int     tbHeight;

				pwintitle    = (LPCTSTR) pcci->p1;
				hwndFrame    = (HWND) pcci->p2;
				winpos.x     = pcci->p3>>16;
				winpos.y     = (pcci->p3<<16)>>16;
				clientDims.x = pcci->p4;
				clientDims.y = pcci->p5;
				styleFlags   = (DWORD) pcci->p6;

				hwndToolbar  = (HWND)GetGWL_USERDATA (hwndFrame);
				if (hwndToolbar == NULL || !GetWindowRect (hwndToolbar,&tbRect))
				{
					tbHeight = 0;
				}
				else
				{
					tbHeight = tbRect.bottom - tbRect.top;
				}

				frameDims.x = clientDims.x;
				frameDims.y = clientDims.y + tbHeight;
				W95AdjustCleanSDIWindowDimensions (styleFlags, &frameDims);

				/* Adjust the pos and size of the frame window. */
				SetWindowPos (hwndFrame,NULL,winpos.x,winpos.y,frameDims.x,frameDims.y,SWP_NOZORDER);
				UpdateWindow (hwndFrame);
				SetWindowText (hwndFrame,pwintitle);

				/* The client style flags are WS_CHILD and styleFlags. */
				styleFlags |= WS_CHILD;

				/* Create the new client window of the frame window. */
				hwndClient = CreateWindow  (SDIWindowClassName,			/* Class name						*/
											pwintitle,					/* title							*/
											styleFlags,					/* SDI client style flags			*/
											0, tbHeight,				/* x, y								*/
											clientDims.x,clientDims.y,	/* width, height					*/
											hwndFrame,					/* Parent window					*/
											(HMENU) NULL,				/* no menu handle					*/
											(HANDLE) ghInst,			/* Instance that owns the window	*/
											0);
				SetWindowPos (hwndClient,HWND_TOP,0,0,0,0,SWP_NOMOVE | SWP_NOSIZE);
				ShowWindow (hwndClient,SW_SHOWNORMAL);
				UpdateWindow (hwndClient);
				UpdateWindowScrollbars (hwndClient);

				MakeReturn1Cci (pcci, (int) hwndClient);
			}
			break;
		/*	Create MDI frame window. */
		case CcRqCREATEMDIFRAMEWINDOW:	/* show, accept file open; frame ptr, client ptr, menubar, windowmenu results. */
			{
				BOOL show, acceptFileOpen;
				DWORD styleFlags;
				HWND hwndFrame, hwndClient;
				HMENU menuBar, windowMenu;

				show           = (BOOL) pcci->p1;
				acceptFileOpen = (BOOL) pcci->p2;	/*	respond to file open events. */

				styleFlags = WS_OVERLAPPEDWINDOW | WS_CLIPCHILDREN;
				if (show)
					styleFlags |= WS_MAXIMIZE;
				else
					styleFlags |= WS_MINIMIZE;

				hwndFrame = CreateWindow (	MDIFrameClassName			// Class name
										 ,	(LPCTSTR) gAppName			// Title is the application name
										 ,	styleFlags					// Style parameters
										 ,	0,0							// Default position (x,y)
										 ,	CW_USEDEFAULT,CW_USEDEFAULT	// Default size (w,h)
										 ,	NULL						// Every window should be top-level
										 ,	NULL
										 ,	(HANDLE) ghInst				// Instance that owns the window
										 ,	NULL
										 );
				hwndClient = GetWindow (hwndFrame,GW_CHILD);	// retrieve the "MDICLIENT" window
				menuBar    = GetMenu (hwndFrame);				// retrieve the menu bar of the frame window
				windowMenu = GetSubMenu (menuBar,0);			// retrieve the "Window" menu of the menu bar
				if (show)
				{
					ShowWindow (hwndFrame,SW_MAXIMIZE);			// show the frame window (SW_MAXIMIZE gives best result)
					UpdateWindow (hwndFrame);					// update the frame window
				}
				DrawMenuBar (hwndFrame);						// update the menu bar

				if (acceptFileOpen)
					DragAcceptFiles (hwndFrame,TRUE);			/* register for WM_DROPFILES events. */

				/*	Store the standard Windows callback routine adress in stdMDIClientCallback
					and subclass the MDI client window with MDIClientProcedure.
				*/
				stdMDIClientCallback = SetWindowLong (hwndClient, GWL_WNDPROC, (LONG) MDIClientProcedure);
				
				MakeReturn4Cci (pcci,(int) hwndFrame,(int) hwndClient,(int) menuBar,(int) windowMenu);
			} break;
		/*	Create MDI child window. */
		case CcRqCREATEMDIDOCWINDOW:		/* textptr, clientPtr, behindPtr, packed pos, packed size, flags; HWND result. */
			{
				HWND    whandle,hwndClient,hwndBehind;
				POINT   dims, winpos;
				LPCTSTR pwintitle;
				DWORD   styleFlags, exStyleFlags;
				MDICREATESTRUCT mdicreate;		// The structure sent to the client window

				pwintitle    = (LPCTSTR) pcci->p1;
				hwndClient   = (HWND) pcci->p2;
				hwndBehind   = (HWND) pcci->p3;
				winpos.x     = pcci->p4>>16;
				winpos.y     = (pcci->p4<<16)>>16;
				dims.x       = pcci->p5>>16;
				dims.y       = (pcci->p5<<16)>>16;
				styleFlags   = (DWORD) pcci->p6;
				exStyleFlags = (DWORD) 0;

				W95AdjustCleanMDIWindowDimensions (styleFlags, &dims);

				if (styleFlags&WS_THICKFRAME != 0)				/* If resizable*/
				{
					exStyleFlags |= WS_EX_CLIENTEDGE;			/* then provide a 3D look to the window. */
				}

				/* fill the MDICREATESTRUCT record */
				mdicreate.szClass = MDIWindowClassName;
				mdicreate.szTitle = pwintitle;
				mdicreate.hOwner  = (HANDLE) ghInst;
				mdicreate.x       = winpos.x;
				mdicreate.y       = winpos.y;
				mdicreate.cx      = dims.x;
				mdicreate.cy      = dims.y;
				mdicreate.style   = styleFlags;
				mdicreate.lParam  = 0;

				/* create the window */
				whandle = (HWND) SendMessage (hwndClient,WM_MDICREATE,0,(LPARAM)(LPMDICREATESTRUCT) &mdicreate);

				/* take care of window stacking */
				if (hwndBehind!=0)
				{
					SetWindowPos (whandle, hwndBehind, 0, 0, 0, 0, SWP_NOMOVE + SWP_NOSIZE);
				}

				MakeReturn1Cci (pcci, (int) whandle);
			}
			break;
		case CcRqSETWINDOWTITLE:		/* hwnd, textptr		no result. */
			{
				SetWindowText ((HWND) pcci->p1, (LPCTSTR) pcci->p2);
				MakeReturn0Cci (pcci);
			}
			break;
		case CcRqGETWINDOWTEXT: /* hwnd;  textptr result. */
			{
				char *textptr;
				HWND hwnd;
				int length;

				hwnd = (HWND) pcci->p1;
				length = GetWindowTextLength (hwnd);

				textptr = (char *) rmalloc (length + 1);
				GetWindowText (hwnd, textptr, length + 1);
				textptr[length] = 0;
				MakeReturn1Cci (pcci, (int) textptr);
			}
			break;
		case CcRqGETFONTNAMES:	/* no params; no result. */
			{
				HDC hdc;

				hdc = GetDC (ghMainWindow);
				EnumFontFamilies (hdc, NULL, (FONTENUMPROC) EnumFontNameProc, 0);
				ReleaseDC (ghMainWindow, hdc);
				MakeReturn0Cci (pcci);
			}
			break;
		case CcRqGETFONTSIZES:	/* textptr; no result. */
			{
				HDC hdc;

				hdc = GetDC (ghMainWindow);
				EnumFontFamilies (hdc, (char *) pcci->p1, (FONTENUMPROC) EnumFontSizeProc, 0);
				ReleaseDC (ghMainWindow, hdc);
				rfree ((char *) pcci->p1);
				MakeReturn0Cci (pcci);
			}
			break;
		/*	Update rect part of a window. */
		case CcRqUPDATEWINDOWRECT: /* hwnd, left,top,right,bottom; no result. */
			{
				RECT rect;
				HWND hwnd;

				hwnd       = (HWND) pcci->p1;
				rect.left  = pcci->p2;
				rect.top   = pcci->p3;
				rect.right = pcci->p4;
				rect.bottom= pcci->p5;

				InvalidateRect (hwnd,&rect,FALSE);
				UpdateWindow (hwnd);
				RedrawWindow (hwnd,&rect,NULL,RDW_FRAME | RDW_VALIDATE | RDW_UPDATENOW | RDW_NOCHILDREN);

				MakeReturn0Cci (pcci);
			}
			break;
		case CcRqGETCLIENTSIZE: /* hwnd;		width, height result.  */
			{
				RECT rect;

				GetClientRect ((HWND) pcci->p1, &rect);

				MakeReturn2Cci (pcci, rect.right - rect.left, rect.bottom - rect.top);
			}
			break;
		/*	Set the ClientRect. */
		case CcRqSETCLIENTSIZE: /* hwnd, width, height; no result. */
			{
				HWND hwnd;
				int w,h,curw,curh,clientw,clienth;
				UINT flags;
				RECT clientRect,windowRect;

				hwnd  = (HWND) pcci->p1;
				w     = pcci->p2;
				h     = pcci->p3;
				flags = SWP_NOMOVE			/* retain position */
					  | SWP_NOZORDER;		/* retain Z order */

				GetClientRect (hwnd, &clientRect);
				GetWindowRect (hwnd, &windowRect);
				clientw = clientRect.right - clientRect.left;
				clienth = clientRect.bottom- clientRect.top;
				curw    = windowRect.right - windowRect.left;
				curh    = windowRect.bottom- windowRect.top;

				SetWindowPos (hwnd, HWND_TOP, 0,0, curw+w-clientw,curh+h-clienth, flags);
				MakeReturn0Cci (pcci);
			}
			break;
		/*	(En/Dis)able windows/dialogues. */
		case CcRqSETSELECTWINDOW:	/* hwnd, hasHScroll, hasVScroll, toAble, modalContext; no result. */
			{
				HWND window;
				BOOL hasHScroll, hasVScroll, toAble, modalContext;

				window       = (HWND) pcci->p1;
				hasHScroll   = (BOOL) pcci->p2;
				hasVScroll   = (BOOL) pcci->p3;
				toAble       = (BOOL) pcci->p4;
				modalContext = (BOOL) pcci->p5;

				if (modalContext)					/*	if not a modal context, then do not disable window	*/
					EnableWindow (window,toAble);	/*  because it can't be moved, or closed. */
				if (hasHScroll)
					EnableScrollBar (window,SB_HORZ,toAble ? ESB_ENABLE_BOTH : ESB_DISABLE_BOTH);
				if (hasVScroll)
					EnableScrollBar (window,SB_VERT,toAble ? ESB_ENABLE_BOTH : ESB_DISABLE_BOTH);

				MakeReturn0Cci (pcci);
			}
			break;
		case CcRqGETWINDOWPOS:	/* hwnd;   width, heigth result */
			{
				RECT rect;

				GetWindowRect ((HWND) pcci->p1, &rect);

				MakeReturn2Cci (pcci, rect.left, rect.top);
			}
			break;
		/*	Set the position of windows/controls. */
		case CcRqSETWINDOWPOS:	/* hwnd, x,y, update, include scrollbars ; no result. */
			{
				HWND hwnd;
				int x,y;
				BOOL update,inclScrollbars;
				UINT flags;

				hwnd           = (HWND)pcci->p1;
				x              = pcci->p2;
				y              = pcci->p3;
				update         = pcci->p4;
				inclScrollbars = pcci->p5;
				flags          = SWP_NOSIZE			/* retain size */
					           | SWP_NOZORDER;		/* retain Z order */

				SetWindowPos (hwnd, HWND_TOP, x,y, 0,0, flags);
				if (IsWindowVisible (hwnd) && update!=0)
				{	/* only if window is visible and update is requested, proceed to enforce update. */
					if (inclScrollbars)
						UpdateWindowScrollbars (hwnd);
					else
					{
						InvalidateRect (hwnd,NULL,TRUE);
						UpdateWindow (hwnd);
						RedrawWindow (hwnd,NULL,NULL,RDW_FRAME | RDW_VALIDATE | RDW_UPDATENOW | RDW_NOCHILDREN);
					}
				}

				MakeReturn0Cci (pcci);
			}
			break;
		/*	Get the size of the bounding rectangle of windows/controls. */
		case CcRqGETWINDOWSIZE: /* hwnd; width,height result. */
			{
				RECT rect;

				GetWindowRect ((HWND) pcci->p1, &rect);

				MakeReturn2Cci (pcci, rect.right-rect.left, rect.bottom-rect.top);
			}
			break;
		/*	Set the size of windows/controls. */
		case CcRqSETWINDOWSIZE: /* hwnd, w,h, update; no result. */
			{
				HWND hwnd;
				int w,h;
				BOOL update;
				UINT flags;

				hwnd   = (HWND)pcci->p1;
				w      = pcci->p2;
				h      = pcci->p3;
				update = pcci->p4;
				flags  = SWP_NOMOVE			/* retain position */
					   | SWP_NOZORDER;		/* retain Z order  */
				/* flags do not contain SWP_NOREDRAW because otherwise not all windows get updated properly. */

				SetWindowPos (hwnd, HWND_TOP, 0,0, w,h, flags);
				if (update!=0)				/* still, updates are not sufficient using SetWindowPos only. */
					UpdateWindowScrollbars (hwnd);

				MakeReturn0Cci (pcci);
			}
			break;
		/*	Activate control. */
		case CcRqACTIVATECONTROL:	/* controlPtr; no result. */
			{
				SetFocus ((HWND) pcci->p1);

				MakeReturn0Cci (pcci);
			}
			break;
		/*	Activate window. */
		case CcRqACTIVATEWINDOW:	/* isMDI, clientPtr, thisWindow; no result. */
			{
				BOOL isMDI;
				HWND clientPtr, thisWindow;

				isMDI       = (BOOL) pcci->p1;
				clientPtr   = (HWND) pcci->p2;
				thisWindow  = (HWND) pcci->p3;

				if (isMDI)
				{
					SendMessage (clientPtr,WM_MDIACTIVATE,(WPARAM)thisWindow,(LPARAM)0);
				}
				else
				{
					SetActiveWindow (thisWindow);
				}
				MakeReturn0Cci (pcci);
			}
			break;
		case CcRqCHANGEWINDOWCURSOR:	/* hwnd, cursor code; no result. It is assumed that the hwnd argument	*/
										/* corresponds to either a SDI/MDI window (and not frame).				*/
			{
				HWND hwnd;
				POINT p;
				int cursorcode;
				LocalWindowData wdata;

				hwnd = (HWND) pcci->p1;
				cursorcode = pcci->p2;

				GetCursorPos (&p);

				if (hwnd == WindowFromPoint (p) &&
					SendMessage (hwnd, WM_NCHITTEST, 0, MAKELPARAM (p.x, p.y)) == HTCLIENT)
				{
					SetCursorFromCode (cursorcode);
				}
				
				wdata = (LocalWindowData) GetWindowLong (hwnd,0);
				wdata->lwd_cursorcode = cursorcode;
				SetWindowLong (hwnd, 0, (long)wdata);

				MakeReturn0Cci (pcci);
			}
			break;
		case CcRqOBSCURECURSOR: /* no params; no result. */
			{
				SetCursor (GetHiddenCursor ());
				MakeReturn0Cci (pcci);
			}
			break;
		case CcRqSETGLOBALCURSOR:		/* cursorcode; no result. */
			{
				HWND hwnd;

				gGlobalCursorCode = pcci->p1;

				hwnd = ApplicationWindowUnderCursor ();

				if (hwnd != NULL)
					SetCursorFromCode (gGlobalCursorCode);

				MakeReturn0Cci (pcci);

			}
			break;
		case CcRqRESETCURSOR:	/* no params, no result. */
			{
				HWND hwnd;

				gGlobalCursorCode = -1;

				hwnd = ApplicationWindowUnderCursor ();

				if (hwnd != NULL)
				{
					int hittestvalue;
					POINT p;

					GetCursorPos (&p);
					hittestvalue = SendMessage (hwnd, WM_NCHITTEST, 0, MAKELPARAM (p.x, p.y));
					SendMessage (hwnd, WM_SETCURSOR, (WPARAM) hwnd, MAKELPARAM (hittestvalue, WM_MOUSEMOVE));
				}

				MakeReturn0Cci (pcci);
			}
			break;
		/*	Set range of scrollbars. */
		case CcRqSETSCROLLRANGE:	/* hwnd, iBar, min, max, redraw, no result */
			{
				HWND hwnd;
				int min, max, iBar;
				BOOL redraw;

				hwnd = (HWND) pcci->p1;
				iBar = pcci->p2;
				min  = pcci->p3;
				max  = pcci->p4;
				redraw = (BOOL) pcci->p5;

				SetScrollRange (hwnd, iBar, min, max, redraw);

				MakeReturn0Cci (pcci);
			}
			break;
		/*	Set pos of scrollbars. */
		case CcRqSETSCROLLPOS:	/* hwnd, iBar, thumb, maxx, maxy, extent, no result */
			{
				HWND hwnd;
				int thumb, iBar, maxx, maxy, extent;
				SCROLLINFO sif;
				RECT scrollBarRect;
				BOOL noredraw;

				hwnd   = (HWND) pcci->p1;
				iBar   = pcci->p2;
				thumb  = pcci->p3;
				maxx   = pcci->p4;		// maxx   is the right-most  x coordinate of the enclosing rectangle of the scrollbar
				maxy   = pcci->p5;		// maxy   is the bottom-most y coordinate of the enclosing rectangle of the scrollbar
				extent = pcci->p6;		// extent is the width (height) of the vertical (horizontal) scrollbar
				
				sif.cbSize = sizeof (SCROLLINFO);
				sif.fMask  = SIF_POS;
				sif.nPos   = thumb;

				SetScrollInfo (hwnd, iBar, &sif, TRUE);	// Setting TRUE here to force redraw is not sufficient.
				noredraw = maxx==0 && maxy==0 && extent==0;
				if (!noredraw)	// if the scrollbar does not need to be updated then all values must be zero
				{
					if (iBar==SB_HORZ)
					{
						scrollBarRect.left   = 0;
						scrollBarRect.top    = maxy-extent;
						scrollBarRect.right  = maxx;
						scrollBarRect.bottom = maxy;
					}
					else
					{
						scrollBarRect.left   = maxx-extent;
						scrollBarRect.top    = 0;
						scrollBarRect.right  = maxx;
						scrollBarRect.bottom = maxy;
					}
					InvalidateRect (hwnd, &scrollBarRect, FALSE);
				//	UpdateWindow (hwnd);
				// PA: use RedrawWindow to immediately redraw the indicated scrollbar instead of update mechanism.
					if (!RedrawWindow (hwnd,&scrollBarRect,NULL,RDW_FRAME | RDW_INVALIDATE | RDW_UPDATENOW | RDW_NOCHILDREN))
						rMessageBox (NULL,MB_APPLMODAL,"CcRqSETSCROLLPOS","RedrawWindow failed");
					ValidateRect (hwnd, &scrollBarRect);
				}

				MakeReturn0Cci (pcci);
			}
			break;
		/*	Set thumb size of scrollbars. */
		case CcRqSETSCROLLSIZE:	/* hwnd, iBar, size, maxx, maxy, extent, no result */
			{
				HWND hwnd;
				int size, iBar, maxx, maxy, extent;
				SCROLLINFO sif;
				RECT scrollBarRect;
				BOOL noredraw;

				hwnd   = (HWND) pcci->p1;
				iBar   = pcci->p2;
				size   = pcci->p3;
				maxx   = pcci->p4;		// maxx   is the right-most  x coordinate of the enclosing rectangle of the scrollbar
				maxy   = pcci->p5;		// maxy   is the bottom-most y coordinate of the enclosing rectangle of the scrollbar
				extent = pcci->p6;		// extent is the width (height) of the vertical (horizontal) scrollbar
				
				sif.cbSize = sizeof (SCROLLINFO);
				sif.fMask = SIF_PAGE;
				sif.nPage = size;

				SetScrollInfo (hwnd, iBar, &sif, TRUE);	// Setting TRUE here to force redraw is not sufficient.
				noredraw = maxx==0 && maxy==0 && extent==0;
				if (!noredraw)	// if the scrollbar does not need to be updated then all values must be zero
				{
					if (iBar==SB_HORZ)
					{
						scrollBarRect.left   = 0;
						scrollBarRect.top    = maxy-extent;
						scrollBarRect.right  = maxx;
						scrollBarRect.bottom = maxy;
					}
					else
					{
						scrollBarRect.left   = maxx-extent;
						scrollBarRect.top    = 0;
						scrollBarRect.right  = maxx;
						scrollBarRect.bottom = maxy;
					}
					InvalidateRect (hwnd, &scrollBarRect, FALSE);
				//	UpdateWindow (hwnd);
				// PA: use RedrawWindow to immediately redraw the indicated scrollbar instead of update mechanism.
					if (!RedrawWindow (hwnd,&scrollBarRect,NULL,RDW_FRAME | RDW_INVALIDATE | RDW_UPDATENOW | RDW_NOCHILDREN))
						rMessageBox (NULL,MB_APPLMODAL,"CcRqSETSCROLLSIZE","RedrawWindow failed");
					ValidateRect (hwnd, &scrollBarRect);
				}

				MakeReturn0Cci (pcci);
			}
			break;
		/*	Set selection of edit controls. */
		case CcRqSETEDITSELECTION:	/* hwnd, first, last, no result. */
			{
				HWND hwnd;
				int first,last;

				hwnd  = (HWND) pcci->p1;
				first = pcci->p2;
				last  = pcci->p3;

				SendMessage (hwnd, EM_SETSEL, (WPARAM) first, (LPARAM) last);		/* Set the selection of the edit control. */
				SendMessage (hwnd, EM_SCROLLCARET, 0,0);		/* Let the caret be displayed - (w/l)Param MUST be 0. */

				MakeReturn0Cci (pcci);
			}
			break;
		/*	Create a toolbar in a window. */
		case CcRqCREATEMDITOOLBAR:			/* hwnd, width, height; toolbarptr, full toolbar height result; */
			{
				HWND hwndToolbar;
				HWND hwndParent;
				int  bmpWidth, bmpHeight, tbHeight;
				RECT tbRect;

				hwndParent  = (HWND) pcci->p1;	// The parent is the frame window
				bmpWidth    = pcci->p2;
				bmpHeight   = pcci->p3;

				hwndToolbar = CreateWindow (TOOLBARCLASSNAME,
											NULL,
											WS_CHILD | WS_VISIBLE | WS_CLIPSIBLINGS | CCS_TOP | TBSTYLE_TOOLTIPS,
											0,0,0,0,
											hwndParent,
											(HMENU) NULL,
											(HANDLE) ghInst,
											0
											);
				SetGWL_USERDATA ((LONG)hwndToolbar, hwndParent);	// Administrate the toolbar handle in the MDI frame parent handle
				SendMessage (hwndToolbar, TB_SETBITMAPSIZE, (WPARAM)0, (LPARAM)MAKELONG(bmpWidth,bmpHeight));
				SendMessage (hwndToolbar, TB_AUTOSIZE, (WPARAM)0, (LPARAM)0);
				SendMessage (hwndToolbar, TB_BUTTONSTRUCTSIZE, (WPARAM) sizeof (TBBUTTON), (LPARAM) 0);

				/*	MDI windows that will get a toolbar are initially created with the WS_MINIMIZE flag
					(see CcRqCREATEMDIFRAMEWINDOW).
					This is needed to ensure that the toolbar, after creation, becomes visible. 
				*/
				ShowWindow (hwndParent,SW_MAXIMIZE);

				if (!GetWindowRect (hwndToolbar,&tbRect))
					rMessageBox (NULL,MB_APPLMODAL,"CcRqCREATEMDITOOLBAR","GetWindowRect failed");
				tbHeight = tbRect.bottom - tbRect.top;

				MakeReturn2Cci (pcci,(int)hwndToolbar,tbHeight);
			}
			break;
		/*	Create a toolbar in a SDI window. */
		case CcRqCREATESDITOOLBAR:			/* hwnd, width, height; toolbarptr, full toolbar height result; */
			{
				HWND hwndParent,hwndClient,hwndToolbar;
				int  bmpWidth, bmpHeight, tbHeight;
				RECT tbRect;

				hwndParent  = (HWND) pcci->p1;
				bmpWidth    = pcci->p2;
				bmpHeight   = pcci->p3;
				hwndClient  = GetSDIClientWindow (hwndParent);
				hwndToolbar = CreateWindow (TOOLBARCLASSNAME,
											NULL,
											WS_CHILD | WS_VISIBLE | WS_CLIPSIBLINGS | CCS_TOP | TBSTYLE_TOOLTIPS,
											0,0,0,0,
											hwndParent,
											(HMENU) NULL,
											(HANDLE) ghInst,
											0
										   );
				SetGWL_USERDATA ((LONG)hwndToolbar,hwndParent);	// Administrate the toolbar handle in the SDI window handle
				SendMessage (hwndToolbar, TB_SETBITMAPSIZE, (WPARAM)0, (LPARAM)MAKELONG(bmpWidth,bmpHeight));
				SendMessage (hwndToolbar, TB_AUTOSIZE, (WPARAM)0, (LPARAM)0);
				SendMessage (hwndToolbar, TB_BUTTONSTRUCTSIZE, (WPARAM) sizeof (TBBUTTON), (LPARAM) 0);

				if (!GetWindowRect (hwndToolbar,&tbRect))
					rMessageBox (NULL,MB_APPLMODAL,"CcRqCREATESDITOOLBAR","GetWindowRect failed");
				tbHeight = tbRect.bottom - tbRect.top;

				/*	Before showing the new toolbar, move the client window down and update its scrollbars. */
				if (hwndClient != NULL)
				{
					SetWindowPos (hwndClient,HWND_TOP,0,tbHeight,0,0,SWP_NOSIZE | SWP_NOZORDER);
					UpdateWindowScrollbars (hwndClient);
				}
				
				ShowWindow (hwndToolbar,SW_SHOWNORMAL);
				UpdateWindow (hwndToolbar);

				MakeReturn2Cci (pcci, (int) hwndToolbar, tbHeight);
			}
			break;
		/*	Create a bitmap toolbar item. */
		case CcRqCREATETOOLBARITEM:		// hwnd, hbmp, index; no results; 
			{
				HWND hwndToolbar;
				HBITMAP hbmp;
				int index;
				TBADDBITMAP tbab;
				TBBUTTON    tbb;
				
				hwndToolbar= (HWND)    pcci->p1;
				hbmp       = (HBITMAP) pcci->p2;
				index      = pcci->p3;

				tbab.hInst = NULL;
				tbab.nID   = (UINT) hbmp;

				tbb.iBitmap   = index-1;
				tbb.idCommand = index;
				tbb.fsState   = (BYTE)TBSTATE_ENABLED;
				tbb.fsStyle   = (BYTE)TBSTYLE_BUTTON;
				tbb.dwData    = (DWORD)0;
				tbb.iString   = 0;

				SendMessage (hwndToolbar, TB_BUTTONSTRUCTSIZE, (WPARAM) sizeof (TBBUTTON), (LPARAM) 0);
				SendMessage (hwndToolbar, TB_ADDBITMAP, (WPARAM)1, (LPARAM)(LPTBADDBITMAP)&tbab);
				SendMessage (hwndToolbar, TB_ADDBUTTONS,(WPARAM)(UINT)1, (LPARAM)(LPTBBUTTON)&tbb);

				MakeReturn0Cci (pcci);
			}
			break;
		/*	Create a separator toolbar item. */
		case CcRqCREATETOOLBARSEPARATOR:	// hwnd; no results;
			{
				HWND hwndToolbar;
				TBBUTTON tbb;

				hwndToolbar = (HWND) pcci->p1;
			    
				tbb.iBitmap   = 0;
				tbb.idCommand = 0;
				tbb.fsState   = (BYTE)TBSTATE_ENABLED;
				tbb.fsStyle   = (BYTE)TBSTYLE_SEP;
				tbb.dwData    = (DWORD)0;
				tbb.iString   = 0;
				
				SendMessage (hwndToolbar, TB_BUTTONSTRUCTSIZE, (WPARAM) sizeof (TBBUTTON), (LPARAM) 0);
				SendMessage (hwndToolbar, TB_ADDBUTTONS,(WPARAM)(UINT)1, (LPARAM)(LPTBBUTTON)&tbb);

				MakeReturn0Cci (pcci);
			}
			break;
		/*	CcRqUPDATEDESKTOP forces an update of the desktop and its applications.
		*/
		case CcRqUPDATEDESKTOP:			/* no params; no result; */
			{
				OSVERSIONINFO osVersionInfo;
				DWORD osPlatformID;

				osVersionInfo.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);	/* Set size BEFORE calling GetVersionEx. */

				if (GetVersionEx (&osVersionInfo))
				{
					osPlatformID = osVersionInfo.dwPlatformId;

					if (osPlatformID==VER_PLATFORM_WIN32_WINDOWS)	/* We're in Windows95. */
					{
						char szParam[13] = "WindowMetrics";

						SendMessage (HWND_BROADCAST, WM_SETTINGCHANGE, (WPARAM)SPI_SETICONMETRICS, (LPARAM)(LPCTSTR)szParam);
					}
					if (osPlatformID==VER_PLATFORM_WIN32_NT)		/* We're in WindowsNT. */
					{
						SendMessage (HWND_BROADCAST, WM_WININICHANGE, (WPARAM)SPI_SETICONMETRICS, (LPARAM)(LPCTSTR)NULL);
					}
				}
				/* if either GetVersionEx fails, or platform is different, skip refresh action altogether.
				*/
				MakeReturn0Cci (pcci);
			}
			break;
		/*	Create the specialised directory selector dialog from Windows.
			By courtesy of Martijn Vervoort.
		*/
		case CcRqDIRECTORYDIALOG:		/* no params;  bool, textptr result; */
			{
				char buffer[MAX_PATH];
				LPITEMIDLIST pidlReturn;
				BROWSEINFO bi;
				char *s;
				char title[17] = "Select Directory\0";

				bi.hwndOwner      = GetActiveWindow ();
				bi.pidlRoot       = NULL;
				bi.pszDisplayName = buffer;
				bi.lpszTitle      = title;
				bi.ulFlags        = BIF_RETURNONLYFSDIRS;
				bi.lpfn           = NULL;
				bi.lParam         = 0;

				CoInitialize (NULL);		// Initialise the COM library; must be balanced by CoUninitialize()

				pidlReturn = SHBrowseForFolder (&bi);
				if (pidlReturn)
				{
					s = (char *) rmalloc (MAX_PATH+1);
					SHGetPathFromIDList (pidlReturn,s);
					CoTaskMemFree (pidlReturn);
					CoUninitialize ();		// Uninitialise the COM library

					MakeReturn2Cci (pcci, (int)TRUE, (int)s);
					/* and have the calling Clean function deallocate the directory name buffer. */
				}
				else
				{
					CoUninitialize ();		// Uninitialise the COM library
					MakeReturn2Cci (pcci, (int)FALSE, (int)NULL);
				}
			}
			break;
		case CcRqFILEOPENDIALOG:		/* no params;  bool, textptr result; */
			{
				OPENFILENAME ofn;
				BOOL success;

				ofn.lStructSize       = sizeof (OPENFILENAME);

				ofn.hwndOwner         = GetActiveWindow ();
				ofn.hInstance         = NULL;
				ofn.lpstrFilter       = NULL;
				ofn.lpstrCustomFilter = NULL;
				ofn.nMaxCustFilter    = 0;
				ofn.nFilterIndex      = 0;
				ofn.lpstrFile         = (LPSTR) rmalloc (MAX_PATH);
				ofn.lpstrFile[0]      = '\0';
				ofn.nMaxFile          = MAX_PATH;
				ofn.lpstrFileTitle    = NULL;
				ofn.nMaxFileTitle     = 0;
				ofn.lpstrInitialDir   = NULL;
				ofn.lpstrTitle        = NULL;
				ofn.Flags             = OFN_EXPLORER
								      | OFN_FILEMUSTEXIST
									  |	OFN_HIDEREADONLY
									  | OFN_PATHMUSTEXIST
									  | OFN_ENABLEHOOK;		// PA: OFN_ENABLEHOOK added from Ronny
				ofn.lpstrDefExt       = NULL;
				ofn.lCustData         = 0;
				ofn.lpfnHook          = &FileSelectorHook;	// PA: &FileSelectorHook instead of NULL from Ronny
				ofn.lpTemplateName    = NULL;

				gProhibitWindowActivation++;
				success = GetOpenFileName (&ofn);
				gProhibitWindowActivation--;

				if (success)
				{
					MakeReturn2Cci (pcci, success, (int) ofn.lpstrFile);
					/* and have the calling clean function deallocate the filename buffer */
				}
				else
				{
					MakeReturn2Cci (pcci, success, (int) NULL);
					rfree (ofn.lpstrFile);
				}
			}
			break;
		case CcRqFILESAVEDIALOG:		/* promptptr, nameptr; bool, textptr result; */
			{
				OPENFILENAME ofn;
				BOOL success;
				char *promptptr;
				char *nameptr;

				promptptr = (char *) pcci->p1;
				nameptr = (char *) pcci->p2;

				if (rstrlen (promptptr) == 0)
					promptptr = NULL;	/* the calling clean function will
										   deallocate the memory allocated
										   for this empty string */

				ofn.lStructSize = sizeof (OPENFILENAME);

			//	if (IsWindowEnabled (ghMainWindow))
			//	{
			//		ofn.hwndOwner = ghMainWindow;
			//	}
			//	else
			//	{
				ofn.hwndOwner = GetActiveWindow ();
			//	}

				ofn.lpstrFilter       = NULL;
				ofn.lpstrCustomFilter = NULL;
				ofn.nMaxCustFilter    = 0;
				ofn.nFilterIndex      = 0;
				ofn.lpstrFile         = (LPSTR) rmalloc (MAX_PATH);
				if (rstrlen (nameptr) < MAX_PATH)
				{
					rscopy (ofn.lpstrFile, nameptr);
				}
				else
				{
					rsncopy (ofn.lpstrFile, nameptr, MAX_PATH - 1);
					ofn.lpstrFile[MAX_PATH - 1] = '\0';
				}
				ofn.nMaxFile        = MAX_PATH;
				ofn.lpstrFileTitle  = NULL;
				ofn.nMaxFileTitle   = 0;
				ofn.lpstrInitialDir = NULL;
				ofn.lpstrTitle      = promptptr;
				ofn.Flags           = OFN_EXPLORER
									| OFN_OVERWRITEPROMPT
									| OFN_HIDEREADONLY
									| OFN_ENABLEHOOK;			// PA: OFN_ENABLEHOOK added from Ronny
				ofn.lpstrDefExt     = NULL;
				ofn.lCustData       = 0;
				ofn.lpfnHook        = &FileSelectorHook;		// PA: &FileSelectorHook instead of NULL from Ronny
				ofn.lpTemplateName  = NULL;

				gProhibitWindowActivation++;
				success = GetSaveFileName (&ofn);
				gProhibitWindowActivation--;

				if (success)
				{
					MakeReturn2Cci (pcci, success, (int) ofn.lpstrFile);
					/* and have the calling clean function deallocate the filename buffer */
				}
				else
				{
					MakeReturn2Cci (pcci, success, (int) NULL);
					rfree (ofn.lpstrFile);
				}
			}
			break;
		/*	CcRqCREATEDIALOG is now restricted to modeless dialogues only. */
		case CcRqCREATEDIALOG:	// textptr,parentptr,behindPtr; HWND result. 
			{
				WORD *p, *pdlgtemplate;
				int nchar;
				DWORD lStyle;
				HWND hwnd,hwndParent,hwndBehind;

				hwndParent = (HWND) pcci->p2;	// The owner window    
				hwndBehind = (HWND) pcci->p3;	// The stacking window 

				// allocate some memory to play with  
				pdlgtemplate = p = (PWORD) rmalloc (1000);

				// start to fill in the dlgtemplate information. Addressing by WORDs 
				lStyle = WS_CAPTION | DS_SETFONT | DS_MODALFRAME | WS_SYSMENU | WS_OVERLAPPED;
			
				*p++ = LOWORD (lStyle);
				*p++ = HIWORD (lStyle);
				*p++ = 0;		// LOWORD (lExtendedStyle) 
				*p++ = 0;		// HIWORD (lExtendedStyle) 
				*p++ = 0;		// NumberOfItems 
				*p++ = 0;		// x  (dummy value) 
				*p++ = 0;		// y  (dummy value) 
				*p++ = 1000;	// cx (dummy value) 
				*p++ = 1000;	// cy (dummy value) 
				*p++ = 0;		// Menu 
				*p++ = 0;		// Class 

				// copy the title of the dialog 
				nchar = nCopyAnsiToWideChar (p, (char *) pcci->p1);
				p += nchar;
				// Font information because of DS_SETFONT 
				*p++ = 8;		// point size 
				nchar = nCopyAnsiToWideChar (p, "MS Sans Serif");		// Face name 
				p += nchar;
				// make sure the first item starts on a DWORD boundary 

				hwnd = CreateDialogIndirectParam (ghInst, (LPDLGTEMPLATE) pdlgtemplate, hwndParent, (DLGPROC) DialogProcedure, (LPARAM) 0);
				LocalFree (LocalHandle (pdlgtemplate));

				if (hwndBehind!=NULL)									// In case the dialog should not be opened top-most: 
				{
					UINT uflags = SWP_NOMOVE + SWP_NOSIZE;				// then restack the dialog. 
					SetWindowPos (hwnd,hwndBehind, 0,0,0,0, uflags);
				}
				ShowWindow (hwnd, SW_SHOWNORMAL);						// Only now the dialog should be made visible. 
				UpdateWindow (hwnd);

				MakeReturn1Cci (pcci, (int) hwnd);
			}
			break;
		/*	Create modal dialogues.
		*/
		case CcRqCREATEMODALDIALOG:	/* textptr,parentptr; error code result. */
			{
				WORD *p, *pdlgtemplate;
				int nchar, errorcode;
				DWORD lStyle;
				HWND hwndParent;
				UINT zeroTimer;

				hwndParent = (HWND) pcci->p2;	/* The owner window    */
				if (hwndParent == 0)
					hwndParent = ghMainWindow;	/* If no parent is passed (NDI) then ghMainWindow should be the parent. */

				/* allocate some memory to play with  */
				pdlgtemplate = p = (PWORD) rmalloc (1000);

				/* start to fill in the dlgtemplate information. Addressing by WORDs */
				lStyle = WS_CAPTION | DS_SETFONT | DS_MODALFRAME | WS_SYSMENU | DS_SYSMODAL | WS_VISIBLE;
			
				*p++ = LOWORD (lStyle);
				*p++ = HIWORD (lStyle);
				*p++ = 0;		/* LOWORD (lExtendedStyle) */
				*p++ = 0;		/* HIWORD (lExtendedStyle) */
				*p++ = 0;		/* NumberOfItems */
				*p++ = 0;		/* x  (dummy value) */
				*p++ = 0;		/* y  (dummy value) */
				*p++ = 1000;	/* cx (dummy value) */
				*p++ = 1000;	/* cy (dummy value) */
				*p++ = 0;		/* Menu */
				*p++ = 0;		/* Class */

				/* copy the title of the dialog */
				nchar = nCopyAnsiToWideChar (p, (char *) pcci->p1);
				p += nchar;
				/* Font information because of DS_SETFONT */
				*p++ = 8;		/* point size */
				nchar = nCopyAnsiToWideChar (p, "MS Sans Serif");		/* Face name */
				p += nchar;
				/* make sure the first item starts on a DWORD boundary */

				/*	Before the modal dialog enters its event loop we create a timer to generate timer events. 
					This is done only once per modal dialog stack.
				*/
				if (ghwndLastModalDialog == NULL)	// The first modal dialog. 
				{
					zeroTimer = SetTimer (NULL, (UINT) 0, (UINT) 0, NULL);
					
					if (zeroTimer==0)				// Creation of zero timer failed. 
					{
						LocalFree (LocalHandle (pdlgtemplate));
						errorcode = 1;				// Report error to Clean side
					}
					else
					{
						DialogBoxIndirectParam (ghInst, (LPDLGTEMPLATE) pdlgtemplate, hwndParent, (DLGPROC) DialogProcedure, (LPARAM) 0);
						LocalFree (LocalHandle (pdlgtemplate));
						KillTimer (NULL, (UINT) zeroTimer);
						errorcode = 0;				// Report no error to Clean side
					}
				}
				else	// a zero timer has already been created. 
				{
					DialogBoxIndirectParam (ghInst, (LPDLGTEMPLATE) pdlgtemplate, hwndParent, (DLGPROC) DialogProcedure, (LPARAM) 0);
					LocalFree (LocalHandle (pdlgtemplate));
					errorcode = 0;					// Report no error to Clean side
				}

				MakeReturn1Cci (pcci,errorcode);
			}
			break;
		/*	Create compound controls (window in window) */
		case CcRqCREATECOMPOUND:	/* hwnd, packed pos,w,h, scrollbars, transparent; HWND result. */
			{
				HWND parentwindow, compoundhandle;
				int left,top, width,height;
				int compoundstyle;
				BOOL transparent;
				DWORD compoundExStyle;

				parentwindow  = (HWND) pcci->p1;
				left          = pcci->p2>>16;
				top           = (pcci->p2<<16)>>16;
				width         = pcci->p3;
				height        = pcci->p4;
				compoundstyle = pcci->p5;
				transparent   = (BOOL) pcci->p6;

				compoundExStyle = WS_EX_CONTROLPARENT;
				if (transparent)
					 compoundExStyle |= WS_EX_TRANSPARENT;

				compoundstyle |= WS_CHILD;// | WS_CLIPSIBLINGS;

				/* create the compound window */
				compoundhandle
					= CreateWindowEx (compoundExStyle,				/* Extended style				 */
									  CompoundControlClassName,		/* Class name					 */
									  "",							/* Window title 				 */
									  compoundstyle,				/* style flags					 */
									  left, top,					/* x, y 						 */
									  width, height,		 		/* width, height				 */
									  parentwindow,					/* Parent window				 */
									  NULL,							/* menu handle					 */
									  (HANDLE) ghInst,				/* Instance that owns the window */
									  0);
				SendMessage  (compoundhandle, WM_SETFONT, (WPARAM)gControlFont, MAKELPARAM (TRUE,0));
				SetWindowPos (compoundhandle, HWND_BOTTOM, 0,0,0,0, SWP_NOMOVE+SWP_NOSIZE);	// This should implement control stack

				MakeReturn1Cci (pcci, (int) compoundhandle);
			}
			break;
		/*	Create scrollbars. */
		case CcRqCREATESCROLLBAR:	/* hwnd, x,y,w,h bool; HWND result. */
			{
				HWND scroll;
				int style;
				int x, y, w, h;
				HWND parent;
				BOOL ishorizontal;

				parent = (HWND) pcci->p1;
				x = pcci->p2;
				y = pcci->p3;
				w = pcci->p4;
				h = pcci->p5;
				ishorizontal = pcci->p6;

				style	= WS_CHILD
						| WS_GROUP
						| WS_CLIPSIBLINGS;

				if (ishorizontal)
					style |= SBS_HORZ;
				else
					style |= SBS_VERT;

				scroll = CreateWindow (	"scrollbar",
										"",
										style,
										x, y, w, h,
										parent,
										0,
										ghInst,
										0);
				SendMessage  (scroll, WM_SETFONT, (WPARAM)gControlFont, MAKELPARAM (TRUE,0));
				SetWindowPos (scroll, HWND_BOTTOM, 0,0,0,0, SWP_NOMOVE+SWP_NOSIZE);	// This should implement control stack

				MakeReturn1Cci (pcci, (int) scroll);
			}
			break;
		case CcRqCREATEBUTTON:	/* hwnd, x,y,w,h, kind;  HWND result. */
			{
				HWND but, parent;
				int style, id;
				int x, y, w, h, kind;

				parent	= (HWND) pcci->p1;
				x		= pcci->p2;
				y		= pcci->p3;
				w		= pcci->p4;
				h		= pcci->p5;
				kind	= pcci->p6;
				style	= WS_CHILD
						| WS_GROUP
						| WS_TABSTOP
						| WS_CLIPSIBLINGS;

				if (kind==ISOKBUTTON)
				{
					style |= BS_DEFPUSHBUTTON;
					id = IDOK;
				}
				else
				{
					style |= BS_PUSHBUTTON;

					if (kind==ISCANCELBUTTON)
					{
						id = IDCANCEL;
					}
					else
					{
						id = 0;
					}
				}

				but = CreateWindow ("button",
									"",
									style,
									x, y, w, h,
									parent,
									(HMENU) id,
									ghInst,
									0);
				SendMessage  (but, WM_SETFONT, (WPARAM)gControlFont, MAKELPARAM (TRUE,0));
				SetWindowPos (but, HWND_BOTTOM, 0,0,0,0, SWP_NOMOVE+SWP_NOSIZE);	// This should implement control stack

				MakeReturn1Cci (pcci, (int) but);
			}
			break;
		case CcRqCREATEICONBUT: /* hwnd, x,y,w,h,kind;	  HWND result. */
			{
				HWND but, parent;
				int style, id, x, y, w, h, kind;

				parent	= (HWND) pcci->p1;
				x		= pcci->p2;
				y		= pcci->p3;
				w		= pcci->p4;
				h		= pcci->p5;
				kind    = pcci->p6;

				style	= WS_CHILD
						| WS_GROUP
						| WS_TABSTOP
						| BS_OWNERDRAW
						| WS_CLIPSIBLINGS;
				
				switch (kind)
				{
					case ISOKBUTTON:
						id = IDOK;
						break;
					case ISCANCELBUTTON:
						id = IDCANCEL;
						break;
					default:
						id = 0;
				}

				but = CreateWindow ("button",
									"", 
									style,
									x, y, w, h,
									parent, 
									(HMENU) id, 
									ghInst, 
									0);
				SendMessage  (but, WM_SETFONT, (WPARAM)gControlFont, MAKELPARAM (TRUE,0));
				SetWindowPos (but, HWND_BOTTOM, 0,0,0,0, SWP_NOMOVE+SWP_NOSIZE);	// This should implement control stack

				MakeReturn1Cci (pcci, (int) but);
			}
			break;
		case CcRqCREATECUSTOM:	/* hwnd, x,y,w,h; HWND result. */
			{
				HWND ctrl, parent;
				int style, x, y, w, h;

				parent	= (HWND) pcci->p1;
				x		= pcci->p2;
				y		= pcci->p3;
				w		= pcci->p4;
				h		= pcci->p5;
				style	= WS_CHILD
						| WS_GROUP
						| WS_TABSTOP
						| WS_CLIPSIBLINGS;		// Necessary to enforce proper control stack

				ctrl = CreateWindow (CustomControlClassName,
									 "",
									 style,
									 x, y, w, h,
									 parent,
									 (HMENU) 0,
									 ghInst,
									 0);
				SendMessage  (ctrl, WM_SETFONT, (WPARAM)gControlFont, MAKELPARAM (TRUE,0));
				SetWindowPos (ctrl, HWND_BOTTOM, 0,0,0,0, SWP_NOMOVE+SWP_NOSIZE);	// This should implement control stack

				MakeReturn1Cci (pcci, (int) ctrl);
			}
			break;
		case CcRqCREATESTATICTXT:		/* hwnd, x,y,w,h; HWND result. */
			{
				HWND handle;
				int x, y, w, h, style;
				HWND parent;

				parent = (HWND) pcci->p1;
				x = pcci->p2;
				y = pcci->p3;
				w = pcci->p4;
				h = pcci->p5;

				style	= WS_CHILD
						| WS_GROUP
						| SS_LEFTNOWORDWRAP	// PA: SS_LEFT replaced by SS_LEFTNOWORDWRAP because text wrapping is not intended for text controls
						| WS_CLIPSIBLINGS;

				handle = CreateWindow (	"static",
										"",
										style,
										x, y, w, h,
										parent,
										0,
										ghInst,
										0);
				SendMessage  (handle, WM_SETFONT, (WPARAM)gControlFont, MAKELPARAM (TRUE,0));
				SetWindowPos (handle, HWND_BOTTOM, 0,0,0,0, SWP_NOMOVE+SWP_NOSIZE);	// This should implement control stack

				MakeReturn1Cci (pcci, (int) handle);
			}
			break;
		case CcRqCREATEEDITTXT: /* hwnd, x,y,w,h, flags; HWND result. */
			{
				HWND handle;
				int x, y, w, h, style, flags;
				HWND parent;
				BOOL isml,isKeySensitive;

				parent			= (HWND) pcci->p1;
				x				= pcci->p2;
				y				= pcci->p3;
				w				= pcci->p4;
				h				= pcci->p5;
				flags			= pcci->p6;
				isml			= flags & EDITISMULTILINE;
				isKeySensitive	= flags & EDITISKEYSENSITIVE;

				style			= WS_CHILD
								| WS_GROUP
								| WS_TABSTOP
								| ES_LEFT
								| WS_CLIPSIBLINGS;

				if (isml)
					style |= ES_AUTOVSCROLL | ES_MULTILINE | ES_WANTRETURN;
				else
					style |= ES_AUTOHSCROLL | ES_MULTILINE;

				handle = CreateWindowEx (WS_EX_CLIENTEDGE,
										 "edit",
										 "",
										 style,
										 x, y, w, h,
										 parent,
										 0,
										 ghInst,
										 0);
				SendMessage  (handle, WM_SETFONT, (WPARAM)gControlFont, MAKELPARAM (TRUE,0));
				SetWindowPos (handle, HWND_BOTTOM, 0,0,0,0, SWP_NOMOVE+SWP_NOSIZE);	// This should implement control stack
				
				/*	Store the standard Windows callback routine adress in stdEditCallback
					and subclass the edit control with EditControlProcedure if isKeySensitive and
					SimpleEditControlProcedure otherwise.
				*/
				if (isKeySensitive)
				{
					stdEditCallback = SetWindowLong (handle, GWL_WNDPROC, (LONG) EditControlProcedure);
				}
				else
				{
					stdEditCallback = SetWindowLong (handle, GWL_WNDPROC, (LONG) SimpleEditControlProcedure);
				}

				MakeReturn1Cci (pcci, (int) handle);
			}
			break;
		case CcRqCREATERADIOBUT:		/* hwnd, x,y,w,h, isfirst;	HWND result. */
			{
				HWND handle;
				int x, y, w, h, style;
				HWND parent;
				BOOL first;

				parent = (HWND) pcci->p1;
				x = pcci->p2;
				y = pcci->p3;
				w = pcci->p4;
				h = pcci->p5;
				first = pcci->p6;

				style	= WS_CHILD			
						| BS_AUTORADIOBUTTON	
						| WS_CLIPSIBLINGS;
				if (first)
					style |= WS_GROUP | WS_TABSTOP;

				handle = CreateWindow (	"button",
										"", 
										style,
										x, y, w, h,
										parent,
										0,
										ghInst,
										0);
				SendMessage  (handle, WM_SETFONT, (WPARAM)gControlFont, MAKELPARAM (TRUE,0));
				SetWindowPos (handle, HWND_BOTTOM, 0,0,0,0, SWP_NOMOVE+SWP_NOSIZE);	// This should implement control stack

				MakeReturn1Cci (pcci, (int) handle);
			}
			break;
		case CcRqCREATECHECKBOX:		/* hwnd, x,y,w,h, isfirst; HWND result. */
			{
				HWND handle;
				int x, y, w, h, style;
				HWND parent;
				BOOL first;

				parent = (HWND) pcci->p1;
				x = pcci->p2;
				y = pcci->p3;
				w = pcci->p4;
				h = pcci->p5;
				first = pcci->p6;

				style	= WS_CHILD
						| BS_AUTOCHECKBOX
						| WS_CLIPSIBLINGS;
				if (first)
					style |= WS_GROUP | WS_TABSTOP;

				handle = CreateWindow (	"button",
										"",
										style,
										x, y, w, h,
										parent,
										0,
										ghInst,
										0);
				SendMessage  (handle, WM_SETFONT, (WPARAM)gControlFont, MAKELPARAM (TRUE,0));
				SetWindowPos (handle, HWND_BOTTOM, 0,0,0,0, SWP_NOMOVE+SWP_NOSIZE);	// This should implement control stack

				MakeReturn1Cci (pcci, (int) handle);
			}
			break;
		case CcRqSETITEMCHECK:	/* hwnd, bool; no result. */
			{
				HWND hwnd;
				BOOL on;
				int check;

				hwnd = (HWND) pcci->p1;
				on = pcci->p2;

				if (on)
					check = 1;
				else
					check = 0;

				SendMessage (hwnd, BM_SETCHECK, (WPARAM) check, 0);

				MakeReturn0Cci (pcci);
			}
			break;
		case CcRqENABLECONTROL: /* hwnd, bool; no result. */
			{
				HWND window;
				BOOL wasUnable, newSelect;

				window    = (HWND) pcci->p1;
				newSelect = (BOOL) pcci->p2;
				
				wasUnable = EnableWindow (window, newSelect);
				if (wasUnable != newSelect)
					InvalidateRect (window, NULL, FALSE);

				MakeReturn0Cci (pcci);
			}
			break;
		case CcRqSHOWCONTROL:	// hwnd, bool; no result. 
			{
				int nCmdShow;

				if (pcci->p2) 
					nCmdShow = SW_SHOWNA;
				else
					nCmdShow = SW_HIDE;

				ShowWindow ((HWND) pcci->p1, nCmdShow);
				MakeReturn0Cci (pcci);
			}
			break;
		/* Hide/show windows. */
		case CcRqSHOWWINDOW:	/* hwnd, show, activate; no result. */
			{
				int nCmdShow;
				BOOL show, activate;
				HWND wPtr;

				wPtr     = (HWND) pcci->p1;
				show     = (BOOL) pcci->p2;
				activate = (BOOL) pcci->p3;

				if (!show)
					nCmdShow = SW_HIDE;
				else if (activate)
					nCmdShow = SW_SHOW;
				else 
					nCmdShow = SW_SHOWNOACTIVATE;

				ShowWindow (wPtr, nCmdShow);
				MakeReturn0Cci (pcci);
			}
			break;
		case CcRqCREATEPOPUP:	/* hwnd, x,y,w,h,isEditable;  HWND hwndPopUp,hwndEdit (if isEditable). */
			{
				HWND parent, hwndPopUp, hwndEdit;
				int x, y, w, h;
				int style;
				BOOL isEditable;

				parent     = (HWND) pcci->p1;
				x          = pcci->p2;
				y          = pcci->p3;
				w          = pcci->p4;
				h          = pcci->p5;
				isEditable = (BOOL) pcci->p6;

				style	= WS_CHILD
						| WS_GROUP
						| WS_TABSTOP
						| WS_VSCROLL
						| CBS_AUTOHSCROLL
					//	| CBS_OWNERDRAWFIXED	// PA: removed, otherwise garbage strings appear for empty popups.
						| CBS_HASSTRINGS
						| WS_CLIPSIBLINGS;
				if (isEditable)
					style |= CBS_DROPDOWN;
				else
					style |= CBS_DROPDOWNLIST;

				hwndPopUp = CreateWindowEx (0,
										 "COMBOBOX",
										 "",
										 style,
										 x, y, w, h,
										 parent,
										 (HMENU) 0,
										 ghInst,
										 0);
				SendMessage  (hwndPopUp, CB_SETEXTENDEDUI, (WPARAM) TRUE, 0);
				SendMessage  (hwndPopUp, WM_SETFONT, (WPARAM)gControlFont, MAKELPARAM (TRUE,0));
				SetWindowPos (hwndPopUp, HWND_BOTTOM, 0,0,0,0, SWP_NOMOVE+SWP_NOSIZE);	// This should implement control stack

				/*	Store the standard Windows callback routine adress in stdPopUpCallback
					and subclass the combobox's edit control with PopUpControlProcedure.
				*/
				if (isEditable)
				{
					hwndEdit = GetWindow (hwndPopUp,GW_CHILD);
					stdPopUpCallback = SetWindowLong (hwndEdit, GWL_WNDPROC, (LONG) PopUpControlProcedure);
				}
				else
				{
					hwndEdit = 0;
				}
				MakeReturn2Cci (pcci, (int) hwndPopUp, (int) hwndEdit);
			}
			break;
		case CcRqADDTOPOPUP:	/* hwnd, textptr, enabled, selected, index; Pos result. */
			{
				int  index, pos;
				HWND hpopup;
				char *text;
				BOOL enabled, selected;

				hpopup   = (HWND) pcci->p1;
				text     = (char *) pcci->p2;
				enabled  = (BOOL) pcci->p3;
				selected = (BOOL) pcci->p4;
				index    = (int)  pcci->p5;

				pos = SendMessage (hpopup, CB_ADDSTRING, 0, (LPARAM) text);
				//pos = SendMessage (hpopup, CB_INSERTSTRING, (WPARAM) index, (LPARAM) text);

				SendMessage (hpopup, CB_SETITEMDATA, (WPARAM) pos, (LPARAM) enabled);

				if (selected)
					SendMessage (hpopup, CB_SETCURSEL, (WPARAM) pos, 0);

				MakeReturn1Cci (pcci, pos);
			}
			break;
		case CcRqSELECTPOPUPITEM:		/* hwnd, pos; no result */
			{
				HWND hpopup;
				int pos;

				hpopup = (HWND) pcci->p1;
				pos = pcci->p2;

				SendMessage (hpopup, CB_SETCURSEL, (WPARAM) pos, 0);

				MakeReturn0Cci (pcci);
			}
			break;
		case CcRqENABLEPOPUPITEM:		/* hwnd, pos, enabled; no result */
			{
				HWND hpopup;
				int pos;
				LPARAM enabled;

				hpopup = (HWND) pcci->p1;
				pos = pcci->p2;
				enabled = (LPARAM) pcci->p3;

				SendMessage (hpopup, CB_SETITEMDATA, (WPARAM) pos, enabled);
				if (SendMessage (hpopup, CB_GETCURSEL, 0, 0) == pos)
				{
					InvalidateRect (hpopup, NULL, TRUE);
				}

				MakeReturn0Cci (pcci);
			}
			break;
		/*	CcRqCLIPBOARDHASTEXT had no implementation. It checks if the clipboard has a CF_TEXT item.
		*/
		case CcRqCLIPBOARDHASTEXT:		/* no arguments; bool result. */
			{
				BOOL ok = IsClipboardFormatAvailable (CF_TEXT);
				MakeReturn1Cci (pcci,(int) ok);
			}
			break;
		case CcRqSETCLIPBOARDTEXT:		/* textptr; no result. */
			{
				HANDLE hClipText;
				char *pClipText;
				int len;
				char *txt;

				txt = (char *) pcci->p1;

				len = lstrlen (txt);
				hClipText = GlobalAlloc (GMEM_MOVEABLE | GMEM_DDESHARE, len + 1);

				if (hClipText != NULL)
				{
					pClipText = GlobalLock (hClipText);
					lstrcpyn (pClipText, txt, len + 1);
					GlobalUnlock (hClipText);

					OpenClipboard (ghMainWindow);
					EmptyClipboard ();
					SetClipboardData (CF_TEXT, hClipText);
					CloseClipboard ();
				}
				MakeReturn0Cci (pcci);
			} break;
		case CcRqGETCLIPBOARDTEXT:			/* no params; string result. */
			{
				HANDLE hClipText;
				char *pClipText;
				char *result;
				int len;

				OpenClipboard (ghMainWindow);
				hClipText = GetClipboardData (CF_TEXT);

				if (hClipText == NULL)
				{
					result = rmalloc (1);
					result[0] = '\0';
				}
				else
				{
					len = GlobalSize (hClipText) - 1;
					result = rmalloc (len + 1);

					pClipText = GlobalLock (hClipText);
					len = lstrlen (pClipText);

					lstrcpyn (result, pClipText, len + 1);
					GlobalUnlock (hClipText);
				}
				CloseClipboard ();

				MakeReturn1Cci (pcci, (int) result);
			} break;
		/*	CcRqGETCLIPBOARDCOUNT is used by Clean to retrieve current clipboard count.
		*/
		case CcRqGETCLIPBOARDCOUNT:	/* no arguments; gClipboardCount result. */
			{
				MakeReturn1Cci (pcci, gClipboardCount);
			} break;
		/*	Implement WinRestackWindow as a crosscall operation.
		*/
		case CcRqRESTACKWINDOW:		/* thewindow,behind; no result. */
			{
				HWND thePtr, behindPtr;
				UINT uflags = SWP_NOMOVE + SWP_NOSIZE;	/*	Do not change current size or location */

				thePtr    = (HWND) pcci->p1;
				behindPtr = (HWND) pcci->p2;

				SetWindowPos (thePtr, behindPtr, 0, 0, 0, 0, uflags);

				MakeReturn0Cci (pcci);
			} break;
		/*	Add controls to tooltip area. */
		case CcRqADDCONTROLTIP: /* parentPtr, controlPtr, textPtr; no result. */
			{
				HWND hwndParent,hwndControl;	/* The handle to the window and control. */
				TOOLINFO ti;					/* The tool information that is sent to the tooltip control. */
				char *text;

				hwndParent  = (HWND) pcci->p1;
				hwndControl = (HWND) pcci->p2;
				text        = (char *)pcci->p3;
				
				/* Fill the tooltip info with the appropriate information. */
				ti.cbSize     = sizeof(TOOLINFO);
				ti.uFlags     = TTF_IDISHWND | TTF_SUBCLASS;
				ti.hwnd       = hwndParent;
				ti.uId        = (UINT) hwndControl;
				ti.rect.left  = 0;
				ti.rect.top   = 0;
				ti.rect.right = 0;
				ti.rect.bottom= 0;
				ti.hinst      = ghInst;
				ti.lpszText   = (LPSTR) text;

				SendMessage (ghwndTT, TTM_ADDTOOL, 0, (LPARAM) (LPTOOLINFO)&ti);
				MakeReturn0Cci (pcci);
			}
			break;
		/*	Remove controls from tooltip area. */
		case CcRqDELCONTROLTIP: /* parentPtr, controlPtr; no result. */
			{
				HWND hwndParent,hwndControl;		/* The handle to the window and control. */
				TOOLINFO ti;						/* The tool information that is sent to the tooltip control. */
				
				hwndParent  = (HWND) pcci->p1;
				hwndControl = (HWND) pcci->p2;

				/* Fill the tooltip info with the appropriate information. */
				ti.cbSize     = sizeof(TOOLINFO);
				ti.uFlags     = TTF_IDISHWND;
				ti.hwnd       = hwndParent;
				ti.uId        = (UINT) hwndControl;

				SendMessage (ghwndTT, TTM_DELTOOL, 0, (LPARAM) (LPTOOLINFO)&ti);
				MakeReturn0Cci (pcci);
			}
			break;
// MW...
		case CcRqDO_PRINT_SETUP:
			{	int ok;
				PRINTDLG *pdPtr;
				printSetup(0, pcci->p1,
							(char*) pcci->p2, (char*) pcci->p3, (char*) pcci->p4, (char*) pcci->p5,
							&ok, &pdPtr);
				MakeReturn2Cci (pcci, ok, (int) pdPtr);
			} break;
		case CcRqGET_PRINTER_DC:
			{	int doDialog,emulateScreenRes,
					err,first,last,copies,pPrintDlg,deviceContext;
	
				// unpack doDialog and emulateScreenRes
				doDialog			= (pcci->p1) & 1;
				emulateScreenRes	= (pcci->p1) & 2;

				getDC(	doDialog,emulateScreenRes,FALSE,pcci->p2,
						(char*) pcci->p3,(char*) pcci->p4,(char*) pcci->p5,(char*) pcci->p6,
						&err,&first,&last,&copies,(PRINTDLG**)&pPrintDlg,&deviceContext);
				MakeReturn6Cci (pcci,err,first,last,copies,pPrintDlg,deviceContext);
			} break;
		case CcRqSTARTDOC:
			{	
				HDC hdc = (HDC) pcci->p1;
				int err;
				
				EnableWindow (ghMainWindow, FALSE) ;
				hDlgPrint = CreateCancelDialog ();
				SetAbortProc (hdc, AbortProc) ;
				err = startDoc((int) hdc);
				if (err<=0 && ghMainWindow!=NULL && !bUserAbort)
					{
						EnableWindow (ghMainWindow, TRUE) ;
				        DestroyWindow (hDlgPrint) ;
					};
				MakeReturn1Cci (pcci,err);
			} break;
		case CcRqENDDOC:
			{	
				HDC hdc = (HDC) pcci->p1;
				
				endDoc((int) hdc);
				if (ghMainWindow!=NULL && !bUserAbort)
					{
						EnableWindow (ghMainWindow, TRUE) ;
				        DestroyWindow (hDlgPrint) ;
					};
				MakeReturn0Cci (pcci);
			} break;
		case CcRqDISPATCH_MESSAGES_WHILE_PRINTING:
			{	
				MSG   msg ;
				char	*pageMessage= (char*) (pcci->p1);

				SetWindowText(hwndText,pageMessage);
				
				while (!bUserAbort && PeekMessage (&msg, NULL, 0, 0, PM_REMOVE))
					{
					if (!hDlgPrint || !IsDialogMessage (hDlgPrint, &msg))
						{
						TranslateMessage (&msg) ;
						DispatchMessage (&msg) ;
						}
					}
				MakeReturn0Cci (pcci);
			} break;
#ifdef HILT
		case CcRqDO_HTML_HELP:
			{	
			char	*chmFileAndWType;
			HWND	hwnd,hwndHelp;
			int		command;
			
			chmFileAndWType	= (char*) (pcci->p1);
			hwnd			= (HWND) (pcci->p2);
			command			= pcci->p3;
			if (!htmlHelpInitialized)
				{
				HtmlHelp(hwnd, NULL, HH_INITIALIZE, (DWORD)&htmlHelpCookie);
				htmlHelpInitialized = TRUE;
				};
			hwndHelp = HtmlHelp(hwnd,chmFileAndWType,command,(DWORD)NULL);
			if (hwndHelp==0)
				rMessageBox(hwnd, MB_APPLMODAL, "Error",
							"htmlHelp called with wrong parameters");
			MakeReturn0Cci (pcci);
			} break;
#endif
// ... MW
		default:
			{
				rprintf ("\'HandleCleanRequest\' got unkown request from Clean:  ");
				printCCI (pcci);
				ErrorExit ("\'HandleCleanRequest\' got unkown request from Clean: %d\n", pcci->mess);
			}
	}
	KickCleanThread (pcci);
}


OS
WinDestroyMenu (HMENU menu, OS os)
{
	rprintf ("DestroyMenu: menu = %d\n", (int) menu);

	DestroyMenu (menu);
	return (os);
}

BOOL
CleanThreadRunning (void)
{
	return GetCurrentThreadId () == gCleanThreadId;
}

BOOL
OsThreadRunning (void)
{
	return GetCurrentThreadId () == gOsThreadId;
}

extern EXPORT_TO_CLEAN void
WinInitOs (Bool * ok, OS * os)
{
	if (gEventsInited)
	{
		*ok = FALSE;
		rprintf ("WIO: *ok = FALSE\n");
	}
	else
	{
		*ok = TRUE;
		gEventsInited = TRUE;
		rprintf ("WIO: *ok = TRUE\n");
	}

	*os = 54321;
}

extern EXPORT_TO_CLEAN Bool
WinCloseOs (OS os)
{
	if (gEventsInited)
	{
		rprintf ("WCO: return TRUE\n");
		gEventsInited = FALSE;
		return TRUE;
	}
	else
	{
		rprintf ("WCO: return FALSE\n");
		return FALSE;
	}
}

extern EXPORT_TO_CLEAN OS
WinStartOsThread (OS os)
{
	rprintf ("WSOT: Started\n");

	InitGlobals ();
	rprintf ("WSOT: Globals Inited\n");
	gCLEAN_DONE = CreateEvent (NULL,	/* Default security attributes	*/
							   FALSE,	/* Not a manual-reset event 	*/
							   FALSE,	/* Initial state nonsignalled	*/
							   NULL);	/* No name						*/
	Check ((BOOL) gCLEAN_DONE, "\'InitOs\' could not create first event object");
	rprintf ("WSOT: CLEANDONE event created\n");

	gOS_DONE = CreateEvent (NULL, FALSE, FALSE, NULL);
	Check ((BOOL) gOS_DONE, "\'InitOs\' could not create second event object");
	rprintf ("WSOT: OS_DONE event created\n");

	gCleanThreadId = GetCurrentThreadId ();
	rprintf ("WSOT: gor current thread id\n");

	ghOSThreadHandle = CreateThread (NULL,				/* Default security attributes		*/
									 0,					/* Default stacksize				*/
								  (LPTHREAD_START_ROUTINE) OsThreadFunction,
									 0,					/* parameter to thread function		*/
									 0,					/* Not initially suspended			*/
									 &(gOsThreadId));	/* store threadId here				*/
	Check ((BOOL) ghOSThreadHandle, "\'InitOs\' could not create second thread");	// PA!!! test fails
	rprintf ("WSOT: new thread created\n");
	WaitForSingleObject (gOS_DONE, INFINITE);
	rprintf ("WSOT: wait done\n");

	return os;
}

extern EXPORT_TO_CLEAN OS
WinKillOsThread (OS os)
{
	rprintf ("OS WinKillOsThread (OS os)\n");
#ifdef HILT
	if (htmlHelpInitialized)
		{
		HtmlHelp(NULL, NULL, HH_UNINITIALIZE, htmlHelpCookie);
		htmlHelpInitialized = FALSE;
		};
#endif
	if (ghOSThreadHandle != NULL)
	{
		rprintf ("ghOSThreadHandle != NULL\n");
		TerminateThread (ghOSThreadHandle, 0);
		rprintf ("		TerminateThread(ghOSThreadHandle, 0);\n");
		ghOSThreadHandle = NULL;
		rprintf ("		ghOSThreadHandle = NULL;\n");

		/* CleanUp */
		if (gAcceleratorTable)
			DestroyAcceleratorTable (gAcceleratorTable);
		rprintf ("		if (gAcceleratorTable) \n");
		rprintf ("				DestroyAcceleratorTable( gAcceleratorTable );\n");
		DeleteCursors ();
		rprintf ("		DeleteCursors();\n");
		CloseHandle (gOS_DONE);
		gOS_DONE = NULL;
		rprintf ("		CloseHandle(gOS_DONE);	  gOS_DONE	  = NULL;\n");
		CloseHandle (gCLEAN_DONE);
		gCLEAN_DONE = NULL;
		rprintf ("		CloseHandle(gCLEAN_DONE); gCLEAN_DONE = NULL;\n");
		if (gDlogFont != NULL)
			DeleteObject (gDlogFont);
		rprintf ("		if(gDlogFont != NULL)\n");
		rprintf ("		   DeleteObject(gDlogFont);\n");
		if (gWinFont != NULL)
			DeleteObject (gWinFont);
		rprintf ("      if(gWinFont != NULL)\n");
		rprintf ("         DeleteObject(gWinFont);\n");

		DeleteObject (gControlFont);	// The global logical font must be deleted.
	};
// MW...
	ghMainWindow = NULL;
// ... MW

	return os;
}

extern EXPORT_TO_CLEAN void
WinKickOsThread (int imess,
				 int ip1, int ip2, int ip3,
				 int ip4, int ip5, int ip6,
				 OS ios,
				 int *omess,
				 int *op1, int *op2, int *op3,
				 int *op4, int *op5, int *op6,
				 OS * oos)
{

/*				rprintf("KOT: filling in Cci\n"); */
	rprintf ("WinKickOsThread (");
	printCCI (&gCci);
	rprintf (")\n");

	gCci.mess = imess;
	gCci.p1 = ip1;
	gCci.p2 = ip2;
	gCci.p3 = ip3;
	gCci.p4 = ip4;
	gCci.p5 = ip5;
	gCci.p6 = ip6;

	if (ghOSThreadHandle != NULL)
	{
		rprintf ("KOT: Cci filled in, setting event\n");
		SetEvent (gCLEAN_DONE);
		rprintf ("KOT: Event set, start wait.\n");
		WaitForSingleObject (gOS_DONE, INFINITE);
		rprintf ("KOT: wait done, reading out Cci.\n");
		*omess = gCci.mess;
		*op1 = gCci.p1;
		*op2 = gCci.p2;
		*op3 = gCci.p3;
		*op4 = gCci.p4;
		*op5 = gCci.p5;
		*op6 = gCci.p6;
		*oos = ios;
		rprintf ("KOT: Cci read: {");
		printCCI (&gCci);
		rprintf ("}\n");
	}
	else
	{
		rprintf ("KOT: no thread existed, returning CcWASQUIT for <");
		printCCI (&gCci);
		rprintf (">\n");
		*omess = CcWASQUIT;
		*op1 = 0;
		*op2 = 0;
		*op3 = 0;
		*op4 = 0;
		*op5 = 0;
		*op6 = 0;
		*oos = ios;
	}
	rprintf ("KOT: done.\n");
}

#define PRINTCROSSCALLS

#ifdef PRINTCROSSCALLS
static CrossCallInfo osstack[10];
static CrossCallInfo clstack[10];
static int ossp = -1;
static int clsp = -1;
#endif

void
KickCleanThread (CrossCallInfo * pcci)
{

#ifdef PRINTCROSSCALLS
	if (ossp == -1)
	{
		for (ossp = 0; ossp < 10; ossp++)
		{
			osstack[ossp].mess = -1;
		}
		ossp = 1;
		osstack[ossp].mess = -2;
	}

	if (clsp == -1)
	{
		for (clsp = 0; clsp < 10; clsp++)
		{
			clstack[clsp].mess = -1;
		}
		clsp = 1;
		clstack[clsp].mess = -2;
	}
#endif

	if (pcci != &gCci)
		gCci = *pcci;
	rprintf ("KCT: started\n");

#ifdef PRINTCROSSCALLS
	if (gCci.mess < 20)
	{
		rprintf ("	-- %d --> OS returning <", clsp + ossp - 2);
		printCCI (&gCci);
		rprintf ("> from <");
		printCCI (&(clstack[clsp]));
		rprintf (">\n");
		clsp--;
	}
	else
	{
		ossp++;
		osstack[ossp] = gCci;
		rprintf ("	-- %d --> OS calling with <", clsp + ossp - 2);
		printCCI (&gCci);
		rprintf (">\n");
	}
#endif

	rprintf ("KCT: setting event\n");
	SetEvent (gOS_DONE);
	rprintf ("KCT: starting wait\n");
	WaitForSingleObject (gCLEAN_DONE, INFINITE);
	rprintf ("KCT: wait done.\n");

	if (pcci != &gCci)
		*pcci = gCci;

#ifdef PRINTCROSSCALLS
	if (gCci.mess < 20)
	{
		rprintf (" <-- %d --  Clean returning <", clsp + ossp - 2);
		printCCI (&gCci);
		rprintf ("> from <");
		printCCI (&(osstack[ossp]));
		rprintf (">\n");
		ossp--;
	}
	else
	{
		clsp++;
		clstack[clsp] = gCci;
		rprintf (" <-- %d --  Clean calling with <", clsp + ossp - 2);
		printCCI (&gCci);
		rprintf (">\n");
	}
#endif
}

void
SendMessageToClean (int mess,
					int p1, int p2, int p3,
					int p4, int p5, int p6)
{
	gCci.mess = mess;
	gCci.p1 = p1;
	gCci.p2 = p2;
	gCci.p3 = p3;
	gCci.p4 = p4;
	gCci.p5 = p5;
	gCci.p6 = p6;

	KickCleanThread (&gCci);
	while (!IsReturnCci (&gCci))
	{
		HandleCleanRequest (&gCci);
	}
}

extern double c_div_real (double n, double d);
extern int c_ftoi (double d);

DWORD
OsThreadFunction (DWORD param)
{
	WNDCLASS wclass;
	int width, height;
	HMENU mainSystemMenu;

	/* register custom control class */
	wclass.style         = CS_NOCLOSE | CS_PARENTDC;
	wclass.lpfnWndProc   = (WNDPROC) CustomControlProcedure;
	wclass.cbClsExtra    = 0;
	wclass.cbWndExtra    = 0;
	wclass.hInstance     = ghInst;
	wclass.hIcon         = NULL;
	wclass.hCursor       = LoadCursor (ghInst, IDC_ARROW);
	wclass.hbrBackground = (HBRUSH) (COLOR_BTNFACE + 1);
	wclass.lpszMenuName  = NULL;
	wclass.lpszClassName = CustomControlClassName;
	RegisterClass (&wclass);

	/* register clean compound control class */
	wclass.style         = CS_PARENTDC;
	wclass.lpfnWndProc   = (WNDPROC) CompoundControlProcedure;
	wclass.cbClsExtra    = 0;
	wclass.cbWndExtra    = 0;
	wclass.hInstance     = ghInst;
	wclass.hIcon         = NULL;
	wclass.hCursor       = LoadCursor (ghInst, IDC_ARROW);
	wclass.hbrBackground = (HBRUSH) (COLOR_BTNFACE + 1);//(NULL_BRUSH);
	wclass.lpszMenuName  = NULL;
	wclass.lpszClassName = CompoundControlClassName;
	RegisterClass (&wclass);

	/* register main window class */
	wclass.style         = CS_NOCLOSE;
	wclass.lpfnWndProc   = (WNDPROC) MainWindowProcedure;
	wclass.cbClsExtra    = 0;
	wclass.cbWndExtra    = 0;
	wclass.hInstance     = ghInst;
	wclass.hIcon         = LoadIcon (ghInst, IDI_APPLICATION);
	wclass.hCursor       = LoadCursor (ghInst, IDC_ARROW);
	wclass.hbrBackground = NULL;
	wclass.lpszMenuName  = NULL;
	wclass.lpszClassName = MainWindowClassName;
	RegisterClass (&wclass);

	/* register clean SDI window class */
	wclass.style         = 0;
	wclass.lpfnWndProc   = (WNDPROC) SDIWindowProcedure;
	wclass.cbClsExtra    = 0;
	wclass.cbWndExtra    = sizeof (HANDLE);						// Allocate local memory for LocalWindowData
	wclass.hInstance     = ghInst;
	wclass.hIcon         = NULL;
	wclass.hCursor       = NULL;								// Must be NULL, otherwise SetCursor won't have effect.
	wclass.hbrBackground = (HBRUSH) GetStockObject (WHITE_BRUSH);
	wclass.lpszMenuName  = NULL;
	wclass.lpszClassName = SDIWindowClassName;
	RegisterClass (&wclass);

	/* register clean SDI frame class */
	wclass.style         = 0;
	wclass.lpfnWndProc   = (WNDPROC) SDIFrameProcedure;
	wclass.cbClsExtra    = 0;
	wclass.cbWndExtra    = sizeof (HANDLE);						// Allocate local memory for shortcut table pointer
	wclass.hInstance     = ghInst;
	wclass.hIcon         = LoadIcon (ghInst, IDI_APPLICATION);
	wclass.hCursor       = LoadCursor (ghInst, IDC_ARROW);
	wclass.hbrBackground = (HBRUSH) GetStockObject (WHITE_BRUSH);
	wclass.lpszMenuName  = NULL;
	wclass.lpszClassName = SDIFrameClassName;
	RegisterClass (&wclass);

	/* register clean MDI window class */
	wclass.style         = 0;
	wclass.lpfnWndProc   = (WNDPROC) MDIWindowProcedure;
	wclass.cbClsExtra    = 0;
	wclass.cbWndExtra    = sizeof (HANDLE);						// Allocate local memory for LocalWindowData
	wclass.hInstance     = ghInst;
	wclass.hIcon         = LoadIcon (ghInst, IDI_APPLICATION);
	wclass.hCursor       = NULL;								// Must be NULL, otherwise SetCursor won't have effect.
	wclass.hbrBackground = (HBRUSH) GetStockObject (WHITE_BRUSH);
	wclass.lpszMenuName  = NULL;
	wclass.lpszClassName = MDIWindowClassName;
	RegisterClass (&wclass);

	/* register clean MDI frame class */
	wclass.style         = 0;
	wclass.lpfnWndProc   = (WNDPROC) MDIFrameProcedure;
	wclass.cbClsExtra    = 0;
	wclass.cbWndExtra    = sizeof (HANDLE);						// Allocate local memory for shortcut table pointer
	wclass.hInstance     = ghInst;
	wclass.hIcon         = LoadIcon (ghInst, IDI_APPLICATION);
	wclass.hCursor       = LoadCursor (ghInst, IDC_ARROW);
	wclass.hbrBackground = (HBRUSH) (COLOR_APPWORKSPACE+1);		// For best results (Petzold)
	wclass.lpszMenuName  = NULL;
	wclass.lpszClassName = MDIFrameClassName;
	RegisterClass (&wclass);

/* Mike... */
    RegisterGameWindowClass ();
/* ...Mike */

	/* initialise the common control library. */
	InitCommonControls ();

	GetAppFileName ();

	width = GetSystemMetrics (SM_CXMAXIMIZED) - 2 * GetSystemMetrics (SM_CXSIZEFRAME);
	height = 2 * GetSystemMetrics (SM_CYSIZEFRAME) + GetSystemMetrics (SM_CYCAPTION) + GetSystemMetrics (SM_CYMENU);

	ghMainWindow
		= CreateWindow (MainWindowClassName,	/* Class name					 */
						(LPCTSTR) gAppName, 	/* Window title 				 */
						WS_OVERLAPPEDWINDOW,	/* style flags					 */
						0, -5 - height,			/* x, y 						 */
						width, height,			/* width, height 				 */
						NULL,					/* Parent window				 */
						NULL,					/* menu handle					 */
						(HANDLE) ghInst,		/* Instance that owns the window */
						0);
	/*	Don't show the main window. This will result in one button less in the taskbar.
	ShowWindow (ghMainWindow, SW_SHOWNORMAL);
	*/
	/*	Before creating Clean controls, the tooltip control is created as the topmost child of this window. */
	ghwndTT = CreateWindowEx (	WS_EX_TOPMOST,					// Apply the topmost style for this window
								TOOLTIPS_CLASS,					// Class name
								NULL,							// Title (NULL)
								WS_POPUP | TTS_ALWAYSTIP,		// Style *must* be WS_POPUP
								CW_USEDEFAULT,					// Default position (x,y)
								CW_USEDEFAULT,
								CW_USEDEFAULT,					// Default size (w,h)
								CW_USEDEFAULT,
								ghMainWindow,					// Parent is the ghMainWindow
								(HMENU) NULL,					// No menu
								(HANDLE) ghInst,				// The instance
								NULL							// No window creation data
							 );
	
	mainSystemMenu = GetSystemMenu (ghMainWindow,FALSE);
	RemoveMenu (mainSystemMenu, SC_RESTORE,  MF_BYCOMMAND);
	RemoveMenu (mainSystemMenu, SC_MOVE,     MF_BYCOMMAND);
	RemoveMenu (mainSystemMenu, SC_SIZE,     MF_BYCOMMAND);
	RemoveMenu (mainSystemMenu, SC_MINIMIZE, MF_BYCOMMAND);
	RemoveMenu (mainSystemMenu, SC_MAXIMIZE, MF_BYCOMMAND);
	DrawMenuBar (ghMainWindow);

	KickCleanThread (MakeReturn0Cci (&gCci));

	while (1)
	{
		HandleCleanRequest (&gCci);
	}

	MakeReturn0Cci (&gCci);
	SetEvent (gOS_DONE);

	return 0;
}

CrossCallInfo *
MakeReturn0Cci (CrossCallInfo * pcci)
{
	pcci->mess = CcRETURN0;
	return pcci;
}

CrossCallInfo *
MakeReturn1Cci (CrossCallInfo * pcci, int v)
{
	pcci->mess = CcRETURN1;
	pcci->p1 = v;
	return pcci;
}

CrossCallInfo *
MakeReturn2Cci (CrossCallInfo * pcci, int v1, int v2)
{
	pcci->mess = CcRETURN2;
	pcci->p1 = v1;
	pcci->p2 = v2;
	return pcci;
}

CrossCallInfo *
MakeReturn3Cci (CrossCallInfo * pcci, int v1, int v2, int v3)
{
	pcci->mess = CcRETURN3;
	pcci->p1 = v1;
	pcci->p2 = v2;
	pcci->p3 = v3;
	return pcci;
}

CrossCallInfo *
MakeReturn4Cci (CrossCallInfo * pcci, int v1, int v2, int v3, int v4)
{
	pcci->mess = CcRETURN4;
	pcci->p1 = v1;
	pcci->p2 = v2;
	pcci->p3 = v3;
	pcci->p4 = v4;
	return pcci;
}

CrossCallInfo *
MakeReturn5Cci (CrossCallInfo * pcci, int v1, int v2, int v3, int v4, int v5)
{
	pcci->mess = CcRETURN5;
	pcci->p1 = v1;
	pcci->p2 = v2;
	pcci->p3 = v3;
	pcci->p4 = v4;
	pcci->p5 = v5;
	return pcci;
}

CrossCallInfo *
MakeReturn6Cci (CrossCallInfo * pcci, int v1, int v2, int v3, int v4, int v5, int v6)
{
	pcci->mess = CcRETURN6;
	pcci->p1 = v1;
	pcci->p2 = v2;
	pcci->p3 = v3;
	pcci->p4 = v4;
	pcci->p5 = v5;
	pcci->p6 = v6;
	return pcci;
}

BOOL
IsReturnCci (CrossCallInfo * pcci)
{
	if (pcci->mess >= CcRETURNmin && pcci->mess <= CcRETURNmax)
		return TRUE;

	return FALSE;
}

extern EXPORT_TO_CLEAN void
WinScreenYSize (OS ios, int *py, OS * oos)
{
	*py = GetSystemMetrics (SM_CYSCREEN);

	*oos = ios;
}

extern EXPORT_TO_CLEAN void
WinScreenXSize (OS ios, int *px, OS * oos)
{
	*px = GetSystemMetrics (SM_CXSCREEN);

	*oos = ios;
}

extern EXPORT_TO_CLEAN void
WinMinimumWinSize (int *mx, int *my)
{
	*mx = 48;
	*my = 0;
}

/*	WinScrollbarSize determines system metrics of width and height of scrollbars.
*/
extern EXPORT_TO_CLEAN void
WinScrollbarSize (OS ios, int *width, int *height, OS * oos)
{
	*width  = GetSystemMetrics (SM_CXVSCROLL);
	*height = GetSystemMetrics (SM_CYHSCROLL);

	*oos = ios;
}

extern EXPORT_TO_CLEAN void
WinMaxFixedWindowSize (int *mx, int *my)
{
	*mx = GetSystemMetrics (SM_CXMAXIMIZED) -		/* MAXIMIZED size	  */
	4 * GetSystemMetrics (SM_CXFIXEDFRAME); 		/* window borders	  */

	*my = GetSystemMetrics (SM_CYMAXIMIZED) -		/* MAXIMIZED size	  */
		4 * GetSystemMetrics (SM_CYFIXEDFRAME) -	/* window borders	  */
		GetSystemMetrics (SM_CYCAPTION);			/* title bar		  */
}

extern EXPORT_TO_CLEAN void
WinMaxScrollWindowSize (int *mx, int *my)
{
	*mx = GetSystemMetrics (SM_CXMAXIMIZED) -		// MAXIMIZED size	  
	4 * GetSystemMetrics (SM_CXSIZEFRAME) - 		// window borders	  
	4;												// 2 * client edge	  

	*my = GetSystemMetrics (SM_CYMAXIMIZED) -		// MAXIMIZED size	  
		4 * GetSystemMetrics (SM_CYSIZEFRAME) -		// window borders	  
		4 - 										// 2 * clientedge	  
		GetSystemMetrics (SM_CYCAPTION);			// title bar		  
}


extern EXPORT_TO_CLEAN CLEAN_STRING
WinGetModulePath (void)
{
	char path[MAX_PATH + 1];

	GetModuleFileName (NULL, path, MAX_PATH);

	return cleanstring (path);
}

extern EXPORT_TO_CLEAN void
WinFileModifiedDate (CLEAN_STRING name, Bool * exists, int *yy, int *mm, int *dd, int *h, int *m, int *s)
{
	char *file_name;
	HANDLE handle;
	WIN32_FIND_DATA find_data;

	file_name = cstring (name);

	handle = FindFirstFile (file_name, &find_data);

	if (handle != INVALID_HANDLE_VALUE)
	{
		SYSTEMTIME system_time;

		FindClose (handle);

		if (FileTimeToSystemTime (&find_data.ftLastWriteTime, &system_time))
		{
			*exists = TRUE;
			*yy = system_time.wYear;
			*mm = system_time.wMonth;
			*dd = system_time.wDay;
			*h = system_time.wHour;
			*m = system_time.wMinute;
			*s = system_time.wSecond;
			return;
		}
	}

	*exists = FALSE;
	*yy = 0;
	*mm = 0;
	*dd = 0;
	*h = 0;
	*m = 0;
	*s = 0;
}

extern EXPORT_TO_CLEAN Bool
WinFileExists (CLEAN_STRING name)
{
	char *file_name;
	HANDLE handle;
	WIN32_FIND_DATA find_data;

	file_name = cstring (name);

	handle = FindFirstFile (file_name, &find_data);

	if (handle != INVALID_HANDLE_VALUE)
	{
		FindClose (handle);

		return TRUE;
	}
	else
		return FALSE;
}

extern EXPORT_TO_CLEAN void
WinCallProcess (PSTR commandline,
				PSTR env,
				PSTR dir,
				PSTR in,
				PSTR out,
				PSTR err,
				OS ios,
				Bool * success,
				int *exitcode,
				OS * oos)
{
	SECURITY_ATTRIBUTES sa;
	STARTUPINFO si;
	PROCESS_INFORMATION pi;
	BOOL fsuccess;
	char *ep;
	HANDLE saveStdin, infile;
	HANDLE saveStdout, outfile;
	HANDLE saveStderr, errfile;

	rprintf ("WCP: starting...\n");

	rprintf ("success = %d, value = %d\n", success, *success);
	*success = FALSE;
	rprintf ("exitcode = %d; value = %d\n", exitcode, *exitcode);
	*exitcode = -1;
	rprintf ("oos = %d; value = %d\n", oos, *oos);
	*oos = ios;

	rprintf ("commandline = %d; value = %s\n", commandline, commandline);

	if (commandline != NULL)
		rprintf ("WCP: commandline = \"%s\"\n", commandline);
	else
		rprintf ("WCP: commandline empty\n");

	if (env != NULL)
	{
		ep = env;
		while (rstrlen (ep) != 0)
		{
			rprintf ("WCP:		   env = \"%s\"\n", ep);
			ep += rstrlen (ep) + 1;
		}
	}
	else
		rprintf ("WCP:		   env empty\n");

	if (dir != NULL)
		rprintf ("WCP: dir = \"%s\"\n", dir);
	else
		rprintf ("WCP: dir empty\n");

	if (in != NULL)
		rprintf ("WCP: in = \"%s\"\n", in);
	else
		rprintf ("WCP: in empty\n");

	if (out != NULL)
		rprintf ("WCP: out = \"%s\"\n", out);
	else
		rprintf ("WCP: out empty\n");

	if (err != NULL)
		rprintf ("WCP: err = \"%s\"\n", err);
	else
		rprintf ("WCP: err empty\n");

	sa.nLength = sizeof (SECURITY_ATTRIBUTES);
	sa.bInheritHandle = TRUE;
	sa.lpSecurityDescriptor = NULL;

	if (in != NULL)
	{
		infile = CreateFile (in,
							 GENERIC_READ,
							 FILE_SHARE_READ | FILE_SHARE_WRITE,
							 &sa,
							 OPEN_EXISTING,
							 FILE_ATTRIBUTE_NORMAL,
							 NULL
			);
		if (infile == INVALID_HANDLE_VALUE)
		{
			rprintf ("infile creation failed\n");
			return;
		}

		saveStdin = GetStdHandle (STD_INPUT_HANDLE);

		if (!SetStdHandle (STD_INPUT_HANDLE, infile))
		{
			rprintf ("could not redirect input\n");
			return;
		}
		rprintf ("redirection of input ok\n");
	}
	else
	{
		rprintf ("in == NULL\n");
		infile = NULL;
	}

	if (out != NULL)
	{
		outfile = CreateFile (out,
							  GENERIC_WRITE,
							  0,
							  &sa,
							  CREATE_ALWAYS,
							  FILE_ATTRIBUTE_NORMAL,
							  NULL
			);
		if (outfile == INVALID_HANDLE_VALUE)
		{
			rprintf ("outfile creation failed\n");
			return;
		}

		saveStdout = GetStdHandle (STD_OUTPUT_HANDLE);

		if (!SetStdHandle (STD_OUTPUT_HANDLE, outfile))
		{
			rprintf ("could not redirect output\n");
			return;
		}
		rprintf ("redirection of output ok\n");

	}
	else
	{
		rprintf ("out == NULL\n");
		outfile = NULL;
	}

	if (err != NULL)
	{
		errfile = CreateFile (err,
							  GENERIC_WRITE,
							  0,
							  &sa,
							  CREATE_ALWAYS,
							  FILE_ATTRIBUTE_NORMAL,
							  NULL
			);
		if (errfile == INVALID_HANDLE_VALUE)
		{
			rprintf ("errfile creation failed\n");
			return;
		}

		saveStderr = GetStdHandle (STD_ERROR_HANDLE);

		if (!SetStdHandle (STD_ERROR_HANDLE, errfile))
		{
			rprintf ("could not redirect errput\n");
			return;
		}
		rprintf ("redirection of errors ok\n");
	}
	else
	{
		rprintf ("err == NULL\n");
		errfile = NULL;
	}

	si.cb = sizeof (STARTUPINFO);
	si.lpReserved = NULL;
	si.lpReserved2 = NULL;
	si.cbReserved2 = 0;
	si.lpDesktop = NULL;
	si.lpTitle = NULL;
	si.dwFlags = 0;

	fsuccess =
		CreateProcess (NULL,				/* pointer to name of executable module		*/
					   commandline,			/* pointer to command line string			*/
					   NULL,				/* pointer to process security attributes	*/
					   NULL,				/* pointer to thread security attributes	*/
					   TRUE,				/* handle inheritance flag					*/
					   DETACHED_PROCESS,	/* creation flags							*/
					   env,					/* pointer to new environment block			*/
					   dir,					/* pointer to current directory name		*/
					   &si,					/* pointer to STARTUPINFO					*/
					   &pi					/* pointer to PROCESS_INFORMATION			*/
		);
	if (fsuccess)
	{
		rprintf ("WCP: success\n");
		WaitForSingleObject (pi.hProcess, INFINITE);
		GetExitCodeProcess (pi.hProcess, (unsigned long *) exitcode);
		rprintf ("WCP: exitcode = %d\n", *exitcode);
		*success = TRUE;
	}
	else
	{
		rprintf ("WCP: failure %d\n", (int) GetLastError ());
		*success = FALSE;
		*exitcode = -1;
	}

	if (infile != NULL)
	{
		if (SetStdHandle (STD_INPUT_HANDLE, saveStdin))
			rprintf ("resetting stdin ok\n");
		else
			rprintf ("resetting stdin failed\n");

		if (CloseHandle (infile))
			rprintf ("closing infile ok\n");
		else
			rprintf ("closing infile failed\n");
	}
	else
		rprintf ("no need to close and reset input\n");

	if (outfile != NULL)
	{
		if (SetStdHandle (STD_OUTPUT_HANDLE, saveStdout))
			rprintf ("resetting stdout ok\n");
		else
			rprintf ("resetting stdout failed\n");

		if (CloseHandle (outfile))
			rprintf ("closing outfile ok\n");
		else
			rprintf ("closing outfile failed\n");
	}
	else
		rprintf ("no need to close and reset output\n");

	if (errfile != NULL)
	{
		if (SetStdHandle (STD_ERROR_HANDLE, saveStderr))
			rprintf ("resetting stderr ok\n");
		else
			rprintf ("resetting stderr failed\n");

		if (CloseHandle (errfile))
			rprintf ("closing errfile ok\n");
		else
			rprintf ("closing errfile failed\n");
	}
	else
		rprintf ("no need to close and reset errput\n");

	rprintf ("WCP: returning\n");
	/* *oos = ios; */
}

extern EXPORT_TO_CLEAN void
WinLaunchApp (CLEAN_STRING commandline, BOOL console, OS ios, Bool * success, OS * oos)
{
	STARTUPINFO si;
	PROCESS_INFORMATION pi;
	BOOL fsuccess;
	char path[_MAX_PATH];
	char *cl, *thepath;
	int i;
	DWORD error;

	rprintf ("WLA: starting...\n");

	*success = FALSE;
	*oos = ios;
	si.lpTitle = NULL;

	rprintf ("WLA: step 2.\n");

	cl = cstring (commandline);
	lstrcpy (path, cl);

	for (i = lstrlen (path); path[i] != '\\' && i >= 0; i--)
		path[i] = 0;

	if (i == 0)
		thepath = NULL;
	else
	{	/* path[i] = '\"'; */
		thepath = path + 1;
	}

	rprintf ("WLA: step 2a: directory = <%s>\n", thepath);

	rprintf ("WLA: step 3: filling in si.\n");

	si.cb = sizeof (STARTUPINFO);
	si.lpReserved = NULL;
	si.lpReserved2 = NULL;
	si.cbReserved2 = 0;
	si.lpDesktop = NULL;
	si.dwFlags = 0;
	si.lpTitle = NULL;

	rprintf ("WLA: step 4: calling process \"%s\".\n", cl);

	fsuccess =
		CreateProcess (NULL,	/* pointer to name of executable module		*/
					   cl,		/* pointer to command line string			*/
					   NULL,	/* pointer to process security attributes	*/
					   NULL,	/* pointer to thread security attributes	*/
					   TRUE,	/* handle inheritance flag					*/
					   0,		/* creation flags							*/
					   NULL,	/* pointer to new environment block			*/
					   thepath, /* pointer to current directory name		*/
					   &si, 	/* pointer to STARTUPINFO					*/
					   &pi		/* pointer to PROCESS_INFORMATION			*/
		);
	error = GetLastError ();
	if (fsuccess)
	{
		rprintf ("WLA: success\n");
	}
	else
	{
		rprintf ("WLA: failure %d\n", error);
	}

	rprintf ("WLA: step 5: returning\n");
	*success = fsuccess;
	*oos = ios;
	rprintf ("WLA: done...\n");
}

/*	New version of WinLaunchApp. In addition to the commandline, it gets a directory argument.
*/
extern EXPORT_TO_CLEAN void
WinLaunchApp2 (CLEAN_STRING commandline, CLEAN_STRING pathname, BOOL console, OS ios, Bool * success, OS * oos)
{
	STARTUPINFO si;
	PROCESS_INFORMATION pi;
	BOOL fsuccess;
	char commandline2[_MAX_PATH];
	char *cl, *thepath;
	DWORD error;

	rprintf ("WLA2: starting...\n");

	*success = FALSE;
	*oos = ios;
	si.lpTitle = NULL;

	rprintf ("WLA2: step 2.\n");

	cl = cstring (commandline);
	lstrcpy (commandline2, cl);
	thepath = cstring (pathname);

	rprintf ("WLA2: step 3: filling in si.\n");

	si.cb = sizeof (STARTUPINFO);
	si.lpReserved = NULL;
	si.lpReserved2 = NULL;
	si.cbReserved2 = 0;
	si.lpDesktop = NULL;
	si.dwFlags = 0;
	si.lpTitle = NULL;

	rprintf ("WLA2: step 4: calling process \"%s\".\n", cl);

	fsuccess =
		CreateProcess (NULL,		/* pointer to name of executable module		*/
					   commandline2,/* pointer to command line string			*/
					   NULL,		/* pointer to process security attributes	*/
					   NULL,		/* pointer to thread security attributes	*/
					   TRUE,		/* handle inheritance flag					*/
					   0,			/* creation flags							*/
					   NULL,		/* pointer to new environment block			*/
					   thepath,		/* pointer to current directory name		*/
					   &si,			/* pointer to STARTUPINFO					*/
					   &pi			/* pointer to PROCESS_INFORMATION			*/
		);
	error = GetLastError ();
	if (fsuccess)
	{
		rprintf ("WLA2: success\n");
	}
	else
	{
		rprintf ("WLA2: failure %d\n", error);
	}

	rprintf ("WLA2: step 5: returning\n");
	*success = fsuccess;
	*oos = ios;
	rprintf ("WLA2: done...\n");
}
/*	End of addition. */

extern EXPORT_TO_CLEAN void
WinGetTickCount (OS ios, int *tickCount, OS * oos)
{
	*tickCount = GetTickCount ();
	*oos = ios;
}


char * toCstring (CLEAN_STRING s)
{
	char *cstr = (char *) NULL;

	cstr = (char *) rmalloc ((s->length) + 1);
	rsncopy (cstr, s->characters, s->length);
	cstr[s->length] = 0;
	return cstr;
}

extern EXPORT_TO_CLEAN void
WinPlaySound (CLEAN_STRING clfilename, OS ios, Bool * ook, OS * oos)
{	char * cfilename;

	cfilename = toCstring (clfilename);
	if (PlaySound (cfilename, NULL, SND_FILENAME | SND_SYNC))
	{
		*ook = TRUE;
	}
	else
	{
		*ook = FALSE;
	}
	rfree (cfilename);

	*oos = ios;
}
