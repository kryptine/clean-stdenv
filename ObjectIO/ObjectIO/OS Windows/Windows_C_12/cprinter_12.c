#include <windows.h>
#include <limits.h>
#include "cpicture_12.h"
#include "util_12.h"
#include "cCrossCall_12.h"
#include "cprinter_12.h"

BOOL   bUserAbort ;
HWND   hDlgPrint ;
HWND	hwndButton,hwndText;
int		semaphor=0;

extern HWND   ghMainWindow;
extern HINSTANCE ghInst;

void startPage(int hdc, int os, int *ok, int *hdcReturn, int *osReturn)
{
	*osReturn = os;
	*hdcReturn = hdc;
	*ok = StartPage((HDC) hdc) > 0;
}

void endPage(int hdc, int os, int *ok, int *hdcReturn, int *osReturn)
{
	*osReturn = os;
	*hdcReturn = hdc;
	*ok = EndPage((HDC) hdc) > 0;
}

void startDoc(int hdc, int os, int *err, int *hdcReturn, int *osReturn)
			// err code: >0:no error, <=0: user cancelled file dialog
{
	static DOCINFO docInfo = { sizeof (DOCINFO), "Clean", NULL } ;

	*osReturn = os;
	*hdcReturn = hdc;
	
	bUserAbort = FALSE ;
    
	*err = StartDoc((HDC) hdc, &docInfo);
}

void endDoc(int hdc, int os, int *hdcReturn, int *osReturn)
{
	*osReturn = os;
	*hdcReturn = hdc;
	if (bUserAbort)
		AbortDoc((HDC) hdc);
	  else
		EndDoc((HDC) hdc);
}

int deleteDC(int hdc, int os)
{
	DeleteDC((HDC) hdc);
	return os;
}

int wasCanceled()
{
	return bUserAbort;
}

/* getDC opens the print job dialog and 
 * lets the user change various settings or gets the default printer
 */

char * strtokMW(char **str, const char ch1, const char ch2)
/* 	nearly like the standard strtok function. This function splits a null terminated
	string into two parts. From the beginning of the string it searches for the next
	occurence of ch1 or ch2, replaces that character with '\0' and gives back the
	first part. The first parameter will be altered, so that it points now to the
	string after that '\0' character.
*/   
{
	char *start,*count;
	int terminate=0;
	start=*str;
	count=*str;
	do 
	  {	if (*count==ch1 || *count==ch2 || *count=='\0')
			{ terminate=1; }
		  else 
		  	count = CharNext(count);	// I hope this works well in the far east
	  }									// (multibyte character codes)
	  while (!terminate);
	if (*count!='\0')
	  { *count='\0';
	    count++;
	  };
	*str=count;
	return start;
}

UINT APIENTRY DialogToFrontHook(HWND hdl, UINT msg, WPARAM wParam, LPARAM lParam)
// This function hooks the Print dialog. It's purpose is to set the dialog in the
// foreground.
{
     if (msg==WM_INITDIALOG)
		{ SetForegroundWindow(hdl);
		};
	 return FALSE;
}

void getDC( int doDialog, int emulateScreen, int calledFromCleanThread, int unq,
					int *err,
					int *first, int *last,
					int *copies,
					int *deviceContext, int *unqReturn
					)
					// err code: -1:no error, others: non fatal error
{
	static PRINTDLG pd;
	HDC hdcPrint;
	
	*unqReturn = unq;
	*err = -1;

	if (doDialog)
	  {	// Set up print dialog record

		pd.lStructSize = sizeof(PRINTDLG);
		pd.hwndOwner = calledFromCleanThread ? NULL : ghMainWindow;	// (NULL = desktop)
			// the handle must belong to the active thread, otherwise PrintDlg will crash
			// When this function is called from the Clean thread, ghMainWindow will not
			// belong to the active thread.
		pd.hDevMode = NULL;
		pd.hDevNames = NULL;
		pd.hDC = NULL;
		pd.Flags = PD_ALLPAGES | PD_COLLATE | PD_RETURNDC | PD_NOSELECTION 
				 | PD_ENABLEPRINTHOOK;
		      // hide some options from print dialog
		pd.nFromPage = 1;
		pd.nToPage = 1; 
		pd.nMinPage = 1;
		pd.nMaxPage = USHRT_MAX;
		pd.nCopies = 1;
		pd.hInstance = NULL;
		pd.lCustData = 0L;
		pd.lpfnPrintHook = DialogToFrontHook;
		pd.lpfnSetupHook = NULL;
		pd.lpPrintTemplateName = NULL;
		pd.lpSetupTemplateName = NULL;
		pd.hPrintTemplate = NULL;
		pd.hSetupTemplate = NULL;

		// Open print dialog

		if (!PrintDlg(&pd))
		  {
			*err = CommDlgExtendedError();	// will return 0 iff user canceled
			return;							// otherwise a positive value
		  }
	
		if (pd.Flags & PD_PAGENUMS)
			{ *first	= pd.nFromPage;
			  *last		= pd.nToPage;
			}
		  else
			{ *first	= 1;
			  *last		= 9999;
			};
		*copies			= pd.nCopies;
		hdcPrint		= pd.hDC;

	  }

	else // get dc for default printer
      // This method searches in the WIN.INI file for th default printer name.
	  
	  { char	szPrinterSpace[80];
		char	*szPrinter = szPrinterSpace;
		char	*szDevice, *szDriver, *szOutput;
		int		success;
		GetProfileString("windows", "device", ",,,", szPrinter, 80);
		szDevice = strtokMW(&szPrinter,',',',');
		szDriver = strtokMW(&szPrinter,',',' ');
		szOutput = strtokMW(&szPrinter,',',' ');
		success = (*szDevice!='\0') && (*szDriver!='\0') && (*szOutput!='\0');
		if (success)
		  hdcPrint = CreateDC(szDriver, szDevice, szOutput, NULL);
		if (success==0 || hdcPrint==NULL)
		  { *err = 0;	// non fatal error, iff e.g. no printer driver is installed
	 	    return;
		  };
		*first	= 1;
		*last	= 9999;
		*copies	= 1;
	  };

	if (emulateScreen)
		{	int pXdpi,pYdpi,sXdpi,sYdpi;
			pXdpi = GetDeviceCaps(hdcPrint, LOGPIXELSX);
			pYdpi = GetDeviceCaps(hdcPrint, LOGPIXELSY);
			sXdpi = WinGetHorzResolution();
			sYdpi = WinGetVertResolution();
			SetMapMode(hdcPrint, MM_ISOTROPIC);
			SetWindowExtEx  (hdcPrint,sXdpi, sYdpi, NULL);
			SetViewportExtEx(hdcPrint,pXdpi, pYdpi, NULL);
		};
	
	*deviceContext	= (int) hdcPrint;
}




void getCaps( HDC hdcPrint, int unq,
				int *maxX, int *maxY,
				int *leftPaper, int *topPaper,
				int *rightPaper, int *bottomPaper,
				int *unqReturn
			)
{
	// Get device capabilities
	int horPaperPixels, verPaperPixels,
		scNX, scNY, scDX, scDY;
	
	*unqReturn = unq;
	
	if (GetMapMode(hdcPrint)==MM_ISOTROPIC)		// for emulation of the screen resolution
		{	scNX = WinGetHorzResolution();		// all the deviceCaps will be scaled
			scNY = WinGetVertResolution();
			scDX = GetDeviceCaps(hdcPrint, LOGPIXELSX);
			scDY = GetDeviceCaps(hdcPrint, LOGPIXELSY);
		}
	  else
		{	scNX = 1; scNY = 1; scDX = 1; scDY = 1;	};
	
	horPaperPixels	= (GetDeviceCaps(hdcPrint, PHYSICALWIDTH)*scNX)/scDX;
	verPaperPixels	= (GetDeviceCaps(hdcPrint, PHYSICALHEIGHT)*scNY)/scDY;

	*maxX			= (GetDeviceCaps(hdcPrint, HORZRES)*scNX)/scDX;
	*maxY			= (GetDeviceCaps(hdcPrint, VERTRES)*scNY)/scDY;

    *leftPaper		= (-GetDeviceCaps(hdcPrint, PHYSICALOFFSETX)*scNX)/scDX;
	*topPaper		= (-GetDeviceCaps(hdcPrint, PHYSICALOFFSETY)*scNY)/scDY;
	*rightPaper		= horPaperPixels - *leftPaper;
	*bottomPaper	= verPaperPixels - *topPaper;

}

BOOL CALLBACK PrintDlgProc (HWND hDlg, UINT msg, WPARAM wParam, LPARAM lParam)
     {
     switch (msg)
          {
          case WM_INITDIALOG :
               EnableMenuItem (GetSystemMenu (hDlg, FALSE), SC_CLOSE,
                                                            MF_GRAYED) ;
               return TRUE ;
          case WM_COMMAND :
               bUserAbort = TRUE ;
               EnableWindow (ghMainWindow, TRUE) ;
               DestroyWindow (hDlg) ;
               hDlgPrint = 0 ;
               return TRUE ;
          }
     return FALSE;
	 }

BOOL CALLBACK AbortProc (HDC hdcPrn, int iCode)
     {
     MSG   msg ;

     while (!bUserAbort && PeekMessage (&msg, NULL, 0, 0, PM_REMOVE))
          {
          if (!hDlgPrint || !IsDialogMessage (hDlgPrint, &msg))
               {
               TranslateMessage (&msg) ;
               DispatchMessage (&msg) ;
               }
          }
     return !bUserAbort ;
     }


#define DIALOG_WIDTH 100
#define DIALOG_HEIGHT 60
	// in dialog units

HWND CreateCancelDialog()
{
	HWND hwndButton,dlgHdl;

	WORD *p, *pdlgtemplate,baseunitX,baseunitY;
	int nchar;
	int scrnWidth,scrnHeight;
	int buttonX, buttonY, buttonWidth, buttonHeight;
	int textX, textY, textWidth, textHeight;
	DWORD lStyle,baseunits;
	HDC screen;
	LOGFONT lf;

	/* allocate some memory to play with  */
	pdlgtemplate = p = (PWORD) rmalloc (1000);

	screen = CreateDC ("DISPLAY", NULL, NULL, NULL);
	scrnWidth  = GetDeviceCaps (screen, HORZRES);
	scrnHeight = GetDeviceCaps (screen, VERTRES);
	DeleteDC (screen);
	baseunits = GetDialogBaseUnits();

	/* start to fill in the dlgtemplate information.  addressing by WORDs */
	lStyle = WS_CAPTION | DS_MODALFRAME | WS_SYSMENU;

	baseunitX=LOWORD(baseunits);
	baseunitY=HIWORD(baseunits);

	*p++ = LOWORD (lStyle);
	*p++ = HIWORD (lStyle);
	*p++ = 0;		/* LOWORD (lExtendedStyle) */
	*p++ = 0;		/* HIWORD (lExtendedStyle) */
	*p++ = 0;		/* NumberOfItems */
	*p++ = ((scrnWidth*4)/3)/baseunitX;		// x 
	*p++ = ((scrnHeight*8)/3)/baseunitY;	// y
	*p++ = DIALOG_WIDTH;	/* cx */
	*p++ = DIALOG_HEIGHT;	/* cy */
	*p++ = 0;		/* Menu */
	*p++ = 0;		/* Class */

	/* copy the title of the dialog */
	nchar = nCopyAnsiToWideChar (p, (char *) "Printing in Progress");
	p += nchar;
	
	dlgHdl = CreateDialogIndirectParam (ghInst, (LPDLGTEMPLATE) pdlgtemplate, ghMainWindow,
										(DLGPROC) PrintDlgProc, (LPARAM) 0);

	LocalFree (LocalHandle (pdlgtemplate));

	// Add a text field
	textWidth = 19*baseunitX;
	textHeight = baseunitY;
	textX =    (((DIALOG_WIDTH*baseunitX)/4) - textWidth)
		       / 2;
	textY =    (((DIALOG_HEIGHT*baseunitY)/8) - textHeight)
			   / 4; 
	hwndText = CreateWindow ("static", "0 pages printed",WS_VISIBLE | WS_CHILD | SS_CENTER, 
									textX, textY, textWidth, textHeight,
									dlgHdl, (HMENU) 0, ghInst, 0);

	
	// Add a Cancel button:
	buttonWidth = 10*baseunitX;
	buttonHeight = (3*baseunitY)/2;
	buttonX =    (((DIALOG_WIDTH*baseunitX)/4) - buttonWidth)
		       / 2;
	buttonY =  (3 *  (((DIALOG_HEIGHT*baseunitY)/8) - buttonHeight))
			   / 5; 
	hwndButton = CreateWindow ("button", "Cancel", WS_VISIBLE | WS_CHILD | BS_PUSHBUTTON,
									buttonX, buttonY, buttonWidth, buttonHeight,
									dlgHdl, (HMENU) 0, ghInst, 0);
	SetLogFontData (&lf,"MS Sans Serif",0,8);
	SendMessage(hwndButton,WM_SETFONT,(WPARAM)CreateFontIndirect (&lf),MAKELPARAM (TRUE,0));
	SendMessage(hwndText,WM_SETFONT,(WPARAM)CreateFontIndirect (&lf),MAKELPARAM (TRUE,0));

	ShowWindow (dlgHdl,SW_SHOWNORMAL);

	return dlgHdl; 
}


int addSemaphor(int add)
{
	int old=semaphor;
	semaphor+=add;
	return old;
}