#ifndef _CPRINTER
#define _CPRINTER

#include <Clean.h>

char * strtokMW(char **str, const char ch1, const char ch2);
int startPage(int hdc);
int endPage  (int hdc);
int startDoc (int hdc);
			// returns err code: >0:no error, <=0: user cancelled file dialog
void endDoc   (int hdc);
void deleteDC(int hdc);
int wasCanceled();
void printSetup(int calledFromCleanThread, int devmodeSize,
			   char *devmode, char *device, char *driver, char *output,
			   int *ok, PRINTDLG **pdPtr);
void getDC( int doDialog, int emulateScreen, int calledFromCleanThread, int devmodeLength,
			char *devmode,char *device,char *driver,char *output,
			int *err,
			int *first, int *last, int *copies,
			PRINTDLG	**ppPrintDlg,
			int *deviceContext
	 		);
					// err code: -1:no error, others: non fatal error
void get_printSetup_with_PRINTDLG(PRINTDLG *pd, CleanString *o_devmode,
								CleanString *o_device, CleanString *o_driver, CleanString *o_output);
void getCaps( HDC hdcPrint, int unq,
				int *maxX, int *maxY,
				int *leftPaper, int *topPaper,
				int *rightPaper, int *bottomPaper,
				int *unqReturn
			);

BOOL CALLBACK AbortProc (HDC hdcPrn, int iCode);
BOOL CALLBACK PrintDlgProc (HWND hDlg, UINT msg, WPARAM wParam, LPARAM lParam);
HWND CreateCancelDialog();

#endif