#ifndef _CPRINTER
#define _CPRINTER

void startPage(int hdc, int os, int *ok, int *hdcReturn, int *osReturn);
void endPage  (int hdc, int os, int *ok, int *hdcReturn, int *osReturn);
void startDoc (int hdc, int os, int *err, int *hdcReturn, int *osReturn);
			// err code: >0:no error, <=0: user cancelled file dialog
void endDoc   (int hdc, int os, int *hdcReturn, int *osReturn);
int deleteDC(int hdc, int os);
int wasCanceled();
void getDC( int doDialog, int emulateScreen, int calledFromCleanThread, int unq,
					int *err,
					int *first, int *last,
					int *copies,
					int *deviceContext, int *unqReturn
					);
					// err code: -1:no error, others: non fatal error
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