/* C module intrface */

#define MaxRand									32767
#define iWhitePattern							4
#define iLtGreyPattern							3
#define iGreyPattern							2
#define iDkGreyPattern							1
#define iBlackPattern							0
#define iModeNotBic								7
#define iModeNotXor								6
#define iModeNotOr								5
#define iModeNotCopy							4
#define iModeBic								3
#define iModeXor								2
#define iModeOr									1
#define iModeCopy								0
#define iStrikeOut								8
#define iUnderline								4
#define iItalic									2
#define iBold									1
#define WinHelpKey								5
#define WinEscapeKey							27
#define WinReturnKey							13
#define WinTabKey								9
#define WinDelKey								127
#define WinBackSpKey							8
#define WinEndKey								4
#define WinBeginKey								1
#define WinPgDownKey							12
#define WinPgUpKey								11
#define WinRightKey								29
#define WinLeftKey								28
#define WinDownKey								31
#define WinUpKey								30
#define WinF1Key								1001
#define WinF2Key								1002
#define WinF3Key								1003
#define WinF4Key								1004
#define WinF5Key								1005
#define WinF6Key								1006
#define WinF7Key								1007
#define WinF8Key								1008
#define WinF9Key								1009
#define WinF10Key								1010
#define WinF11Key								1011
#define WinF12Key								1012
#define CTRLBIT									4
#define ALTBIT									2
#define SHIFTBIT								1
#define KEYREPEAT								4
#define KEYUP									2
#define KEYDOWN									1
#define BUTTONSTILLUP							0			/* PA: new constant for mouse handling. */
#define BUTTONUP								50
#define BUTTONSTILLDOWN							40
#define BUTTONTRIPLEDOWN						3
#define BUTTONDOUBLEDOWN						2
#define BUTTONDOWN								1
#define CURSHIDDEN								6
#define CURSARROW								5
#define CURSFATCROSS							4
#define CURSCROSS								3
#define CURSIBEAM								2
#define CURSBUSY								1
#define EDITISMULTILINE							1			/* PA: flag value: edit control is multi-line. */
#define EDITISKEYSENSITIVE						2			/* PA: flag value: edit control sends keyboard events to Clean. */

/* PA: new constants that are passed when creating (custom)button controls.
*/
#define ISNORMALBUTTON							0			/* The button is a normal button.   */
#define ISOKBUTTON								1			/* The button is the OK button.     */
#define ISCANCELBUTTON							2			/* The button is the CANCEL button. */

/* Mike... */
#define CcRqUSERGAMEEVENT						1905        /* send user event to other objects */
#define CcRqCREATEGAMEOBJECT					1904        /* create a new game object */
#define CcRqPLAYSOUNDSAMPLE						1903        /* initialize sound sample */
#define CcRqRUNGAME								1901		/* run the game engine */
#define CcRqCREATEGAMEWINDOW					1900		/* create a game window */
/* ...Mike */

// MW...
#define CcRqDO_PRINT_SETUP						1828
#define CcRqDO_HTML_HELP						1827
#define CcRqGET_PRINTER_DC						1824
#define CcRqDISPATCH_MESSAGES_WHILE_PRINTING	1823
#define CcRqENDDOC								1822
#define CcRqSTARTDOC							1821
// ... MW
#define CcRqDESTROYMDIDOCWINDOW					1817		/* PA: added to destroy MDI document window */
#define CcRqCREATESDIDOCWINDOW					1816		/* PA: added to create SDI document window */
#define CcRqCREATEMDIDOCWINDOW					1815		/* PA: added to create MDI document window */
#define CcRqCREATEMDIFRAMEWINDOW				1814		/* PA: added to create MDI frame window */
#define CcRqCREATESDIFRAMEWINDOW				1813		/* PA: added to create SDI frame window */
#define CcRqCLIPBOARDHASTEXT					1812
#define CcRqGETCLIPBOARDTEXT					1811
#define CcRqSETCLIPBOARDTEXT					1810
#define CcRqGETCLIPBOARDCOUNT					1809		/* PA: added to retrieve clipboard count. */
#define CcRqDIRECTORYDIALOG						1802		/* PA: added to create directory selector dialog. */
#define CcRqFILESAVEDIALOG						1801
#define CcRqFILEOPENDIALOG						1800
#define CcRqUPDATEDESKTOP						1790		/* PA: added to force refresh of desktop. */
#define CcRqSHOWCONTROL							1755		/* PA: added */
#define CcRqSELECTPOPUPITEM						1754
#define CcRqENABLEPOPUPITEM						1753
#define CcRqADDTOPOPUP							1752
#define CcRqSETITEMCHECK						1751
#define CcRqENABLECONTROL						1750
#define CcRqCREATECOMPOUND						1729		/* PA: added */
#define CcRqCREATESCROLLBAR						1728		/* PA: added */
#define CcRqCREATECUSTOM						1727
#define CcRqCREATEICONBUT						1726
#define CcRqCREATEPOPUP							1725
#define CcRqCREATECHECKBOX						1724
#define CcRqCREATERADIOBUT						1723
#define CcRqCREATEEDITTXT						1722
#define CcRqCREATESTATICTXT						1721
#define CcRqCREATEBUTTON						1720
#define CcRqCREATEMODALDIALOG					1701		/* PA: added to create modal dialog. */
#define CcRqCREATEDIALOG						1700
//#define CcRqGETBLINKTIME						1670	PA: has become obsolete because implemented as direct C call
//#define CcRqWAIT								1660	PA: has become obsolete because implemented as direct C call
//#define CcRqGETCURDATE							1651	PA: has become obsolete because implemented as direct C call
//#define CcRqGETCURTIME							1650	PA: has become obsolete because implemented as direct C call
#define CcRqCREATETOOLBARSEPARATOR				1603		/* PA: added to create a toolbar separator item. */
#define CcRqCREATETOOLBARITEM					1602		/* PA: added to create a toolbar bitmap item. */
#define CcRqCREATEMDITOOLBAR					1601		/* PA: added to create a toolbar for a MDI process. */
#define CcRqCREATESDITOOLBAR					1600		/* PA: added to create a toolbar. */
#define CcCbFONTSIZE							1530
#define CcCbFONTNAME							1520
#define CcRqGETFONTSIZES						1510
#define CcRqGETFONTNAMES						1500

#define CcRqSETCLIENTSIZE						1438		/* PA: added to set client size. */
#define CcRqDELCONTROLTIP						1437		/* PA: added to remove controls from tooltip areas. */
#define CcRqADDCONTROLTIP						1436		/* PA: added to add controls to tooltip areas. */
#define CcRqGETWINDOWSIZE						1435		/* PA: new identifier for cross-calls */
#define CcRqRESTACKWINDOW						1434		/* PA: new identifier for cross-calls */
#define CcRqSHOWWINDOW							1433		/* PA: new identifier for cross-calls */
#define CcRqSETWINDOWSIZE						1432		/* PA: new identifier for cross-calls */
#define CcRqSETSELECTWINDOW						1431		/* PA: new identifier for cross-calls */
#define CcRqSETWINDOWPOS						1430		/* PA: new identifier for cross-calls */
//#define CcRqINVALIDATERECT					1429		PA: has become obsolete because implemented as direct C call
#define CcRqSETEDITSELECTION					1428		/* PA: new identifier for cross-calls */
#define CcRqSETSCROLLSIZE						1427		/* PA: new identifier for cross-calls */
#define CcRqSETSCROLLPOS						1426		/* PA: new identifier for cross-calls */
#define CcRqSETSCROLLRANGE						1425		/* PA: new identifier for cross-calls */
#define CcRqRESETCURSOR							1424
#define CcRqSETGLOBALCURSOR						1423
#define CcRqOBSCURECURSOR						1422
#define CcRqCHANGEWINDOWCURSOR					1421
#define CcRqACTIVATEWINDOW						1420		/* PA: added for activating window. */
#define CcRqACTIVATECONTROL						1419		/* PA: added for activating controls. */
#define CcRqGETWINDOWPOS						1416
#define CcRqGETCLIENTSIZE						1415
#define CcRqUPDATEWINDOWRECT					1412		/* PA: added for updating rect part of a window/control. */
#define CcRqGETWINDOWTEXT						1411
#define CcRqSETWINDOWTITLE						1410
//#define CcRqVALIDATERGN						1409		PA: has become obsolete because implemented as direct C call
//#define CcRqVALIDATERECT						1408		PA: has become obsolete because implemented as direct C call
//#define CcRqINVALIDATEWINDOW					1407		PA: has become obsolete because implemented as direct C call
//#define CcRqRELEASEDC							1406		PA: has become obsolete because implemented as direct C call
//#define CcRqGETDC								1405		PA: has become obsolete because implemented as direct C call
#define CcRqFAKEPAINT							1405		/* PA: added combination of BeginPaint; EndPaint; InvalidateRect; */
#define CcRqENDPAINT							1404
#define CcRqBEGINPAINT							1403
#define CcRqDESTROYWINDOW						1402
#define CcRqDESTROYMODALDIALOG					1401		/* PA: added to destroy modal dialog. */
#define CcRqDRAWMBAR							1265
#define CcRqTRACKPOPMENU						1256		/* PA: added for handling pop up menu. */
#define CcRqCREATEPOPMENU						1255
#define CcRqINSERTSEPARATOR						1245
#define CcRqMENUENABLE							1235
#define CcRqMODIFYMENU							1230
#define CcRqINSERTMENU							1226		/* PA: new constant for inserting a menu in the menu bar. */
#define CcRqITEMENABLE							1220
#define CcRqREMOVEMENUSHORTKEY					1217		/* PA: new constant for removing a shortkey of a menu item. */
#define CcRqADDMENUSHORTKEY						1216		/* PA: new constant for adding a shortkey of a menu item. */
#define CcRqMODIFYMENUITEM						1215
#define CcRqDESTROYMENU							1214		/* PA: new constant for destroying a menu 'physically' */
#define CcRqDELETEMENU							1213		/* PA: new constant for deleting a menu logically */
#define CcRqREMOVEMENUITEM						1212
#define CcRqCHECKMENUITEM						1210
#define CcRqINSERTMENUITEM						1205
#define CcRqDOMESSAGE							1100
//#define CcRqBEEP								1001		PA: has become obsolete because implemented as direct C call


/* Mike... */
// Mike: Convention for OS to Clean requests: 500-599 //
#define CcWmCHECKQUIT							513         /* Mike: check user's quit function */
#define CcWmUSEREVENT							512         /* Mike: user defined event */
#define CcWmSTATISTICS							511         /* Mike: request for statistics */
#define CcWmOBJECTKEYUP							510         /* Mike: key released */
#define CcWmOBJECTKEYDOWN						509         /* Mike: key pressed for object */
#define CcWmOBJECTTIMER							508			/* Mike: framecounter reached 0 */
#define CcWmANIMATION							507			/* Mike: animation sequence ended */
#define CcWmCOLLISION							506         /* Mike: collision of two objects */
#define CcWmTOUCHBOUND							505			/* Mike: object touches bound */
#define CcWmOBJECTDONE							504			/* Mike: object is destroyed */
#define CcWmMOVEOBJECT							503			/* Mike: move object */
#define CcWmINITOBJECT							502			/* Mike: initialize new object */
#define CcWmSCROLL								501			/* Mike: calculate layer positions */
#define CcWmGAMEKEYBOARD						500			/* Mike: keyboard input for game */
/* ...Mike */

// MW ...
#define CcWmINETEVENT							140		
// ... MW
#define CcWmSPECIALBUTTON						133			/* PA: new constant for info about OK/CANCEL button selected. */
#define CcWmPROCESSDROPFILES					132			/* PA: new constant for requesting opening of files. */
#define CcWmGETTOOLBARTIPTEXT					131			/* PA: new constant for getting tooltip text. */
#define CcWmSETFOCUS							130			/* PA: new constant for notifying obtaining keyboard input focus. */
#define CcWmKILLFOCUS							129			/* PA: new constant for notifying loss of keyboard input focus. */
#define CcWmPROCESSCLOSE						127			/* PA: new constant for requesting closing of process. */
#define	CcWmDRAWCLIPBOARD						126			/* PA: new constant for clipboard handling. Copied from Ronny. */
#define CcWmGETSCROLLBARINFO					125			/* PA: new constant for info about scrollbars. */
#define CcWmSCROLLBARACTION						124			/* PA: new constant for scrollbar handling. */
#define CcWmDDEEXECUTE							123
#define CcWmIDLEDIALOG							121			/* PA: old constant reused for initialising modal dialogues. */
#define CcWmDRAWCONTROL							120
#define CcWmCOMBOSELECT							119
#define CcWmBUTTONCLICKED						118
#define CcWmINITDIALOG							117
#define CcWmIDLETIMER							116
#define CcWmTIMER								115
#define CcWmNEWVTHUMB							114
#define CcWmNEWHTHUMB							113
#define CcWmGETVSCROLLVAL						112
#define CcWmGETHSCROLLVAL						111
#define CcWmSIZE								110			/* PA: old constant reused for resize information. */
#define CcWmMOUSE								109
#define CcWmKEYBOARD							108
#define CcWmDEACTIVATE							107
#define CcWmACTIVATE							106
#define CcWmCLOSE								105
#define CcWmCOMMAND								103
#define CcWmCHAR								102
#define CcWmCREATE								101
#define CcWmPAINT								100
#define CcWmNOTIFY								78			/* PA: new constant for notify events. */
#define CcWINMESSmax							999
#define CcWINMESSmin							100
#define CcRETURN6								16
#define CcRETURN5								15
#define CcRETURN4								14
#define CcRETURN3								13
#define CcRETURN2								12
#define CcRETURN1								11
#define CcRETURN0								10
#define CcRETURNmax								19
#define CcRETURNmin								10
#define CcWASQUIT								1

// MW: new convention: messages that are passed within the OS thread begin with PM
// They can be in range WM_USER (currently 0x0400) to 0x7FFF. 

#define	PM_SOCKET_EVENT							0x0405
#define	PM_DNS_EVENT							0x0406

# define EXPORT_TO_CLEAN	/* __declspec(dllexport) __stdcall */

extern EXPORT_TO_CLEAN void WinLaunchApp (CLEAN_STRING,Bool,OS,Bool*,OS*);
/* PA: WinLaunchApp2 added. Identical to WinLaunchApp, except that second CLEAN_STRING argument
       identifies application path; and first argument is the command line. 
*/
extern EXPORT_TO_CLEAN void WinLaunchApp2 (CLEAN_STRING,CLEAN_STRING,Bool,OS,Bool*,OS*);

extern EXPORT_TO_CLEAN  void WinCallProcess (PSTR,PSTR,PSTR,PSTR,PSTR,PSTR,OS,Bool*,int*,OS*);

extern EXPORT_TO_CLEAN  CLEAN_STRING WinGetModulePath (void);

extern EXPORT_TO_CLEAN  void WinFileModifiedDate (CLEAN_STRING,Bool*,int*,int*,int*,int*,int*,int*);

extern EXPORT_TO_CLEAN  Bool WinFileExists (CLEAN_STRING);

extern EXPORT_TO_CLEAN  int Rand (void);

extern EXPORT_TO_CLEAN  OS ConsolePrint (CLEAN_STRING,OS);

extern EXPORT_TO_CLEAN  OS WinReleaseCString (PSTR,OS);

extern EXPORT_TO_CLEAN  void WinGetCStringAndFree (PSTR,OS,CLEAN_STRING*,OS*);

extern EXPORT_TO_CLEAN  void WinGetCString (PSTR,OS,CLEAN_STRING*,OS*);

extern EXPORT_TO_CLEAN  void WinMakeCString (CLEAN_STRING,OS,PSTR*,OS*);

extern EXPORT_TO_CLEAN  PSTR WinGetAppPath (void);

extern EXPORT_TO_CLEAN  OS WinSetDoubleDownDist (int,OS);

extern EXPORT_TO_CLEAN  void WinKickOsThread (int,int,int,int,int,int,int,OS,int*,int*,int*,int*,int*,int*,int*,OS*);

extern EXPORT_TO_CLEAN  OS WinKillOsThread (OS);

extern EXPORT_TO_CLEAN  OS WinStartOsThread (OS);

extern EXPORT_TO_CLEAN  Bool WinCloseOs (OS);

extern EXPORT_TO_CLEAN  void WinInitOs (Bool*,OS*);

extern EXPORT_TO_CLEAN void WinGetDC (int,OS,HDC*,OS*);
extern EXPORT_TO_CLEAN OS WinReleaseDC (int,HDC,OS);

extern EXPORT_TO_CLEAN int  WinBeep (int);
extern EXPORT_TO_CLEAN void WinGetTime (int,int*,int*,int*,int*);
extern EXPORT_TO_CLEAN void WinGetDate (int,int*,int*,int*,int*,int*);
extern EXPORT_TO_CLEAN int  WinWait (int,int);
extern EXPORT_TO_CLEAN void WinGetBlinkTime (int,int*,int*);

/* PA: The following four routines used to be crosscalls. */
extern EXPORT_TO_CLEAN int WinInvalidateWindow (int, int);
extern EXPORT_TO_CLEAN int WinInvalidateRect (int, int, int, int, int, int);
extern EXPORT_TO_CLEAN int WinValidateRect (int, int, int, int, int, int);
extern EXPORT_TO_CLEAN int WinValidateRgn (int, int, int);
/* PA: Up to here. */

extern EXPORT_TO_CLEAN  int WinGetHorzResolution (void);

extern EXPORT_TO_CLEAN  int WinGetVertResolution (void);

extern EXPORT_TO_CLEAN  void WinMaxFixedWindowSize (int*,int*);

extern EXPORT_TO_CLEAN  void WinMaxScrollWindowSize (int*,int*);

//	PA: WinScreenYSize and WinScreenXSize already implemented, now also exported.
extern EXPORT_TO_CLEAN  void WinScreenYSize (OS,int*,OS*);
extern EXPORT_TO_CLEAN  void WinScreenXSize (OS,int*,OS*);

extern EXPORT_TO_CLEAN  void WinMinimumWinSize (int*,int*);

//	PA: WinScrollbarSize added to determine system metrics of width and height of scrollbars.
extern EXPORT_TO_CLEAN  void WinScrollbarSize (OS,int*,int*,OS*);

extern EXPORT_TO_CLEAN  OS WinActivateShortcutTable (LPACCEL,int,OS);

extern EXPORT_TO_CLEAN  OS WinCopyShortcutToTable (int,int,int,LPACCEL,OS);

extern EXPORT_TO_CLEAN  void WinAllocShortcutTable (int,OS,LPACCEL*,OS*);

extern EXPORT_TO_CLEAN  void WinGetPicStringWidth (CLEAN_STRING,HDC,OS,int*,HDC*,OS*);

extern EXPORT_TO_CLEAN  void WinGetPicCharWidth (int,HDC,OS,int*,HDC*,OS*);

extern EXPORT_TO_CLEAN  void WinGetStringWidth (CLEAN_STRING,CLEAN_STRING,int,int,int,HDC,OS,int*,OS*);

extern EXPORT_TO_CLEAN  void WinGetCharWidth (int,CLEAN_STRING,int,int,int,HDC,OS,int*,OS*);

extern EXPORT_TO_CLEAN  void WinGetPicFontInfo (HDC,OS,int*,int*,int*,int*,HDC*,OS*);

extern EXPORT_TO_CLEAN  void WinGetFontInfo (CLEAN_STRING,int,int,int,HDC,OS,int*,int*,int*,int*,OS*);

extern EXPORT_TO_CLEAN  void WinSetFontStyle (int,HDC,OS,HDC*,OS*);

extern EXPORT_TO_CLEAN  void WinSetFontSize (int,HDC,OS,HDC*,OS*);

extern EXPORT_TO_CLEAN  void WinSetFontName (CLEAN_STRING,HDC,OS,HDC*,OS*);

extern EXPORT_TO_CLEAN  void WinSetFont (CLEAN_STRING,int,int,HDC,OS,HDC*,OS*);

/*	PA: two routines that print bitmaps. */
extern EXPORT_TO_CLEAN void WinPrintBitmap (int,int,int,int,char*,HDC,int,HDC*,int*);
extern EXPORT_TO_CLEAN void WinPrintResizedBitmap (int,int,int,int,int,int,char*,HDC,int,HDC*,int*);
/*	PA: two routines that draw bitmaps. */
extern EXPORT_TO_CLEAN void WinDrawBitmap (int,int,int,int,HBITMAP,HDC,int,HDC*,int*);
extern EXPORT_TO_CLEAN void WinDrawResizedBitmap (int,int,int,int,int,int,HBITMAP,HDC,int,HDC*,int*);
/*	PA: new routine that creates a HBITMAP from a DIB definition (char*).
*/
extern EXPORT_TO_CLEAN void WinCreateBitmap (int,char*,HDC,int,HBITMAP*,int*);

/*	PA: two new routines that temporarily create and destroy a DISPLAY HDC.
		Use this HDC only for local use.
*/
extern EXPORT_TO_CLEAN void WinCreateScreenHDC (OS,HDC*,OS*);
extern EXPORT_TO_CLEAN OS WinDestroyScreenHDC (HDC,OS);

extern EXPORT_TO_CLEAN  void WinInvertPolygon (HDC,OS,HDC*,OS*);

extern EXPORT_TO_CLEAN  void WinErasePolygon (HDC,OS,HDC*,OS*);

extern EXPORT_TO_CLEAN  void WinFillPolygon (HDC,OS,HDC*,OS*);

extern EXPORT_TO_CLEAN  void WinDrawPolygon (HDC,OS,HDC*,OS*);

extern EXPORT_TO_CLEAN  OS WinAddPolygonPoint (int,int,OS);

extern EXPORT_TO_CLEAN  OS WinStartPolygon (int,OS);

extern EXPORT_TO_CLEAN  OS WinEndPolygon (OS);

extern EXPORT_TO_CLEAN  void WinInvertWedge (int,int,int,int,int,int,int,int,HDC,OS,HDC*,OS*);

extern EXPORT_TO_CLEAN  void WinEraseWedge (int,int,int,int,int,int,int,int,HDC,OS,HDC*,OS*);

extern EXPORT_TO_CLEAN  void WinFillWedge (int,int,int,int,int,int,int,int,HDC,OS,HDC*,OS*);

extern EXPORT_TO_CLEAN  void WinDrawWedge (int,int,int,int,int,int,int,int,HDC,OS,HDC*,OS*);

extern EXPORT_TO_CLEAN  void WinInvertCircle (int,int,int,HDC,OS,HDC*,OS*);

extern EXPORT_TO_CLEAN  void WinEraseCircle (int,int,int,HDC,OS,HDC*,OS*);

extern EXPORT_TO_CLEAN  void WinFillCircle (int,int,int,HDC,OS,HDC*,OS*);

extern EXPORT_TO_CLEAN  void WinDrawCircle (int,int,int,HDC,OS,HDC*,OS*);

extern EXPORT_TO_CLEAN  void WinInvertOval (int,int,int,int,HDC,OS,HDC*,OS*);

extern EXPORT_TO_CLEAN  void WinEraseOval (int,int,int,int,HDC,OS,HDC*,OS*);

extern EXPORT_TO_CLEAN  void WinFillOval (int,int,int,int,HDC,OS,HDC*,OS*);

extern EXPORT_TO_CLEAN  void WinDrawOval (int,int,int,int,HDC,OS,HDC*,OS*);

extern EXPORT_TO_CLEAN  void WinInvertRoundRectangle (int,int,int,int,int,int,HDC,OS,HDC*,OS*);

extern EXPORT_TO_CLEAN  void WinEraseRoundRectangle (int,int,int,int,int,int,HDC,OS,HDC*,OS*);

extern EXPORT_TO_CLEAN  void WinFillRoundRectangle (int,int,int,int,int,int,HDC,OS,HDC*,OS*);

extern EXPORT_TO_CLEAN  void WinDrawRoundRectangle (int,int,int,int,int,int,HDC,OS,HDC*,OS*);
// PA+++
extern EXPORT_TO_CLEAN void WinScrollRectangle (int,int,int,int,int,int,HDC,OS,int*,int*,int*,int*,HDC*,OS*);
// +++PA
extern EXPORT_TO_CLEAN  void WinCopyRectangle (int,int,int,int,int,int,HDC,OS,HDC*,OS*);

extern EXPORT_TO_CLEAN  void WinCopyRectangleTo (int,int,int,int,int,int,HDC,OS,HDC*,OS*);

extern EXPORT_TO_CLEAN  void WinMoveRectangle (int,int,int,int,int,int,HDC,OS,HDC*,OS*);

extern EXPORT_TO_CLEAN  void WinMoveRectangleTo (int,int,int,int,int,int,HDC,OS,HDC*,OS*);

extern EXPORT_TO_CLEAN  void WinInvertRectangle (int,int,int,int,HDC,OS,HDC*,OS*);

extern EXPORT_TO_CLEAN  void WinEraseRectangle (int,int,int,int,HDC,OS,HDC*,OS*);

extern EXPORT_TO_CLEAN  void WinFillRectangle (int,int,int,int,HDC,OS,HDC*,OS*);

extern EXPORT_TO_CLEAN  void WinDrawRectangle (int,int,int,int,HDC,OS,HDC*,OS*);

extern EXPORT_TO_CLEAN  void WinDrawChar (int,HDC,OS,HDC*,OS*);

extern EXPORT_TO_CLEAN  void WinDrawString (CLEAN_STRING,HDC,OS,HDC*,OS*);

extern EXPORT_TO_CLEAN  void WinDrawCCurve (int,int,int,int,int,int,int,int,int,int,int,HDC,OS,HDC*,OS*);

extern EXPORT_TO_CLEAN  void WinDrawCLine (int,int,int,int,int,int,int,HDC,OS,HDC*,OS*);

extern EXPORT_TO_CLEAN  void WinDrawCPoint (int,int,int,int,int,HDC,OS,HDC*,OS*);

extern EXPORT_TO_CLEAN  void WinDrawCurve (int,int,int,int,int,int,int,int,HDC,OS,HDC*,OS*);

extern EXPORT_TO_CLEAN  void WinDrawLine (int,int,int,int,HDC,OS,HDC*,OS*);

extern EXPORT_TO_CLEAN  void WinDrawPoint (int,int,HDC,OS,HDC*,OS*);

extern EXPORT_TO_CLEAN  void WinLinePen (int,int,HDC,OS,HDC*,OS*);

extern EXPORT_TO_CLEAN  void WinLinePenTo (int,int,HDC,OS,HDC*,OS*);

extern EXPORT_TO_CLEAN  void WinMovePen (int,int,HDC,OS,HDC*,OS*);

extern EXPORT_TO_CLEAN  void WinMovePenTo (int,int,HDC,OS,HDC*,OS*);

extern EXPORT_TO_CLEAN  void WinSetPattern (int,HDC,OS,HDC*,OS*);

extern EXPORT_TO_CLEAN  void WinGetPenPos( HDC, OS, int*, int*, HDC*, OS* );

extern EXPORT_TO_CLEAN  void WinSetMode (int,HDC,OS,HDC*,OS*);

extern EXPORT_TO_CLEAN  void WinSetBackColor (int,int,int,HDC,OS,HDC*,OS*);

extern EXPORT_TO_CLEAN  void WinSetPenColor (int,int,int,HDC,OS,HDC*,OS*);

extern EXPORT_TO_CLEAN  void WinSetPenSize (int,HDC,OS,HDC*,OS*);

extern EXPORT_TO_CLEAN  void WinClipPicture (int,int,int,int,HDC,OS,HDC*,OS*);

extern EXPORT_TO_CLEAN  void WinClipRgnPicture(HRGN,HDC,OS,HDC*,OS*);		// PA: changed, no HRGN result
extern EXPORT_TO_CLEAN void WinSetClipRgnPicture (HRGN,HDC,OS,HDC*,OS*);    // PA+++
extern EXPORT_TO_CLEAN void WinGetClipRgnPicture (HDC,OS,HRGN*,HDC*,OS*);	// PA+++

/*	PA: Operations to create, modify, and destroy polygon shapes.
*/
extern EXPORT_TO_CLEAN  void WinAllocPolyShape (int,OS,POINT**,OS*);

extern EXPORT_TO_CLEAN  OS   WinSetPolyPoint (int,int,int,POINT*,OS);

extern EXPORT_TO_CLEAN  OS   WinFreePolyShape (POINT*,OS);
//	PA: end of addition

/*	PA: operations to create, modify and destroy regions.
*/
extern EXPORT_TO_CLEAN  void WinCreateRectRgn (int,int,int,int,OS,HRGN*,OS*);

extern EXPORT_TO_CLEAN  void WinCreatePolygonRgn (POINT*,int,int,OS,HRGN*,OS*);

extern EXPORT_TO_CLEAN  void WinSetRgnToRect (int,int,int,int,HRGN,OS,HRGN*,OS*);

extern EXPORT_TO_CLEAN  void WinCombineRgn (HRGN,HRGN,HRGN,int,OS,HRGN*,OS*);

extern EXPORT_TO_CLEAN  void WinGetRgnBox (HRGN,OS,int*,int*,int*,int*,BOOL*,BOOL*,OS*);

extern EXPORT_TO_CLEAN  OS WinDeleteObject (HGDIOBJ,OS);
//	PA: end of addition

// PA: simplified version of WinDonePicture, because nobody is interested in final values.
extern EXPORT_TO_CLEAN  void WinDonePicture (HDC,OS,int*,int*,int*,int*,int*,int*,int*,int*,int*,int*,CLEAN_STRING*,int*,int*,HDC*,OS*);

//extern EXPORT_TO_CLEAN void WinDonePicture (HDC,OS,HDC*,OS*);

extern EXPORT_TO_CLEAN  void WinInitPicture (int,int,int,int,int,int,int,int,int,int,CLEAN_STRING,int,int,int,int,HDC,OS,HDC*,OS*);

//	MW: get the resolution of a picture
extern EXPORT_TO_CLEAN void getResolutionC(int,int*,int*);

// MW: get scaling factors, which have to be applied to coordinates for clipping regions in case 
// of emulating the screen resolution for printing (MM_ISOTROPIC)
extern EXPORT_TO_CLEAN void WinGetPictureScaleFactor(int,int,int*,int*,int*,int*,int*,int*);



/* Mike... */

/* Game Result Codes */
#define GR_OK					 0
#define GR_FAILED               -1
#define GR_OS_ERROR				-2	/* OS function returns an error */
#define GR_INVALID_BITMAP_ID	-3
#define GR_INVALID_SPRITE_ID	-4
#define GR_INVALID_MAP_ID		-5
#define GR_NOT_FOUND			-6	/* file or resource not found */

extern EXPORT_TO_CLEAN  CLEAN_STRING WinBinaryIntStr (int);
extern EXPORT_TO_CLEAN  CLEAN_STRING WinBinaryBoolStr (BOOL);
extern EXPORT_TO_CLEAN  void WinInitGameBitmap (int,CLEAN_STRING,int,int,int,int,OS,int*,OS*);
extern EXPORT_TO_CLEAN  void WinGameBitmapDone (int,OS,int*,OS*);
extern EXPORT_TO_CLEAN  void WinClearAllGameBitmaps (OS,int*,OS*);
extern EXPORT_TO_CLEAN  void WinSetTransparentColor (int,int,int,OS,int*,OS*);
extern EXPORT_TO_CLEAN  void WinInitBlockSequence (int,int,CLEAN_STRING,OS,int*,OS*);
extern EXPORT_TO_CLEAN  void WinInitGameLayerMap (int,int,CLEAN_STRING,int,int,BOOL,OS,int*,OS*);
extern EXPORT_TO_CLEAN  void WinGameLayerMapDone (int,OS,int*,OS*);
extern EXPORT_TO_CLEAN  void WinSetGameBoundMap (int,int,CLEAN_STRING,int,int,int,int,int,OS,int*,OS*);
extern EXPORT_TO_CLEAN  void WinMoveScreenTo (int,int,OS,int*,OS*);
extern EXPORT_TO_CLEAN  void WinInitSpriteAnimation (int,CLEAN_STRING,BOOL,OS,int*,OS*);
// extern EXPORT_TO_CLEAN  void WinInitGameObject (int,int,int,int,OS,int*,OS*);
extern EXPORT_TO_CLEAN  void WinSetObjectFocus (int,int,int,int,int,int,OS,int*,OS*);
// extern EXPORT_TO_CLEAN  void WinCreateUserEvent (int,int,int,int,OS,int*,OS*);
extern EXPORT_TO_CLEAN  void WinSetObjectRec (int,int,int,BOOL,int,int,
                                              int,int,int,int,int,int,
                                              int,int,int,int,int,
                                              BOOL,int,int,int,int,int,int,int,int,int,int,int,int,
                                              int,OS,int*,OS*);
extern EXPORT_TO_CLEAN  void WinGetObjectRec (int,OS,int*,int*,BOOL*,
                                              int*,int*,int*,int*,int*,
                                              int*,int*,int*,
                                              int*,int*,int*,int*,int*,int*,
                                              BOOL*,int*,int*,int*,int*,int*,int*,int*,int*,int*,int*,int*,int*,
                                              int*,OS*);
extern EXPORT_TO_CLEAN  void WinShowStatistic (int,int,CLEAN_STRING,int,int,int,int,CLEAN_STRING,int,BOOL,BOOL,BOOL,int,int,int,int,int,int,OS,int*,OS*);
extern EXPORT_TO_CLEAN  void WinPlayMusic (CLEAN_STRING,BOOL,OS,int*,OS*);
extern EXPORT_TO_CLEAN  void WinStopMusic (OS,int*,OS*);
extern EXPORT_TO_CLEAN  void WinGameLevelOptions (int,int,int,BOOL,BOOL,BOOL,BOOL,OS,int*,OS*);
extern EXPORT_TO_CLEAN  void WinInitSoundSample (int,CLEAN_STRING,int,OS,int*,OS*);
extern EXPORT_TO_CLEAN  void WinGetBoundMap (int,int,OS,int*,int*,OS*);
extern EXPORT_TO_CLEAN  void WinSetBoundMap (int,int,int,OS,int*,OS*);
// extern EXPORT_TO_CLEAN  void WinPlaySoundSample (int,int,int,int,int,OS,int*,OS*);

/* ...Mike */
