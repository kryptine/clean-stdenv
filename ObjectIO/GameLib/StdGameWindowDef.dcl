definition module StdWindowDef


//  ********************************************************************************
//  Clean Standard Object I/O library, version 1.0.1
//
//  StdWindowDef contains the types to define the standard set of windows and
//  dialogues.
//  ********************************************************************************


import  StdControlDef


::  Window c ls ps = Window Title (c ls ps) [WindowAttribute *(ls,ps)]
::  Dialog c ls ps = Dialog Title (c ls ps) [WindowAttribute *(ls,ps)]

::  WindowAttribute ps                          // Default:
//  Attributes for Windows and Dialogs:
    =   WindowId            Id                  // system defined id
    |   WindowPos           ItemPos             // system dependent
    |   WindowIndex         Int                 // open front-most
    |   WindowSize          Size                // screen size
    |   WindowHMargin       Int Int             // system dependent
    |   WindowVMargin       Int Int             // system dependent
    |   WindowItemSpace     Int Int             // system dependent
    |   WindowOk            Id                  // no default (Custom)ButtonControl
    |   WindowCancel        Id                  // no cancel  (Custom)ButtonControl
    |   WindowHide                              // initially visible
    |   WindowClose         (IOFunction ps)     // user can't close window
    |   WindowInit          [IdFun ps]          // no actions after opening window
//  Attributes for Windows only:
    |   WindowSelectState   SelectState         // Able
    |   WindowLook          Look                // show system dependent background
    |   WindowViewDomain    ViewDomain          // {zero,max range}
    |   WindowOrigin        Point               // left top of picture domain
    |   WindowHScroll       ScrollFunction      // no horizontal scrolling
    |   WindowVScroll       ScrollFunction      // no vertical   scrolling
    |   WindowMinimumSize   Size                // system dependent
    |   WindowResize                            // fixed size
    |   WindowActivate      (IOFunction ps)     // id
    |   WindowDeactivate    (IOFunction ps)     // id
    |   WindowMouse         MouseStateFilter    SelectState (MouseFunction    ps)
                                                // no mouse input
    |   WindowKeyboard      KeyboardStateFilter SelectState (KeyboardFunction ps)
                                                // no keyboard input
    |   WindowCursor        CursorShape         // no change of cursor
// Mike ... //
//  Attributes for GameWindows only:
    |   WindowColorDepth    Int                 // 8
    |   WindowFullScreen    Bool                // True
// ... Mike //
::  CursorShape
    =   StandardCursor
    |   BusyCursor
    |   IBeamCursor
    |   CrossCursor
    |   FatCrossCursor
    |   ArrowCursor
    |   HiddenCursor

::  WindowType
    :== String
