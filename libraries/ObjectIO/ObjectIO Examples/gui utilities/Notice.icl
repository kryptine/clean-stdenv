implementation module Notice

//	**************************************************************************************************
//
//	A new instance of the Dialogs type constructor class to easily create simple notice dialogues.
//
//	This module has been written in Clean 1.3.1 and uses the Clean Standard Object I/O library 1.0.2
//	
//	**************************************************************************************************

import StdEnv, StdIO

/*  A simple state type.
*/
::  NoState
    =   NoState

/*  The data type that defines a notice.
*/
::  Notice    ls pst = Notice [String] (NoticeButton *(ls,pst)) [NoticeButton *(ls,pst)]
::  NoticeButton  st = NoticeButton String (IdFun st)

/*  Notices are defined as a new instance of the Dialogs type constructor class.
*/
instance Dialogs Notice where
    openDialog :: .ls !(Notice .ls (PSt .l .p)) !(PSt .l .p) -> (!ErrorReport,!PSt .l .p)
    openDialog ls notice ps
        # (wId, ps) = accPIO openId ps
        # (okId,ps) = accPIO openId ps
        = openDialog ls (noticeToDialog wId okId notice) ps
    
    openModalDialog :: .ls !(Notice .ls (PSt .l .p)) !(PSt .l .p) -> (!(!ErrorReport,!Maybe .ls),!PSt .l .p)
    openModalDialog ls notice ps
        # (wId,ps)  = accPIO openId ps
        # (okId,ps) = accPIO openId ps
        = openModalDialog ls (noticeToDialog wId okId notice) ps
    
    getDialogType :: (Notice .ls .pst) -> WindowType
    getDialogType notice
        = "Notice"

/*  A specialised version that ignores the error report.
*/
openNotice :: !(Notice .ls (PSt .l .p)) !(PSt .l .p) -> PSt .l .p
openNotice notice ps
    = snd (openModalDialog undef notice ps)

/*  noticeToDialog converts a Notice expression into a Dialog expression.
*/
noticeToDialog :: Id Id (Notice .ls (PSt .l .p)) 
               -> Dialog (:+: (CompoundControl (ListLS TextControl))
                         (:+:  ButtonControl
                              (ListLS ButtonControl)
                         )) .ls (PSt .l .p)
noticeToDialog wId okId (Notice texts (NoticeButton text f) buttons)
    = Dialog ""
        (   CompoundControl 
        (   ListLS
        [   TextControl text [ControlPos (Left,zero)]
        \\  text <- texts
        ]
        )   [ControlHMargin 0 0, ControlVMargin 0 0, ControlItemSpace 3 3]
        :+: ButtonControl text 
            [ControlFunction (noticefun f), ControlPos (Right,zero), ControlId okId]
        :+: ListLS
        [   ButtonControl text [ControlFunction (noticefun f),ControlPos (LeftOfPrev,zero)]
        \\  (NoticeButton text f) <- buttons
        ]
        )
        [   WindowId    wId
        ,   WindowOk    okId
        ]
where
    noticefun f (ls,pst) = f (ls,closeWindow wId pst)
