system module xdialog;

XCreateCommandDialog :: !{#Char} !Int !Int !Int -> Int;
XCreatePropertyDialog :: !{#Char} !Int !Int -> Int;
XCreateDialButton :: !Int !Int !Int !Int !Int !{#Char} -> Int;
XCreateStaticText :: !Int !Int !Int !Int !Int !{#Char} -> Int;
XCreateEditText :: !Int !Int !Int !Int !Int !Int !{#Char} -> Int;
XCreateRadioGroup :: !Int !Int !Int !Int !Int !Int !Int -> Int;
XCreateDialogPopup :: !Int !Int !Int !Int !Int -> Int;
XGetPopupEx :: !Int -> Int;
XCorrectPopupSize :: !Int -> Int;
XCreateDialogRadioItem :: !Int !Int !{#Char} !Int -> Int;
XCreateCheckGroup :: !Int !Int !Int !Int !Int !Int !Int -> Int;
XCreateDialogCheckItem :: !Int !Int !{#Char} !Int -> Int;
XCreateDialogControl :: !Int !Int !Int !Int !Int !Int !Int !Int -> Int;
XSetCommandDefault :: !Int !Int -> Int;
XGetEditText :: !Int -> {#Char};
XSetEditText :: !Int !{#Char} -> Int;
XSetStaticText :: !Int !{#Char} -> Int;
XGetMark :: !Int -> Int;
XPressRadioWidget :: !Int !{#Char} -> Int;
GetXDialogEvent :: !Int -> !(!Int,!Int);
XPopupModal :: !Int -> Int;
XPopupModeless :: !Int -> Int;
XCreateNotice :: !{#Char} -> Int;
XCreateAboutDialog :: !Int !Int !Int !Int !Int !{#Char} -> Int;
XCreateNoticeButton :: !Int !{#Char} !Int -> Int;
XHandleNotice :: !Int -> Int;
XBeep :: !Int -> Int;
XGetCurrentRect :: !Int -> !(!Int,!Int,!Int,!Int);
XRepositionWidget :: !Int !Int !Int !Int !Int -> Int;
XGetFatherWidth :: !Int -> Int;
XSetDialogMargins :: !Int !Int !Int -> Int;
XMMToPixelHor :: !Real -> Int;
XMMToPixelVer :: !Real -> Int;
ActivateDialogX :: !Int -> Int;
XEnableDialogItem :: !Int -> Int;
XDisableDialogItem :: !Int -> Int;
CheckDialogItemX :: !Int !Int -> Int;
DestroyDialogX :: !Int -> Int;
XPopDownDialog :: !Int -> Int;
DialogItem2Object :: !Int -> Int;