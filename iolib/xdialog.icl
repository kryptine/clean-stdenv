implementation module xdialog;

XCreateCommandDialog :: !{#Char} !Int !Int !Int -> Int;
XCreateCommandDialog _ _ _ _
	= code {
		.inline XCreateCommandDialog
			ccall create_commanddial "SIII-I"
		.end
	};

XCreatePropertyDialog :: !{#Char} !Int !Int -> Int;
XCreatePropertyDialog _ _ _
	= code {
		.inline XCreatePropertyDialog
			ccall create_propertydial "SII-I"
		.end
	};

XCreateDialButton :: !Int !Int !Int !Int !Int !{#Char} -> Int;
XCreateDialButton _ _ _ _ _ _
	= code {
		.inline XCreateDialButton
			ccall add_dialog_button "IIIIIS-I"
		.end
	};

XCreateStaticText :: !Int !Int !Int !Int !Int !{#Char} -> Int;
XCreateStaticText _ _ _ _ _ _
	= code {
		.inline XCreateStaticText
			ccall add_static_text "IIIIIS-I"
		.end
	};

XCreateEditText :: !Int !Int !Int !Int !Int !Int !{#Char} -> Int;
XCreateEditText _ _ _ _ _ _ _
	= code {
		.inline XCreateEditText
			ccall add_edit_text "IIIIIIS-I"
		.end
	};

XCreateRadioGroup :: !Int !Int !Int !Int !Int !Int !Int -> Int;
XCreateRadioGroup _ _ _ _ _ _ _
	= code {
		.inline XCreateRadioGroup
			ccall add_dialog_exclusives "IIIIIII-I"
		.end
	};

XCreateDialogPopup :: !Int !Int !Int !Int !Int -> Int;
XCreateDialogPopup _ _ _ _ _
	= code {
		.inline XCreateDialogPopup
			ccall add_dialog_popup "IIIII-I"
		.end
	};

XGetPopupEx :: !Int -> Int;
XGetPopupEx _
	= code {
		.inline XGetPopupEx
			ccall get_popup_ex "I-I"
		.end
	};

XCorrectPopupSize :: !Int -> Int;
XCorrectPopupSize _
	= code {
		.inline XCorrectPopupSize
			ccall correct_popup_size "I-I"
		.end
	};

XCreateDialogRadioItem :: !Int !Int !{#Char} !Int -> Int;
XCreateDialogRadioItem _ _ _ _
	= code {
		.inline XCreateDialogRadioItem
			ccall add_dialog_radiob "IISI-I"
		.end
	};

XCreateCheckGroup :: !Int !Int !Int !Int !Int !Int !Int -> Int;
XCreateCheckGroup _ _ _ _ _ _ _
	= code {
		.inline XCreateCheckGroup
			ccall add_dialog_nonexclusives "IIIIIII-I"
		.end
	};

XCreateDialogCheckItem :: !Int !Int !{#Char} !Int -> Int;
XCreateDialogCheckItem _ _ _ _
	= code {
		.inline XCreateDialogCheckItem
			ccall add_dialog_checkb "IISI-I"
		.end
	};

XCreateDialogControl :: !Int !Int !Int !Int !Int !Int !Int !Int -> Int;
XCreateDialogControl _ _ _ _ _ _ _ _
	= code {
		.inline XCreateDialogControl
			ccall add_dialog_control "IIIIIIII-I"
		.end
	};

XSetCommandDefault :: !Int !Int -> Int;
XSetCommandDefault _ _
	= code {
		.inline XSetCommandDefault
			ccall set_command_default "II-I"
		.end
	};

XGetEditText :: !Int -> {#Char};
XGetEditText _
	= code {
		.inline XGetEditText
			ccall get_edit_text "I-S"
		.end
	};

XSetEditText :: !Int !{#Char} -> Int;
XSetEditText _ _
	= code {
		.inline XSetEditText
			ccall set_edit_text "IS-I"
		.end
	};

XSetStaticText :: !Int !{#Char} -> Int;
XSetStaticText _ _
	= code {
		.inline XSetStaticText
			ccall set_static_text "IS-I"
		.end
	};

XGetMark :: !Int -> Int;
XGetMark _
	= code {
		.inline XGetMark
			ccall get_mark "I-I"
		.end
	};

XPressRadioWidget :: !Int !{#Char} -> Int;
XPressRadioWidget _ _
	= code {
		.inline XPressRadioWidget
			ccall press_radio_widget "IS-I"
		.end
	};

GetXDialogEvent :: !Int -> (!Int,!Int);
GetXDialogEvent _
	= code {
		.inline GetXDialogEvent
			ccall get_dialog_event "I-II"
		.end
	};

XPopupModal :: !Int -> Int;
XPopupModal _
	= code {
		.inline XPopupModal
			ccall popup_modaldialog "I-I"
		.end
	};

XPopupModeless :: !Int -> Int;
XPopupModeless _
	= code {
		.inline XPopupModeless
			ccall popup_modelessdialog "I-I"
		.end
	};

XCreateNotice :: !{#Char} -> Int;
XCreateNotice _
	= code {
		.inline XCreateNotice
			ccall create_notice "S-I"
		.end
	};

XCreateAboutDialog :: !Int !Int !Int !Int !Int !{#Char} -> Int;
XCreateAboutDialog _ _ _ _ _ _
	= code {
		.inline XCreateAboutDialog
			ccall create_about_dialog "IIIIIS-I"
		.end
	};

XCreateNoticeButton :: !Int !{#Char} !Int -> Int;
XCreateNoticeButton _ _ _
	= code {
		.inline XCreateNoticeButton
			ccall add_n_button "ISI-I"
		.end
	};

XHandleNotice :: !Int -> Int;
XHandleNotice _
	= code {
		.inline XHandleNotice
			ccall handle_notice "I-I"
		.end
	};

XBeep :: !Int -> Int;
XBeep _
	= code {
		.inline XBeep
			ccall beep "I-I"
		.end
	};

XGetCurrentRect :: !Int -> (!Int,!Int,!Int,!Int);
XGetCurrentRect _
	= code {
		.inline XGetCurrentRect
			ccall get_current_rect "I-IIII"
		.end
	};

XRepositionWidget :: !Int !Int !Int !Int !Int -> Int;
XRepositionWidget _ _ _ _ _
	= code {
		.inline XRepositionWidget
			ccall repos_widget "IIIII-I"
		.end
	};

XGetFatherWidth :: !Int -> Int;
XGetFatherWidth _
	= code {
		.inline XGetFatherWidth
			ccall get_father_width "I-I"
		.end
	};

XSetDialogMargins :: !Int !Int !Int -> Int;
XSetDialogMargins _ _ _
	= code {
		.inline XSetDialogMargins
			ccall set_dialog_margins "III-I"
		.end
	};

XMMToPixelHor :: !Real -> Int;
XMMToPixelHor _
	= code {
		.inline XMMToPixelHor
			ccall mm_to_pixel_hor "R-I"
		.end
	};

XMMToPixelVer :: !Real -> Int;
XMMToPixelVer _
	= code {
		.inline XMMToPixelVer
			ccall mm_to_pixel_ver "R-I"
		.end
	};

ActivateDialogX :: !Int -> Int;
ActivateDialogX _
	= code {
		.inline ActivateDialogX
			ccall activate_dialog "I-I"
		.end
	};

XEnableDialogItem :: !Int -> Int;
XEnableDialogItem _
	= code {
		.inline XEnableDialogItem
			ccall enable_dialog_item "I-I"
		.end
	};

XDisableDialogItem :: !Int -> Int;
XDisableDialogItem _
	= code {
		.inline XDisableDialogItem
			ccall disable_dialog_item "I-I"
		.end
	};

CheckDialogItemX :: !Int !Int -> Int;
CheckDialogItemX _ _
	= code {
		.inline CheckDialogItemX
			ccall check_dialog_item "II-I"
		.end
	};

DestroyDialogX :: !Int -> Int;
DestroyDialogX _
	= code {
		.inline DestroyDialogX
			ccall destroy_dialog "I-I"
		.end
	};

XPopDownDialog :: !Int -> Int;
XPopDownDialog _
	= code {
		.inline XPopDownDialog
			ccall popdown_dialog "I-I"
		.end
	};

DialogItem2Object :: !Int -> Int;
DialogItem2Object _
	= code {
		.inline DialogItem2Object
			ccall dialog_item_to_object "I-I"
		.end
	};

