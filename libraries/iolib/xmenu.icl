implementation module xmenu;

AddMenuBar :: !Int -> Int;
AddMenuBar _
	= code {
		.inline AddMenuBar
			ccall add_menu_bar "I-I"
		.end
	};

AddMenu :: !Int !{#Char} -> Int;
AddMenu _ _
	= code {
		.inline AddMenu
			ccall add_menu "IS-I"
		.end
	};

AddSubMenu :: !Int !{#Char} -> Int;
AddSubMenu _ _
	= code {
		.inline AddSubMenu
			ccall add_sub_menu "IS-I"
		.end
	};

AddCheckItem :: !Int !{#Char} !Int -> Int;
AddCheckItem _ _ _
	= code {
		.inline AddCheckItem
			ccall add_check_item "ISI-I"
		.end
	};

AddMenuSeparator :: !Int -> Int;
AddMenuSeparator _
	= code {
		.inline AddMenuSeparator
			ccall add_menu_separator "I-I"
		.end
	};

AddMenuItem :: !Int !{#Char} -> Int;
AddMenuItem _ _
	= code {
		.inline AddMenuItem
			ccall add_menu_item "IS-I"
		.end
	};

EnableMenuWidgetX :: !Int -> Int;
EnableMenuWidgetX _
	= code {
		.inline EnableMenuWidgetX
			ccall enable_menu_widget "I-I"
		.end
	};

DisableMenuWidgetX :: !Int -> Int;
DisableMenuWidgetX _
	= code {
		.inline DisableMenuWidgetX
			ccall disable_menu_widget "I-I"
		.end
	};

CheckWidget :: !Int !Int -> Int;
CheckWidget _ _
	= code {
		.inline CheckWidget
			ccall check_widget "II-I"
		.end
	};

SetWidgetTitle :: !Int !{#Char} -> Int;
SetWidgetTitle _ _
	= code {
		.inline SetWidgetTitle
			ccall set_widget_title "IS-I"
		.end
	};

InstallShortcut :: !Int !{#Char} -> Int;
InstallShortcut _ _
	= code {
		.inline InstallShortcut
			ccall install_shortcut "IS-I"
		.end
	};

HideMenuX :: !Int -> Int;
HideMenuX _
	= code {
		.inline HideMenuX
			ccall hide_menu "I-I"
		.end
	};

ShowMenuX :: !Int -> Int;
ShowMenuX _
	= code {
		.inline ShowMenuX
			ccall show_menu "I-I"
		.end
	};

GetItemInfo :: !Int -> !(!Int,!Int,!{#Char},!{#Char});
GetItemInfo _
	= code {
		.inline GetItemInfo
			ccall get_item_info "I-IISS"
		.end
	};

GetSubmenuInfo :: !Int -> !(!{#Char},!Int);
GetSubmenuInfo _
	= code {
		.inline GetSubmenuInfo
			ccall get_submenu_info "I-SI"
		.end
	};

DestroyItemWidget :: !Int -> Int;
DestroyItemWidget _
	= code {
		.inline DestroyItemWidget
			ccall destroy_item_widget "I-I"
		.end
	};

DestroyMenu :: !Int -> Int;
DestroyMenu _
	= code {
		.inline DestroyMenu
			ccall destroy_menu "I-I"
		.end
	};

