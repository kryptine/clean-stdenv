system module xmenu;

AddMenuBar :: !Int -> Int;
AddMenu :: !Int !{#Char} -> Int;
AddSubMenu :: !Int !{#Char} -> Int;
AddCheckItem :: !Int !{#Char} !Int -> Int;
AddMenuSeparator :: !Int -> Int;
AddMenuItem :: !Int !{#Char} -> Int;
EnableMenuWidgetX :: !Int -> Int;
DisableMenuWidgetX :: !Int -> Int;
CheckWidget :: !Int !Int -> Int;
SetWidgetTitle :: !Int !{#Char} -> Int;
InstallShortcut :: !Int !{#Char} -> Int;
HideMenuX :: !Int -> Int;
ShowMenuX :: !Int -> Int;
GetItemInfo :: !Int -> (!Int,!Int,!{#Char},!{#Char});
GetSubmenuInfo :: !Int -> (!{#Char},!Int);
DestroyItemWidget :: !Int -> Int;
DestroyMenu :: !Int -> Int;
