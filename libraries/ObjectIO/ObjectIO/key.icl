implementation module key


//	Clean Object I/O library, version 1.2


import	StdBool, StdClass, StdInt, StdOverloaded, StdString


::	SpecialKey
	=	{	virtual	:: !Int
		}

BackSpaceVirtualCode:==   8		// BackSpace
BeginVirtualCode	:== 115		// Begin of text
ClearVirtualCode	:==  71		// Clear
DeleteVirtualCode	:== 117		// Delete
DownVirtualCode		:== 125		// Arrow down
EndVirtualCode		:== 119		// End of text
EnterVirtualCode	:==  13		// Enter		PA: was 76
EscapeVirtualCode	:==  53		// Escape
F1VirtualCode		:== 122		// Function 1
F2VirtualCode		:== 120		// Function 2
F3VirtualCode		:==  99		// Function 3
F4VirtualCode		:== 118		// Function 4
F5VirtualCode		:==  96		// Function 5
F6VirtualCode		:==  97		// Function 6
F7VirtualCode		:==  98		// Function 7
F8VirtualCode		:== 100		// Function 8
F9VirtualCode		:== 101		// Function 9
F10VirtualCode		:== 109		// Function 10
F11VirtualCode		:== 103		// Function 11
F12VirtualCode		:== 111		// Function 12
F13VirtualCode		:== 105		// Function 13
F14VirtualCode		:== 107		// Function 14
F15VirtualCode		:== 113		// Function 15
HelpVirtualCode		:== 114		// Help
LeftVirtualCode		:== 123		// Arrow left
PgDownVirtualCode	:== 121		// Page down
PgUpVirtualCode		:== 116		// Page up
RightVirtualCode	:== 124		// Arrow right
UpVirtualCode		:== 126		// Arrow up

instance == SpecialKey where
	(==) :: !SpecialKey !SpecialKey -> Bool
	(==) {virtual=v1} {virtual=v2} = v1==v2

instance toString SpecialKey where
	toString :: !SpecialKey -> {#Char}
	toString {virtual}
		= specialKeyCodeName virtual
	where
		specialKeyCodeName :: !Int -> {#Char}
		specialKeyCodeName BackSpaceVirtualCode	= "BackSpaceKey"
		specialKeyCodeName BeginVirtualCode		= "BeginKey"
		specialKeyCodeName ClearVirtualCode		= "ClearKey"
		specialKeyCodeName DeleteVirtualCode	= "DeleteKey"
		specialKeyCodeName DownVirtualCode		= "DownKey"
		specialKeyCodeName EndVirtualCode		= "EndKey"
		specialKeyCodeName EnterVirtualCode		= "EnterKey"
		specialKeyCodeName EscapeVirtualCode	= "EscapeKey"
		specialKeyCodeName F1VirtualCode		= "F1Key"
		specialKeyCodeName F2VirtualCode		= "F2Key"
		specialKeyCodeName F3VirtualCode		= "F3Key"
		specialKeyCodeName F4VirtualCode		= "F4Key"
		specialKeyCodeName F5VirtualCode		= "F5Key"
		specialKeyCodeName F6VirtualCode		= "F6Key"
		specialKeyCodeName F7VirtualCode		= "F7Key"
		specialKeyCodeName F8VirtualCode		= "F8Key"
		specialKeyCodeName F9VirtualCode		= "F9Key"
		specialKeyCodeName F10VirtualCode		= "F10Key"
		specialKeyCodeName F11VirtualCode		= "F11Key"
		specialKeyCodeName F12VirtualCode		= "F12Key"
		specialKeyCodeName F13VirtualCode		= "F13Key"
		specialKeyCodeName F14VirtualCode		= "F14Key"
		specialKeyCodeName F15VirtualCode		= "F15Key"
		specialKeyCodeName HelpVirtualCode		= "HelpKey"
		specialKeyCodeName LeftVirtualCode		= "LeftKey"
		specialKeyCodeName PgDownVirtualCode	= "PgDownKey"
		specialKeyCodeName PgUpVirtualCode		= "PgUpKey"
		specialKeyCodeName RightVirtualCode		= "RightKey"
		specialKeyCodeName UpVirtualCode		= "UpKey"
		specialKeyCodeName otherCode			= "toSpecialKey "+++toString otherCode

BackSpaceKey:: SpecialKey;			BackSpaceKey= {virtual=BackSpaceVirtualCode}// BackSpace
BeginKey	:: SpecialKey;			BeginKey	= {virtual=BeginVirtualCode}	// Begin of text
ClearKey	:: SpecialKey;			ClearKey	= {virtual=ClearVirtualCode}	// Clear
DeleteKey	:: SpecialKey;			DeleteKey	= {virtual=DeleteVirtualCode}	// Delete
DownKey		:: SpecialKey;			DownKey		= {virtual=DownVirtualCode}		// Arrow down
EndKey		:: SpecialKey;			EndKey		= {virtual=EndVirtualCode}		// End of text
EnterKey	:: SpecialKey;			EnterKey	= {virtual=EnterVirtualCode}	// Enter
EscapeKey	:: SpecialKey;			EscapeKey	= {virtual=EscapeVirtualCode}	// Escape
F1Key		:: SpecialKey;			F1Key		= {virtual=F1VirtualCode}		// Function 1
F2Key		:: SpecialKey;			F2Key		= {virtual=F2VirtualCode}		// Function 2
F3Key		:: SpecialKey;			F3Key		= {virtual=F3VirtualCode}		// Function 3
F4Key		:: SpecialKey;			F4Key		= {virtual=F4VirtualCode}		// Function 4
F5Key		:: SpecialKey;			F5Key		= {virtual=F5VirtualCode}		// Function 5
F6Key		:: SpecialKey;			F6Key		= {virtual=F6VirtualCode}		// Function 6
F7Key		:: SpecialKey;			F7Key		= {virtual=F7VirtualCode}		// Function 7
F8Key		:: SpecialKey;			F8Key		= {virtual=F8VirtualCode}		// Function 8
F9Key		:: SpecialKey;			F9Key		= {virtual=F9VirtualCode}		// Function 9
F10Key		:: SpecialKey;			F10Key		= {virtual=F10VirtualCode}		// Function 10
F11Key		:: SpecialKey;			F11Key		= {virtual=F11VirtualCode}		// Function 11
F12Key		:: SpecialKey;			F12Key		= {virtual=F12VirtualCode}		// Function 12
F13Key		:: SpecialKey;			F13Key		= {virtual=F13VirtualCode}		// Function 13
F14Key		:: SpecialKey;			F14Key		= {virtual=F14VirtualCode}		// Function 14
F15Key		:: SpecialKey;			F15Key		= {virtual=F15VirtualCode}		// Function 15
HelpKey		:: SpecialKey;			HelpKey		= {virtual=HelpVirtualCode}		// Help
LeftKey		:: SpecialKey;			LeftKey		= {virtual=LeftVirtualCode}		// Arrow left
PgDownKey	:: SpecialKey;			PgDownKey	= {virtual=PgDownVirtualCode}	// Page down
PgUpKey		:: SpecialKey;			PgUpKey		= {virtual=PgUpVirtualCode}		// Page up
RightKey	:: SpecialKey;			RightKey	= {virtual=RightVirtualCode}	// Arrow right
UpKey		:: SpecialKey;			UpKey		= {virtual=UpVirtualCode}		// Arrow up

toSpecialKey :: !Int -> SpecialKey
toSpecialKey specialkey = {virtual=specialkey}

isSpecialKey:: !Int -> Bool
isSpecialKey specialKey
	= containsSorted specialKey VirtualKeyCodes
where
	containsSorted :: !Int ![Int] -> Bool
	containsSorted x [y:ys]
		| x>y		= containsSorted x ys
		| otherwise	= x==y
	containsSorted _ _
		= False

VirtualKeyCodes :: [Int]						// The < sorted list of virtual key codes
VirtualKeyCodes	=:	[	BackSpaceVirtualCode	//   8
					,	EscapeVirtualCode		//  53
					,	ClearVirtualCode		//  71
					,	EnterVirtualCode		//  76
					,	F5VirtualCode			//  96
					,	F6VirtualCode			//  97
					,	F7VirtualCode			//  98
					,	F3VirtualCode			//  99
					,	F8VirtualCode			// 100
					,	F9VirtualCode			// 101
					,	F11VirtualCode			// 103
					,	F13VirtualCode			// 105
					,	F14VirtualCode			// 107
					,	F10VirtualCode			// 109
					,	F12VirtualCode			// 111
					,	F15VirtualCode			// 113
					,	HelpVirtualCode			// 114
					,	BeginVirtualCode		// 115
					,	PgUpVirtualCode			// 116
					,	DeleteVirtualCode		// 117
					,	F4VirtualCode			// 118
					,	EndVirtualCode			// 119
					,	F2VirtualCode			// 120
					,	PgDownVirtualCode		// 121
					,	F1VirtualCode			// 122
					,	LeftVirtualCode			// 123
					,	RightVirtualCode		// 124
					,	DownVirtualCode			// 125
					,	UpVirtualCode			// 126
					]
