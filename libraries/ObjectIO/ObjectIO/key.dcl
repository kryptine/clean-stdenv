definition module key

//	Clean Object I/O library, version 1.2

import	StdOverloaded

::	SpecialKey

instance	==			SpecialKey					// Equality on SpecialKey
instance	toString	SpecialKey					// Name of the SpecialKey

BackSpaceKey:: SpecialKey							// Backspace
BeginKey	:: SpecialKey							// Begin of text
ClearKey	:: SpecialKey							// Clear
DeleteKey	:: SpecialKey							// Delete
DownKey		:: SpecialKey							// Arrow down
EndKey		:: SpecialKey							// End of text
EnterKey	:: SpecialKey							// Enter
EscapeKey	:: SpecialKey							// Escape
F1Key		:: SpecialKey							// Function 1
F2Key		:: SpecialKey							// Function 2
F3Key		:: SpecialKey							// Function 3
F4Key		:: SpecialKey							// Function 4
F5Key		:: SpecialKey							// Function 5
F6Key		:: SpecialKey							// Function 6
F7Key		:: SpecialKey							// Function 7
F8Key		:: SpecialKey							// Function 8
F9Key		:: SpecialKey							// Function 9
F10Key		:: SpecialKey							// Function 10
F11Key		:: SpecialKey							// Function 11
F12Key		:: SpecialKey							// Function 12
F13Key		:: SpecialKey							// Function 13
F14Key		:: SpecialKey							// Function 14
F15Key		:: SpecialKey							// Function 15
HelpKey		:: SpecialKey							// Help
LeftKey		:: SpecialKey							// Arrow left
PgDownKey	:: SpecialKey							// Page down
PgUpKey		:: SpecialKey							// Page up
RightKey	:: SpecialKey							// Arrow right
UpKey		:: SpecialKey							// Arrow up

toSpecialKey:: !Int -> SpecialKey					// Convert Int to SpecialKey
isSpecialKey:: !Int -> Bool							// Check for one of the upper SpecialKeys
