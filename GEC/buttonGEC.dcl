definition module buttonGEC

import genericgecs

// various buttons and controls ...

derive gGEC Button, Checkbox, Text, UpDown

:: Button 	= Button Int String | Pressed
:: UpDown 	= UpPressed | DownPressed | Neutral
:: Checkbox = Checked | NotChecked 
:: Text 	= Text String

