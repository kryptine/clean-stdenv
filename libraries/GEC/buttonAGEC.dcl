definition module buttonAGEC

import genericgecs

// various buttons

derive gGEC Button, Checkbox, Text

:: Button 	= Button Int String | Pressed
:: Checkbox = Checked | NotChecked 
:: Text 	= Text String
