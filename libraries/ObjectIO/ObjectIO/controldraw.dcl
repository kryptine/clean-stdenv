definition module controldraw


//	********************************************************************************
//	Clean Standard Object I/O library, version 1.2.2
//	
//	Drawing operations on customised controls
//	Author: Peter Achten
//	Modified: 7 September 2001 for Clean 2.0
//	********************************************************************************


import	ossystem, ostypes
import	wstate


/*	draw(Compound/Custom(Button))Look(`) render their control using the current look.
	Drawing is clipped inside the argument OSRect (in window coordinates).
	It is assumed that they are applied to a WItemHandle(`) argument that refers to a (Compound/CustomButton/Custom)Control respectively.
	drawCompoundLook(`) furthermore assumes that the compound control's ClipState is valid.
*/
drawCompoundLook		:: !OSWindowMetrics !Bool !OSWindowPtr !OSRect !(WItemHandle .ls .pst) !*OSToolbox -> (!WItemHandle .ls .pst,!*OSToolbox)
drawCompoundLook`		:: !OSWindowMetrics !Bool !OSWindowPtr !OSRect ! WItemHandle`          !*OSToolbox -> (!WItemHandle`,        !*OSToolbox)
drawCustomButtonLook	::                  !Bool !OSWindowPtr !OSRect !(WItemHandle .ls .pst) !*OSToolbox -> (!WItemHandle .ls .pst,!*OSToolbox)
drawCustomButtonLook`	::                  !Bool !OSWindowPtr !OSRect ! WItemHandle`          !*OSToolbox -> (!WItemHandle`,        !*OSToolbox)
drawCustomLook			::                  !Bool !OSWindowPtr !OSRect !(WItemHandle .ls .pst) !*OSToolbox -> (!WItemHandle .ls .pst,!*OSToolbox)
drawCustomLook`			::                  !Bool !OSWindowPtr !OSRect ! WItemHandle`          !*OSToolbox -> (!WItemHandle`,        !*OSToolbox)

/*	drawIn(Compound/Custom(Button))(`) apply a Picture access function to the given control Picture.
	Drawing is clipped inside the argument OSRect.
	It is assumed that they are applied to a WItemHandle(`) argument that refers to a (Compound/CustomButton/Custom)Control respectively.
	drawInCompound(`) furthermore assumes that the compound control is non-transparent and has a valid ClipState.
*/
drawInCompound			:: !OSWindowPtr !.(St *Picture .x) !OSRect !(WItemHandle .ls .pst) !*OSToolbox -> (.x,!WItemHandle .ls .pst,!*OSToolbox)
drawInCompound`			:: !OSWindowPtr !.(St *Picture .x) !OSRect ! WItemHandle`          !*OSToolbox -> (.x,!WItemHandle`,        !*OSToolbox)
drawInCustomButton		:: !OSWindowPtr !.(St *Picture .x) !OSRect !(WItemHandle .ls .pst) !*OSToolbox -> (.x,!WItemHandle .ls .pst,!*OSToolbox)
drawInCustomButton`		:: !OSWindowPtr !.(St *Picture .x) !OSRect ! WItemHandle`          !*OSToolbox -> (.x,!WItemHandle`,        !*OSToolbox)
drawInCustom			:: !OSWindowPtr !.(St *Picture .x) !OSRect !(WItemHandle .ls .pst) !*OSToolbox -> (.x,!WItemHandle .ls .pst,!*OSToolbox)
drawInCustom`			:: !OSWindowPtr !.(St *Picture .x) !OSRect ! WItemHandle`          !*OSToolbox -> (.x,!WItemHandle`,        !*OSToolbox)
