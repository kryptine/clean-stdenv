module list

import StdEnv, StdHtml

derive gForm []
derive gUpd []

Start world = doHtmlServer (multiUserTask 2 listControl) world


listControl
=					[Txt "Define the list:",Br,Br]
					?>>	appIData (vertlistFormButs 5 True (Init,sFormId "list0" [0]))
	=>> \list ->	[]
					?>> (1,"Control List:") 
						@:	(	[Txt "Control the list:",Br,Br]
								?>> appIData (vertlistFormButs 1 True (Init,sFormId "list1" list))
							)
	=>> \list ->	[Txt "sum of list = ",Br,Br]
					?>> editTask "OK" (DisplayMode (sum list))		 



