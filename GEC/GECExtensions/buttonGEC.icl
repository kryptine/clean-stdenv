implementation module buttonGEC

import genericgecs, guigecs, infragecs
import StdPSt

// some handy buttons
	
instance toInt Button where
	toInt any = 0

derive generate Button
instance parseprint Button where
	parseGEC any 	= Just undef
	printGEC any		= "any"

gGEC{|Button|} gecArgs=:{gec_value=mv} pSt
	= basicGEC typeName tGEC (buttonGECGUI typeName (setGECvalue tGEC)) gecArgs pSt1
where
	(tGEC,pSt1)	= openGECId pSt
	typeName	= "Button"
	buttonname	= case mv of Just (Button name) = name
							 Nothing			= "??"
	
	buttonGECGUI typeName setValue outputOnly pSt
		# (sId,pSt) = openId  pSt
		# (rId,pSt)	= openRId pSt
		# buttonGUI	=     ButtonControl buttonname  [ ControlTip      (":: "+++typeName)
	                                                , ControlId       sId
	                                                , ControlFunction setButton
	                                                ]
					  :+: Receiver rId (setButton2 sId) []
	    = customGECGUIFun Nothing [] undef buttonGUI (update rId) outputOnly pSt
	where
		setButton (ls,pSt)
			= (ls,setValue YesUpdate Pressed pSt)
		setButton2 sId (Button name) (ls,pSt)
			= (ls,appPIO (setControlText sId name) pSt)
		setButton2 sId Pressed (ls,pSt)
			= (ls,pSt)
		update rId b pSt
			= snd (syncSend rId b pSt)
