implementation module buttonAGEC

import genericgecs, guigecs, infragecs
import StdPSt

// some handy buttons
	
instance toInt Button where
	toInt any = 0

derive generate Button
instance parseprint Button where
	parseGEC any = Just undef
	printGEC any = "Button"

gGEC{|Button|} gecArgs=:{gec_value=mv} pSt
	= basicGEC typeName tGEC (buttonGECGUI typeName (setGECvalue tGEC)) gecArgs pSt1
where
	(tGEC,pSt1)	= openGECId pSt
	typeName	= "Button"
	(bwidth,buttonname)	= case mv of Just (Button w name) = (w,name)
							         Nothing              = (defCellWidth,"??")
	
	buttonGECGUI typeName setValue outputOnly pSt
		# (sId,pSt) = openId  pSt
		# (rId,pSt)	= openRId pSt
		# buttonGUI	=     ButtonControl buttonname  [ ControlTip      (":: "+++typeName)
	                                                , ControlId       sId
	                                                , ControlFunction setButton
	                                                , ControlViewSize {w=bwidth,h=defCellHeight}
	                                                ]
					  :+: Receiver rId (setButton2 sId) []
	    = customGECGUIFun Nothing [] undef buttonGUI (update rId) outputOnly pSt
	where
		setButton (ls,pSt)
			= (ls,setValue YesUpdate Pressed pSt)
		setButton2 sId (Button _ name) (ls,pSt)
			= (ls,appPIO (setControlText sId name) pSt)
		setButton2 sId Pressed (ls,pSt)
			= (ls,pSt)
		update rId b pSt
			= snd (syncSend rId b pSt)

derive generate Checkbox
instance parseprint Checkbox where
	parseGEC "Checked" =  Just Checked
	parseGEC any		= Just NotChecked
	printGEC Checked = "Checked"
	printGEC any = "NotChecked"

gGEC{|Checkbox|} gecArgs=:{gec_value=mv} pSt
	= basicGEC typeName tGEC (checkGECGUI typeName (setGECvalue tGEC)) gecArgs pSt1
where
	(tGEC,pSt1)			= openGECId pSt
	typeName			= "Checkbox"
	mark = case mv of
				Just Checked 	= Mark
				Just NotChecked = NoMark
				any				= NoMark 
	
	checkGECGUI typeName setValue outputOnly pSt
		# (sId,pSt) = openId  pSt
		# (rId,pSt)	= openRId pSt
		# buttonGUI	=     CheckControl  [("", Just (PixelWidth (defCellWidth / 8)), mark, setCheckbox)] (Rows 0)
										[ ControlTip      (":: "+++typeName)
	                                    , ControlId       sId
	                                    , ControlFunction setCheckbox
	                                    , ControlViewSize {w=defCellWidth / 8,h=defCellHeight}
	                                    ]
					  :+: Receiver rId (setCheckbox2 sId) []
	    = customGECGUIFun Nothing [] undef buttonGUI (update rId) outputOnly pSt
	where
		setCheckbox (Checked,pSt)
			= (NotChecked,setValue YesUpdate NotChecked pSt)
		setCheckbox (NotChecked,pSt)
			= (Checked,setValue YesUpdate Checked pSt)
		setCheckbox2 sId Checked (ls,pSt)
			= (Checked,appPIO (markCheckControlItems sId [1]) pSt)
		setCheckbox2 sId NotChecked (ls,pSt)
			= (NotChecked,appPIO (unmarkCheckControlItems sId [1]) pSt)
		update rId b pSt
			= snd (syncSend rId b pSt)


derive generate Text
instance parseprint Text where
	parseGEC any 	=  Just (Text "")
	printGEC (Text t) = t
	printGEC any 	= ""

gGEC{|Text|} gecArgs=:{gec_value=mv} pSt
	= basicGEC typeName tGEC (checkGECGUI typeName (setGECvalue tGEC)) gecArgs pSt1
where
	(tGEC,pSt1)			= openGECId pSt
	typeName			= "Checkbox"
	mytext = case mv of
				Just (Text any) = any
				any				= "" 
	
	checkGECGUI typeName setValue outputOnly pSt
		# (sId,pSt) = openId  pSt
		# (rId,pSt)	= openRId pSt
		# buttonGUI	= TextControl  mytext 	[ ControlTip      (":: "+++typeName)
	                                    		, ControlId       sId
	                                    		]
					  :+: Receiver rId (setCheckbox2 sId) []
	    = customGECGUIFun Nothing [] undef buttonGUI (update rId) outputOnly pSt
	where
		setCheckbox (newtext,pSt)
			= (newtext,setValue YesUpdate newtext pSt)
		setCheckbox2 sId newtext (ls,pSt)
			= (newtext,appPIO (setControlText sId newtext) pSt)
		update rId (Text b) pSt
			= snd (syncSend rId b pSt)			