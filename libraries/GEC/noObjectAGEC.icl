implementation module noObjectAGEC

import StdAGEC

gGEC{|NoObject|} gGECa {location,makeUpValue,outputOnly,gec_value=valueNoObj,update=updateNoObj} pSt
	# (aGECVALUE,pSt)	= gGECa {location=location,makeUpValue=makeUpValue,outputOnly=outputOnly,gec_value=ma,update=updateA updateNoObj,hasOBJECT=False} pSt
	= (toNoObjectGECVALUE aGECVALUE,pSt)
where
	ma	= case valueNoObj of
			Just (NoObject a)	-> Just a
			_					-> Nothing
	
	updateA :: (Update (NoObject a) .env) UpdateReason a .env -> .env
	updateA updateNoObj updReason new_a env
		= updateNoObj updReason (NoObject new_a) env
	
	toNoObjectGECVALUE :: (GECVALUE a .env) -> GECVALUE (NoObject a) .env
	toNoObjectGECVALUE gvA
		= { gecOpen     = gvA.gecOpen
		  , gecClose    = gvA.gecClose
		  , gecOpenGUI  = gvA.gecOpenGUI
		  , gecCloseGUI = gvA.gecCloseGUI
		  , gecGetValue = \env -> let (a`,env`) = gvA.gecGetValue env in (NoObject a`,env`)
		  , gecSetValue = \inclUpd (NoObject a) env -> gvA.gecSetValue inclUpd a env
		  , gecSwitch   = gvA.gecSwitch
		  , gecArrange  = gvA.gecArrange
		  , gecOpened   = gvA.gecOpened
		  }

noObjectAGEC :: a -> AGEC a	| gGEC{|*|} a	// identity, no OBJ pulldown menu constructed.
noObjectAGEC j = mkAGEC {	toGEC	= \i _ -> NoObject i
						,	fromGEC = \(NoObject i) -> i
						,	value	= j
						,	updGEC	= id
						} "noObjectAGEC"
