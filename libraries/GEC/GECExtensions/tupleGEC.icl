implementation module tupleGEC

import genericgecs, guigecs, infragecs

// (,) is used to place editors next to each other

gGEC{|(,)|} gGECa gGECb gecArgs=:{gec_value=mtuple,update=tupdate} pSt
	= convert (pairGEC spairGECGUI gGECa gGECb {gecArgs & gec_value=mpair,update=pupdate} pSt)
where
	mpair = case mtuple of
				Just (a,b) = Just (PAIR a b)
				Nothing	   = Nothing

	pupdate reason (PAIR a b) pst = tupdate reason (a,b) pst

	convert (pairhandle,pst) = ({pairhandle & gecSetValue = tupleSetValue pairhandle.gecSetValue
	                                        , gecGetValue = tupleGetValue pairhandle.gecGetValue
	                            },pst)
	
	tupleSetValue pairSetValue upd (a,b)  = pairSetValue upd (PAIR a b)
	tupleGetValue pairGetValue pst
		# (PAIR a b,pst) = pairGetValue pst
		= ((a,b),pst)


	spairGECGUI :: GECGUIFun (PAIR a b) (PSt .ps)
	spairGECGUI = spairGECGUI`
	where
		spairGECGUI` outputOnly pSt
			# (id1,pSt)	= openId pSt
			# (id2,pSt)	= openId pSt
			= customGECGUIFun Nothing [(id1,Just (Left,zero),Just (Right,zero)),(id2,Nothing,Just (Right,zero))] undef NilLS (const id) outputOnly pSt
