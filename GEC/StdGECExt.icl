implementation module StdGECExt

import StdEnv
import StdIO
import genericgecs, StdGEC, StdAGEC
import store, TRACE

nothing _ _ pSt = pSt
tracing r x pSt = DO_TRACE (r,x) pSt
	
mk_GEC 		:: !(!String,!a) !*(PSt .ps) -> *(PSt .ps) | bimap{|*|} ps & gGEC{|*|} a 
mk_GEC (title,a) pSt
		#	(_,_,pSt)		=	createGEC title Interactive a nothing pSt
		=	pSt

apply_GEC 	:: !(!String,a -> b) !(!String,!a) !*(PSt .ps) -> *(PSt .ps) | bimap{|*|} ps & gGEC{|*|} a & gGEC{|*|} b
apply_GEC (f_title,f) (a_title,a) pSt
		#	(dosetF,_,pSt)	=	createGEC f_title OutputOnly  (f a) nothing                       pSt
		#	(_,_,pSt)		= 	createGEC a_title Interactive a    (\r a pst -> dosetF (f a) pst) pSt
		=	pSt

apply2_GEC 	:: !(!String,b -> c) !(!String,a -> b) !(!String,!a) !*(PSt .ps) -> *(PSt .ps) | bimap{|*|} ps & gGEC{|*|} a & gGEC{|*|} b & gGEC{|*|} c 
apply2_GEC (g_title,g) (f_title,f) (a_title,a) pSt
		#	(dosetGF,_,pSt)	=	createGEC g_title OutputOnly  (g (f a)) nothing						      pSt
		#	(_,dosetF,pSt)	=	createGEC f_title OutputOnly  (f a)     (\r f_a pst -> dosetGF (g f_a) pst) pSt
		#	(_,_,pSt)		= 	createGEC a_title Interactive a         (\r a   pst -> dosetF  (f a)   pst) pSt
		=	pSt

apply_GEC2 	:: !(!String,a b -> c) !(!String,!a) !(!String,!b) !*(PSt .ps) -> *(PSt .ps) | bimap{|*|} ps & gGEC{|*|} a & gGEC{|*|} b & gGEC{|*|} c
apply_GEC2 (f_title,f) (a_title,a) (b_title,b) pSt
		#	(aStore,pSt)	=	openStoreId pSt
		#	(_,pSt)			=	openStore aStore (Just a) pSt
	    #	(bStore,pSt)	=	openStoreId pSt
	    #	(_,pSt)			=	openStore bStore (Just b) pSt
		#	(dosetF,_,pSt)	=	createGEC f_title OutputOnly  (f a b) nothing pSt
		#	(_,_,pSt)		= 	createGEC a_title Interactive a (newA dosetF aStore bStore) pSt
		#	(_,_,pSt)		=	createGEC b_title Interactive b (newB dosetF aStore bStore) pSt
		=	pSt
	where
		newA dosetF aStore bStore r a pSt
		#	pSt		=	writeStore aStore a pSt
		#	(b,pSt)	=	readStore bStore pSt
		=	dosetF	(f a b) pSt	

		newB dosetF aStore bStore r b pSt
		#	pSt		=	writeStore bStore b pSt
		#	(a,pSt)	=	readStore aStore pSt
		=	dosetF	(f a b) pSt	

self_GEC 	:: !(a -> a) !(!String,!a) !*(PSt .ps) -> *(PSt .ps) | bimap{|*|} ps & gGEC{|*|} a 
self_GEC f (a_title,a) pSt
		=	pSt1
		where
			(dosetF,_,pSt1)		=	createGEC a_title Interactive (f a) (\r a -> dosetF (f a)) pSt

selfapply_GEC :: !(a b -> a) !(!String,!a) !(!String,!b) !*(PSt .ps) -> *(PSt .ps) | bimap{|*|} ps & gGEC{|*|} a & gGEC{|*|} b 
selfapply_GEC  f (a_title,a) (b_title,b) pSt
		#	(aStore,pSt)	=	openStoreId pSt
		#	(_,pSt)			=	openStore aStore (Just a) pSt
		#	(_,dosetF,pSt)	=	createGEC a_title OutputOnly (f a b) nothing pSt
//		#	(_,dosetF,pSt)	=	createGEC a_title (f a b) (\_ a pst -> writeStore aStore a (appPIO beep pst)) pSt
		#	(_,_,pSt)		= 	createGEC b_title Interactive b (newA dosetF aStore) pSt
		=	pSt
	where
		newA dosetF aStore  r b pSt
		#	(a,pSt)	=	readStore aStore pSt
		#	pSt		=	writeStore aStore (f a b) pSt
		=	dosetF	(f a b) pSt

pred_GEC 	:: !(a -> Bool) !(!String,!a) !*(PSt .ps) -> *(PSt .ps) | bimap{|*|} ps & gGEC{|*|} a
pred_GEC p (a_title,a) pSt
		=	pSt3
		where
			(aStore,pSt1)	=	openStoreId pSt
			(_,pSt2)		=	openStore aStore (Just a) pSt1
			(dosetF,_,pSt3)	=	createGEC a_title Interactive a testset pSt2

			testset r na  
			= if (p na) savenew takeold
			where
				takeold pSt	
				# (oa,pSt)	=	readStore aStore pSt
				= dosetF oa pSt
				
				savenew pSt = writeStore aStore na pSt


mutual_GEC 	:: !a !(!String,b -> a) !(!String,a -> b) !*(PSt .ps) -> *(PSt .ps) | bimap{|*|} ps & gGEC{|*|} a & gGEC{|*|} b 
mutual_GEC a (g_title,g) (f_title,f) pSt
		=	pSt2
		where
			(dosetF,setf,pSt1)	=	createGEC f_title Interactive (f a)     (\r b pst -> dosetG (g b) pst) pSt
			(dosetG,setg,pSt2)	=	createGEC g_title Interactive (g (f a)) (\r a pst -> dosetF (f a) pst) pSt1


selfStateGEC:: !(a s -> (a,s)) !(!String,!a) s !*(PSt .ps) -> *(PSt .ps) | bimap{|*|} ps & gGEC{|*|} a 
selfStateGEC f (a_title,a) s pSt
	=	pSt3
	where
		(sStore,pSt1)		=	openStoreId pSt
		(_,pSt2)			=	openStore sStore (Just s) pSt1
		(dosetF,_,pSt3)		=	createGEC a_title Interactive a (newA dosetF) pSt2

		newA dosetF r a pSt
		#	(s,pSt)			=	readStore sStore pSt
		#	(na,ns)			=	f a s
		#	pSt				=	writeStore sStore ns pSt
		=	dosetF na pSt	

selfStateGECps:: !(a s *(PSt .ps) -> (a,s,*(PSt .ps))) 	!(!String,!a) s	!*(PSt .ps) -> *(PSt .ps) | bimap{|*|} ps & gGEC{|*|} a 
selfStateGECps f (a_title,a) s pSt	=	pSt3
	where
		(sStore,pSt1)		=	openStoreId pSt
		(_,pSt2)			=	openStore sStore (Just s) pSt1
		(dosetF,_,pSt3)		=	createGEC a_title Interactive a (newA dosetF) pSt2

		newA dosetF r a pSt
		#	(s,pSt)			=	readStore sStore pSt
		#	(na,ns,pSt)		=	f a s pSt
		#	pSt				=	writeStore sStore ns pSt
		=	dosetF na pSt	

timer_GEC 	:: !((!Int,!a) -> (!Int,!a)) !(!String,(!Int,!a)) !*(PSt .ps) -> *(PSt .ps) | bimap{|*|} ps & gGEC{|*|} a 
timer_GEC f (a_title,(i,a)) pSt
# (tid,pSt) 		= openId pSt
# (handlegec,pSt)	= createNGEC "a_title" OutputOnly (a,Hide i) (\_ _ pst -> pst) pSt
# pSt				= snd (openTimer Void (timer tid handlegec) pSt)
= pSt
where
	timer tid handlegec = (Timer i NilLS [TimerId tid, TimerFunction (timeout tid handlegec)])

	timeout tid handlegec _ (lSt,pSt)
	# ((a,Hide i),pSt) = handlegec.gecGetValue pSt
	# (ni,na) = f (i,a)
	# pst = accPIO (setTimerInterval tid ni) pSt
	= (lSt,handlegec.gecSetValue NoUpdate (na,Hide ni) pSt)


//	createGEC to store "a", given a call back that tells that the GEC might have changed, returning
//	a function to store a new value in the GEC    causing no call back call
//	a function to store a new value in the GEC do causing a call back call

createNGEC :: String OutputOnly a (Update a (PSt .ps)) *(PSt .ps) -> *((GECVALUE a *(PSt .ps)),*(PSt .ps)) | gGEC{|*|} a & bimap{|*|} ps
createNGEC title outputOnly initval userUpdate pSt
	# (objId,pSt)	= openOBJECTControlId pSt
	= case accPIO (searchWindowIdWithTitle title) pSt of
		(Nothing,pSt)
			#	(id,pSt)	= openId pSt
			#	(_,pSt)		= openWindow undef 
								(Window title NilLS 
											[	WindowId id
											, 	WindowViewSize {w=800,h=800}
											, 	WindowLook     True stdUnfillNewFrameLook
											, 	WindowPen      [PenBack defWindowBackColour]
											,	WindowHScroll  (stdScrollFunction Horizontal 20)
											,	WindowVScroll  (stdScrollFunction Horizontal 20)
											,	WindowViewDomain {zero & corner2={x=800,y=800}}
											]) pSt
			#	guiLoc		= {guiId=id,guiItemPos=(Fix,OffsetVector {vx=hOffset,vy=vOffset})}
			# 	(setA,pSt)	= openGECVALUE (guiLoc,objId) outputOnly (Just initval) (why_changed id) pSt
			#	pSt			= setA.gecOpenGUI (guiLoc,objId) pSt
			= 	(setA,pSt)
		(Just id,pSt)
			#	guiLoc		= {guiId=id,guiItemPos=(BelowPrev,OffsetVector {zero & vy=vOffset})}
			# 	(setA,pSt)	= openGECVALUE (guiLoc,objId) outputOnly (Just initval) (why_changed id) pSt
			#	pSt			= setA.gecOpenGUI (guiLoc,objId) pSt
			= 	(setA,pSt)
where
	why_changed wId reason t pSt = adjustViewDomain wId (userUpdate reason t pSt)
	
	hOffset		= 10		// The horizontal margin 
	vOffset		= 10		// The vertical distance between the GUIs of two subsequent GECs
	
	adjustViewDomain wId pSt
		= case accPIO (getWindow wId) pSt of
			(Just wSt,pSt)
				# topLevelIds	= [fromJust mId \\ (_,mId)<-getControlTypes wSt | isJust mId]
				# topLevelSizes	= map (\id -> (\{w,h} -> (w,h)) (snd (getControlOuterSize id wSt))) topLevelIds
				# (ws,hs)		= unzip topLevelSizes
				# (w, h)		= (maxList [0:ws] + hOffset,sum hs + (length ws)*vOffset)
				# pSt			= appPIO (setWindowViewDomain wId {zero & corner2={x=w,y=h}}) pSt
				= pSt
			(Nothing, pSt)
				= abort "Could not access WState from window."

	searchWindowIdWithTitle :: String (IOSt .ps) -> (Maybe Id,IOSt .ps)
	searchWindowIdWithTitle title ioSt
		# (id_types,ioSt)		= getWindowStack ioSt
		# ids					= map fst id_types
		# (maybe_titles,ioSt)	= seqList (map getWindowTitle ids) ioSt
		# titles				= map fromJust maybe_titles
		# title_index			= titles ?? title
		| title_index < 0		// title does not occur
			= (Nothing,ioSt)
		| otherwise
			= (Just (ids !! title_index),ioSt)

	(??) infixl 9 :: ![a] a -> Int | == a 
	(??) list x 
	    = searchIndex list 0 x 
	where 
	    searchIndex :: ![a] !Int a -> Int | == a 
	    searchIndex [] _ _ 
	        = -1 
	    searchIndex [x:xs] i y 
	        | x==y      = i 
	        | otherwise = searchIndex xs (i+1) y

createGEC :: String OutputOnly a (Update a (PSt .ps)) *(PSt .ps) -> *(PState a .ps,PState a .ps,*(PSt .ps)) | gGEC{|*|} a & bimap{|*|} ps
createGEC title outputOnly initval userUpdate pSt
	# (setA,pSt) = createNGEC title outputOnly initval userUpdate pSt
	= 	(setA.gecSetValue NoUpdate,setA.gecSetValue YesUpdate,pSt)
		


