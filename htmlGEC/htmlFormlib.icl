implementation module htmlFormlib

// Handy collection of Form's
// Form library similar to the AGEC lib
// (c) MJP 2005

import StdEnv
import StdHtml

// operators for lay-out of html bodys ...

// Place two bodies next to each other

(<=>) infixl 5 :: [BodyTag] [BodyTag] -> BodyTag
(<=>) b1 b2 =  STable [Tbl_CellPadding (Pixels 0), Tbl_CellSpacing (Pixels 0)] [[BodyTag b1,BodyTag b2]]

(<.=.>) infixl 5 :: BodyTag BodyTag -> BodyTag
(<.=.>) b1 b2 =  STable [Tbl_CellPadding (Pixels 0), Tbl_CellSpacing (Pixels 0)] [[b1,b2]]


// Place second body below first

(<||>) infixl 4	:: [BodyTag] [BodyTag] -> BodyTag	// Place a above b
(<||>) b1 b2 =  STable [Tbl_CellPadding (Pixels 0), Tbl_CellSpacing (Pixels 0)] [[BodyTag b1],[BodyTag b2]]

(<.||.>) infixl 4	:: BodyTag BodyTag -> BodyTag	// Place a above b
(<.||.>) b1 b2 =  STable [Tbl_CellPadding (Pixels 0), Tbl_CellSpacing (Pixels 0)] [[b1],[b2]]


// row and column making

mkColForm :: [BodyTag] -> BodyTag
mkColForm xs 	= foldr (<.||.>) EmptyBody xs

mkRowForm :: [BodyTag] -> BodyTag
mkRowForm xs 	= foldr (<.=.>) EmptyBody xs


mkSTable :: [[BodyTag]] -> BodyTag
mkSTable table
= Table []	(mktable table)
where
	mktable table 	= [Tr [] (mkrow rows) \\ rows <- table]	
	mkrow rows 		= [Td [Td_VAlign Alo_Top, Td_Width (Pixels defpixel)] [row] \\ row <- rows] 

// Form collection:

counterForm 	:: !FormId !Mode a !*HSt -> (Form a,!*HSt) | +, -, one,  gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} a
counterForm name mode i hst = mkViewForm name i mode bimap hst
where
	bimap =	{ toForm 	= \n v -> case v of
									Nothing -> (n,down,up)
									Just v -> v
			, updForm	= updCounter`
			, fromForm	= \_ (n,_,_) -> n
			, resetForm = Nothing
			}

	updCounter` True val = updCounter val
	updCounter` _ val = val

	updCounter (n,Pressed,_)  = (n - one,down,up)
	updCounter (n,_,Pressed) 	= (n + one,down,up)
	updCounter else 			= else

	up 		= LButton (defpixel / 6) "+"
	down	= LButton (defpixel / 6) "-"


browseButtons :: !Bool !Int !Int !Int !Int !FormId !Mode !*HSt -> (Form Int,!*HSt)
browseButtons reset curindex step length nbuttuns formid mode hst
# (nindex, hst)		= mkStoreForm formid curindex (\v -> if reset curindex v)  hst
# (calcnext, hst)	= browserForm nindex.value hst
# (nindex, hst)		= mkStoreForm formid curindex calcnext.value  hst
# (shownext, hst)	= browserForm nindex.value hst
= ({changed = calcnext.changed
   ,value	= nindex.value
   ,body	= shownext.body},hst)
where
	browserForm :: !Int *HSt -> (Form (Int -> Int),!*HSt) 
	browserForm index hst
		= ListFuncBut2 False formid (browserButtons index step length) hst
	where
		browserButtons :: !Int !Int !Int -> [(Mode,Button,Int -> Int)]
		browserButtons init step length = 
			if (init - range >= 0) 	   [(mode,sbut "--", set (init - range))] [] 
			++
			take nbuttuns [(setmode i index,sbut (toString (i)),set i) \\ i <- [startval,startval+step .. length-1]] 
			++ 
			if (startval + range < length - 1) [(mode,sbut "++", set (startval + range))] []
		where
			set j i 	= j
			range 		= nbuttuns * step
			start i j	= if (i < range) j (start (i-range) (j+range))
			startval 	= start init 0
			sbut s		= LButton (defpixel/3) s
			setmode i index
			| index <= i && i < index + step = Display
			| otherwise = mode

horlist2Form 		:: !FormId a !Mode ![a] 	!*HSt -> (Form [a],!*HSt) 	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} a
horlist2Form s defaultval mode  list hst 
# (fun,hst) 	= TableFuncBut  s mode [[(but "-", less),(but "+", more)]] hst
# (nlist,hst) 	= horlistForm s mode (fun.value list) hst  
= ({changed	= fun.changed || nlist.changed
   ,value	= nlist.value
   ,body	= [fun.body <||> nlist.body]
   },hst)
where
	but s =  LButton (defpixel / 6) s

	more []   = [defaultval]
	more list = list ++ [last list]

	less [x:xs] = xs
	less [] = []

horlistForm 		:: !FormId !Mode ![a] 		!*HSt -> (Form [a],!*HSt) 	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} a
horlistForm s mode [] hst  
= ({changed	= False
   ,value	= []
   ,body	= []
   },hst)
horlistForm s mode [x:xs] hst
# (nxs,hst) = horlistForm s mode xs hst
# (nx, hst) = mkEditForm (s +++ toString (length xs)) x mode hst
= ({changed	= nxs.changed || nx.changed 
   ,value	= [nx.value:nxs.value]
   ,body	= [nx.body <=> nxs.body]
   },hst)
	
vertlistForm 		:: !FormId !Mode ![a] 		!*HSt -> (Form [a],!*HSt) 	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} a
vertlistForm s mode [] hst  
= ({changed	= False
   ,value	= []
   ,body	= []
   },hst)
vertlistForm s mode [x:xs] hst
# (nxs,hst) = vertlistForm s mode xs hst
# (nx, hst) = mkEditForm (s +++ toString (length xs)) x mode hst
= ({changed	= nxs.changed || nx.changed 
   ,value	= [nx.value:nxs.value]
   ,body	= [nx.body <||> nxs.body]
   },hst)

table_hv_Form 		:: !FormId !Mode ![[a]] 	!*HSt -> (Form [[a]],!*HSt) 	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} a
table_hv_Form s mode [] hst
= ({changed	= False
   ,value	= []
   ,body	= []
   },hst)
table_hv_Form s mode [x:xs] hst
# (nxs,hst)	= table_hv_Form s mode xs hst
# (nx, hst)	= horlistForm (s +++ toString (length xs)) mode x hst
= ({changed	= nxs.changed || nx.changed 
   ,value	= [nx.value:nxs.value]
   ,body	= [nx.body <||> nxs.body]
   },hst)
   
t2EditForm  :: !FormId !Mode !(a,b) !*HSt -> ((Form a,Form b),!*HSt) |  gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} a
																	 &  gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} b
t2EditForm formid mode (a,b) hst
# (forma,hst) = mkEditForm (formid +++ "t21") a mode hst 
# (formb,hst) = mkEditForm (formid +++ "t22") b mode hst
= ((forma,formb),hst) 

t3EditForm  :: !FormId !Mode !(a,b,c) !*HSt -> ((Form a,Form b,Form c),!*HSt) |  gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} a
																	   &  gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} b
																	   &  gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} c
t3EditForm formid mode (a,b,c) hst
# (forma,hst) = mkEditForm (formid +++ "t31") a mode hst 
# (formb,hst) = mkEditForm (formid +++ "t32") b mode hst
# (formc,hst) = mkEditForm (formid +++ "t33") c mode hst
= ((forma,formb,formc),hst) 

t4EditForm  :: !FormId !Mode !(a,b,c,d) !*HSt -> ((Form a,Form b,Form c,Form d),!*HSt) |  gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} a
																	   &  gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} b
																	   &  gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} c
																	   &  gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} d
t4EditForm formid mode (a,b,c,d) hst
# (forma,hst) = mkEditForm (formid +++ "t41") a mode hst 
# (formb,hst) = mkEditForm (formid +++ "t42") b mode hst
# (formc,hst) = mkEditForm (formid +++ "t43") c mode hst
# (formd,hst) = mkEditForm (formid +++ "t44") d mode hst
= ((forma,formb,formc,formd),hst) 

   
TableFuncBut 		:: !FormId !Mode ![[(Button, a -> a)]] !*HSt 
													  -> (Form (a -> a) ,!*HSt)
TableFuncBut s mode list hst = TableFuncBut` 0 s mode list hst
where
	TableFuncBut` n s mode [] hst
	= ({changed	= False
	   ,value	= id
	   ,body	= []
	   },hst)

	TableFuncBut` n s mode [row:rows] hst 
	# (rows,hst)	= TableFuncBut` n s mode rows hst
	# (row,  hst)	= RowFuncBut` n s mode row hst
	= ({changed		= rows.changed || row.changed 
	   ,value		= rows.value o row.value
	   ,body		= [row.body <||> rows.body]
	   },hst)
	where
		RowFuncBut` :: !Int !FormId !Mode [(Button, a -> a)] !*HSt -> (Form (a -> a),!*HSt)
		RowFuncBut` n s mode [] hst 
		= ({changed	= False
		   ,value	= id
		   ,body	= []
		   },hst)
		RowFuncBut` n s mode [x:xs] hst 
		# (rowfun,hst) 	= RowFuncBut` n s mode xs hst
		# (fun   ,hst)	= FuncBut False n s mode x hst
		= ({changed		= rowfun.changed || fun.changed 
		   ,value		= rowfun.value o fun.value
		   ,body		= [fun.body <=> rowfun.body]
		   },hst)


FuncBut :: !Bool !Int !FormId !Mode !(Button, a -> a) !*HSt -> (Form (a -> a),!*HSt)
FuncBut init i s mode (button=:LButton _ name ,cbf) hst = mkViewForm (s +++ name +++ toString i) id mode bimap hst
where
	bimap =	{ toForm 	= \_ v -> case v of
									Nothing = button
									(Just v) = if init button v
			, updForm	= \_ v -> v
			, fromForm	= \_ but -> case but of 
									Pressed  -> cbf
									_		 -> id
			, resetForm	= Just (\_ 	-> button)
			}
FuncBut init i s mode (button=:PButton _ ref ,cbf) hst = mkViewForm (s +++ toString i +++ ref) id mode bimap hst
where
	bimap =	{ toForm 	= \_ v -> case v of
									Nothing = button
									(Just v) = if init button v
			, updForm	= \_ v -> v
			, fromForm	= \_ but -> case but of 
									Pressed -> cbf
									_		  -> id
			, resetForm	= Just (\_ 	-> button)
			}
FuncBut init i s mode (Pressed,cbf) hst = FuncBut init i s mode (LButton 10 "??",cbf) hst

ListFuncBut :: !Bool !FormId !Mode [(Button, a -> a)] !*HSt -> (Form (a -> a),!*HSt)
ListFuncBut b s mode list hst = ListFuncBut` b 0 s mode list hst 
where
	ListFuncBut` b n s mode [] hst
	= ({changed	= False
	   ,value	= id
	   ,body	= []
	   },hst)
	ListFuncBut` b n s mode [x:xs] hst 
	# (rowfun,hst) 	= ListFuncBut` b (n+1) s mode xs hst
	# (fun   ,hst)	= FuncBut b n s mode x hst
	= ({changed	= rowfun.changed || fun.changed
	   ,value	= fun.value o rowfun.value
	   ,body	= [BodyTag fun.body:rowfun.body]
	   },hst)

ListFuncBut2 :: !Bool !FormId [(Mode,Button, a -> a)] !*HSt -> (Form (a -> a),!*HSt)
ListFuncBut2 b s list hst = ListFuncBut` b 0 s list hst 
where
	ListFuncBut` b n s [] hst
	= ({changed	= False
	   ,value	= id
	   ,body	= []
	   },hst)
	ListFuncBut` b n s [(mode,but,func):xs] hst 
	# (rowfun,hst) 	= ListFuncBut` b (n+1) s xs hst
	# (fun   ,hst)	= FuncBut b n s mode (but,func) hst
	= ({changed	= rowfun.changed || fun.changed
	   ,value	= fun.value o rowfun.value
	   ,body	= [BodyTag fun.body:rowfun.body]
	   },hst)


ListFuncCheckBox :: !Bool !FormId !Mode [(CheckBox, Bool [Bool] a -> a)] !*HSt 
									-> (Form (a -> a,[Bool]),!*HSt)
ListFuncCheckBox init s mode defs hst 
# (check,hst) = ListFuncCheckBox` init s mode defs hst
# (f,bools) = check.value
= ({changed	= False
   ,value	= (f bools,bools)
   ,body	= check.body
   },hst)
where
	ListFuncCheckBox` :: !Bool !FormId !Mode [(CheckBox, Bool [Bool] a -> a)] !*HSt 
										-> (Form ([Bool] a -> a,[Bool]),!*HSt)
	ListFuncCheckBox` init s mode [] hst
	= ({changed	= False
	   ,value	= (\_ a -> a,[])
	   ,body	= []
	   },hst)
	ListFuncCheckBox` init s mode [x:xs] hst 
	# (rowfun,hst) 	= ListFuncCheckBox` init s mode xs hst
	# (fun   ,hst)	= FuncCheckBox init s mode x hst
	# (rowfunv,boolsv) = rowfun.value
	# (funv,nboolv) = fun.value
	= ({changed	= rowfun.changed || fun.changed
	   ,value	= (funcomp funv rowfunv,[nboolv:boolsv])
	   ,body	= [BodyTag fun.body:rowfun.body]
	   },hst)
	where
		funcomp f g = \bools a = f bools (g bools a)
	
		FuncCheckBox init s mode (checkbox,cbf) hst = mkViewForm s` (\_ a -> a,False) mode bimap hst
		where
			bimap =	{ toForm 	= \_ v -> case v of
											Nothing = checkbox
											(Just v) = if init checkbox v
					, updForm	= \b v -> if (not init && b) (toggle v) v
					, fromForm	= \b v -> if b ((docbf  v),toBool v) (\_ a -> a,toBool v)
					, resetForm	= Nothing
					}
		
			docbf (CBChecked name) 		= cbf True
			docbf (CBNotChecked name) 	= cbf False
		
			toggle (CBChecked name) 	= CBNotChecked name
			toggle (CBNotChecked name) 	= CBChecked name
		
			s` = s +++ case checkbox of 
							(CBChecked name) = name
							(CBNotChecked name) = name


// the radio buttons implementation is currently more complicated than strictly necessary
// browsers demand the same name to be used for every member in the radio group
// the current implementation requires different names
// we therefore ourselves have to determine and remember which radio button in the family is set


ListFuncRadio :: !Int !FormId !Mode [Int a -> a] !*HSt 
									-> (Form (a -> a,Int),!*HSt)
ListFuncRadio i s mode defs hst 
# (ni,hst)		= mkStoreForm s (set (abs i) 0) (set i)  hst	// determine which radio to select

# (radio,hst) 	= ListFuncRadio` ni.value 0 s mode defs hst	// determine if radio changed by user
# (f,nni)		= radio.value
# (f,i) 		= if (nni>=0) (f nni, nni) (id,ni.value)		// if so, select function, otherwise set to old radio
# (i,hst)		= mkStoreForm s (set i i) (set i)  hst	// store current selected radio for next round
= ({changed	= ni.changed || radio.changed
   ,value	= (f,i.value)
   ,body	= radio.body
   },hst)
where
	radio i j 
	| i == j 	= RBChecked s
	| otherwise = RBNotChecked s

	set i j 
	| i >= 0 && i < length defs = i		// set to new radio buttun 
	| otherwise = j						// set to old radio button

	ListFuncRadio` :: !Int !Int !FormId !Mode [Int a -> a] !*HSt 
										-> (Form (Int a -> a,Int),!*HSt)
	ListFuncRadio` i j s mode [] hst
	= ({changed	= False
	   ,value	= (\_ a -> a,-1)
	   ,body	= []
	   },hst)
	ListFuncRadio` i j s mode [f:fs] hst 
	# (listradio,hst) 	= ListFuncRadio` i (j+1) s mode fs hst
	# (funcradio,hst)	= FuncRadio i j s mode f hst
	# (rowfun,rri) 		= listradio.value
	# (fun,ri) 			= funcradio.value
	= ({changed	= listradio.changed || funcradio.changed
	   ,value	= (funcomp fun rowfun,max ri rri)
	   ,body	= [BodyTag funcradio.body:listradio.body]
	   },hst)


	where
		funcomp f g = \i a = f i (g i a)
	
		FuncRadio i j s mode cbf hst = mkViewForm s` (\_ a -> a,-1) mode bimap  hst
		where
			bimap =	{ toForm 	= \_ v -> radio i j
					, updForm	= \b v -> if b (RBChecked s) (otherradio v)
					, fromForm	= \b v -> if b (cbf,j) (\_ a -> a,-1)
					, resetForm	= Nothing
					}
			s` = s +++ "_" +++ toString j

			otherradio v
			| StrippedCheckUpdateId == s = RBNotChecked s
			| otherwise = v

FuncMenu :: !Int !FormId !Mode [(String, a -> a)] !*HSt 
													 -> (Form (a -> a,Int),!*HSt)
FuncMenu index s mode defs hst = mkViewForm s (id,init index) mode bimap  hst
where
	menulist = PullDown (1,defpixel) (init index,map fst defs) 

	bimap =	{ toForm 	= toForm
			, updForm	= \b v -> v
			, fromForm	= \b v=:(PullDown _ (nindex,_)) -> if b (snd (defs!!nindex),nindex) (id,nindex)
			, resetForm	= Nothing
			}
	toForm _ Nothing 		= menulist
	toForm _ (Just oldlist) = if (index >= 0) menulist oldlist

	init index
	| abs index >= 0 && abs index < length defs = abs index
	| otherwise = 0


listForm 			:: !FormId !Mode ![a] 		!*HSt -> (Form [a],!*HSt) 	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} a
listForm s mode [] hst
= ({changed	= False
   ,value	= []
   ,body	= []
   },hst)
listForm s mode [x:xs] hst
# (nxs,hst) = listForm s mode xs hst
# (nx, hst) = mkEditForm (s +++ toString (length xs)) x mode hst
= ({changed	= nx.changed || nxs.changed
   ,value	= [nx.value:nxs.value]
   ,body	= [BodyTag nx.body:nxs.body]
   },hst)


// scripts

openWindowScript ::  !String !Int !Int !Bool !Bool !Bool !Bool !Bool !Bool !Html -> Script
openWindowScript scriptname height width toolbar menubar scrollbars resizable location status html
= FScript( \file -> file <+
			"function " <+ scriptname <+ "\r" <+
			"{ OpenWindow=window.open(\"\", \"newwin\", \"" <+
					"height=" <+ height <+
					",width=" <+ width <+
					",toolbar=" <+ yn toolbar <+
					",menubar=" <+ yn menubar <+
					",scrollbars=" <+ yn scrollbars <+
					",resizable=" <+ yn resizable <+
					",location=" <+ yn location <+
					",status=" <+ yn status <+ "\");\r" <+
				"OpenWindow.document.write(\"" <+ html <+ "</HTML>\");\r" <+
				"OpenWindow.document.close();\r" <+
	//			"self.name=\"webshop.php\";\r" <+     // don't know whther is needed ???
			"}")
where
	yn bool = if bool "yes" "no" 

openNoticeScript ::  !String !Int !Int !Html -> Script
openNoticeScript scriptname height width html 
	= openWindowScript scriptname height width False False False False False False html

// special objects ...

mediaPlayer:: (Int,Int) Bool String -> BodyTag
mediaPlayer (height,width) autostart filename
	= Body_Object 
		[ Oba_ClassId "CLSID:05589FA1-C356-11CE-BF01-00AA0055595A"
		, Oba_Height height
		, Oba_Width width
		] 
		[ Param [ Pam_Name "FileName", Pam_Value (SV filename) ]
		, Param [ Pam_Name "autostart", Pam_Value (SV (toString autostart)) ]
		]
	
