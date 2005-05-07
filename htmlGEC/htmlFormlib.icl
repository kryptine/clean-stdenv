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

// Place second body below first

(<||>) infixl 4	:: [BodyTag] [BodyTag] -> BodyTag	// Place a above b
(<||>) b1 b2 =  STable [Tbl_CellPadding (Pixels 0), Tbl_CellSpacing (Pixels 0)] [[BodyTag b1],[BodyTag b2]]

mkSTable :: [[BodyTag]] -> BodyTag
mkSTable table
= Table []	(mktable table)
where
	mktable table 	= [Tr [] (mkrow rows) \\ rows <- table]	
	mkrow rows 		= [Td [Td_VAlign Alo_Top, Td_Width (Pixels defpixel)] [row] \\ row <- rows] 

// Form collection:

counterForm 	:: !FormId !Mode a !*HSt -> (Form a,!*HSt) | +, -, one,  gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} a
counterForm name mode i hst = mkViewForm name mode bimap i hst
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


horlist2Form 		:: !FormId !Mode a ![a] 	!*HSt -> (Form [a],!*HSt) 	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} a
horlist2Form s mode defaultval list hst 
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
# (nx, hst) = mkEditForm (s +++ toString (length xs)) mode x hst
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
# (nx, hst) = mkEditForm (s +++ toString (length xs)) mode x hst
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
FuncBut init i s mode (button=:LButton _ name ,cbf) hst = mkViewForm (s +++ name +++ toString i) mode bimap id hst
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
FuncBut init i s mode (button=:PButton _ ref ,cbf) hst = mkViewForm (s +++ toString i +++ ref) mode bimap id hst
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
	
		FuncCheckBox init s mode (checkbox,cbf) hst = mkViewForm s` mode bimap (\_ a -> a,False) hst
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
# (ni,hst)		= mkStoreForm s (set i) (set (abs i) 0) hst	// determine which radio to select

# (radio,hst) 	= ListFuncRadio` ni.value 0 s mode defs hst	// determine if radio changed by user
# (f,nni)		= radio.value
# (f,i) 		= if (nni>=0) (f nni, nni) (id,ni.value)		// if so, select function, otherwise set to old radio
# (i,hst)		= mkStoreForm s (set i) (set i i) hst	// store current selected radio for next round
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
	
		FuncRadio i j s mode cbf hst = mkViewForm s` mode bimap (\_ a -> a,-1) hst
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
FuncMenu index s mode defs hst = mkViewForm s mode bimap (id,init index) hst
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
# (nx, hst) = mkEditForm (s +++ toString (length xs)) mode x hst
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
	
