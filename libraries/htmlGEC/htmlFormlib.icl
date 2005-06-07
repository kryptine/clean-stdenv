implementation module htmlFormlib

// Handy collection of Form's
// (c) MJP 2005

import StdEnv, StdHtml

// utility for creating FormId's

pFormId :: String -> FormId					// persitent formid
pFormId s = {id = s, lifespan = Persistent, mode = Edit}

sFormId :: String -> FormId					// session formid
sFormId s = {id = s, lifespan = Session, mode = Edit}

nFormId :: String -> FormId					// page formid
nFormId s = {id = s, lifespan = Page, mode = Edit}

pdFormId :: String -> FormId					// persitent formid
pdFormId s = {id = s, lifespan = Persistent, mode = Display}

sdFormId :: String -> FormId					// session formid
sdFormId s = {id = s, lifespan = Session, mode = Display}

ndFormId :: String -> FormId					// page formid
ndFormId s = {id = s, lifespan = Page, mode = Display}

// easy creation of an html page

mkHtml:: String [BodyTag] *HSt ->  (Html,*HSt)
mkHtml s tags hst 	= (Html (header s) (body tags),hst)
where
	header s 			= Head [`Hd_Std [Std_Title s]] [] 
	body tags 			= Body [] tags

// frequently used variants of mkViewForm

toFormid d Nothing = d
toFormid d (Just v) = v

mkEditForm:: !FormId d !*HSt -> (Form d,!*HSt) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC d
mkEditForm formid=:{mode = Edit} data hst
= mkViewForm formid data
	{toForm = toFormid , updForm = \_ v -> v , fromForm = \_ v -> v , resetForm = Nothing}  hst
mkEditForm formid  data hst
= mkSetForm formid data hst

mkSetForm:: !FormId d !*HSt -> (Form d,!*HSt) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC d
mkSetForm formid data hst
= mkViewForm formid data 
	{toForm = toFormid , updForm = \_ _ -> data , fromForm = \_ v -> v , resetForm = Nothing}  hst

mkSelfForm  :: !FormId 	d !(d -> d) !*HSt -> (Form d,!*HSt) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC d
mkSelfForm formid initdata cbf  hst
= mkViewForm formid initdata 
	{toForm = toFormid , updForm = update , fromForm = \_ v -> v , resetForm = Nothing}  hst
where
	update True newval = cbf newval
	update _ val = val
	
mkSelf2Form :: !FormId d !(d -> d) !*HSt -> (Form d,!*HSt) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC d
mkSelf2Form formid data cbf  hst
= mkViewForm formid data 
	{toForm = toFormid , updForm = \b v = cbf v , fromForm = \_ v -> v, resetForm = Nothing}  hst
where
	toFormid d Nothing = d
	toFormid d (Just v) = v

mkApplyForm :: !FormId d !(d -> d) !*HSt -> (Form d,!*HSt) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC d
mkApplyForm formid data cbf  hst
= mkViewForm formid data 
	{toForm = toFormid , updForm = \_ _ = cbf data , fromForm = \_ v -> v, resetForm = Nothing}  hst

mkStoreForm :: !FormId d !(d -> d) !*HSt -> (Form d,!*HSt) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC d
mkStoreForm formid data cbf  hst
= mkViewForm formid data 
	{toForm = toFormid , updForm = \_ v = cbf v , fromForm = \_ v -> v, resetForm = Nothing}  hst

mkApplyEditForm	:: !FormId d !d !*HSt -> (Form d,!*HSt) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC d
mkApplyEditForm formid initval inputval  hst
= mkViewForm formid initval 
	{toForm =  toFormid , updForm = update , fromForm = \_ v -> v, resetForm = Nothing}  hst
where
	update True  newval = newval
	update False val    = inputval

mkBimapEditor :: !FormId d !(Bimap d v) !*HSt -> (Form d,!*HSt) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC v
mkBimapEditor formid d {map_to,map_from} hst
= mkViewForm formid d 	{ toForm 	= \d v -> case v of 
												Nothing -> map_to d
												Just v -> v
						, updForm 	= \b v -> v
						, fromForm 	= \b v -> map_from v
						, resetForm = Nothing
						} hst 

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

counterForm 	:: !FormId !a !*HSt -> (Form a,!*HSt) | +, -, one,  gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
counterForm name i hst = mkViewForm name i bimap hst
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


browseButtons :: !Bool !Int !Int !Int !Int !FormId !*HSt -> (Form Int,!*HSt)
browseButtons reset curindex step length nbuttuns formid hst
# (nindex, hst)		= mkStoreForm formid curindex (\v -> if reset curindex v)  hst
# (calcnext, hst)	= browserForm nindex.value hst
# (nindex, hst)		= mkStoreForm formid curindex calcnext.value  hst
# (shownext, hst)	= browserForm nindex.value hst
= ({changed = calcnext.changed
   ,value	= nindex.value
   ,form	= shownext.form},hst)
where
	browserForm :: !Int *HSt -> (Form (Int -> Int),!*HSt) 
	browserForm index hst
		= ListFuncBut2 False formid (browserButtons index step length) hst
	where
		browserButtons :: !Int !Int !Int -> [(Mode,Button,Int -> Int)]
		browserButtons init step length = 
			if (init - range >= 0) 	   [(formid.mode,sbut "--", set (init - range))] [] 
			++
			take nbuttuns [(setmode i index,sbut (toString (i)),set i) \\ i <- [startval,startval+step .. length-1]] 
			++ 
			if (startval + range < length - 1) [(formid.mode,sbut "++", set (startval + range))] []
		where
			set j i 	= j
			range 		= nbuttuns * step
			start i j	= if (i < range) j (start (i-range) (j+range))
			startval 	= start init 0
			sbut s		= LButton (defpixel/3) s
			setmode i index
			| index <= i && i < index + step = Display
			| otherwise = formid.mode

horlist2Form 		:: !FormId a ![a] 	!*HSt -> (Form [a],!*HSt) 	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
horlist2Form s defaultval list hst 
# (fun,hst) 	= TableFuncBut  s [[(but "-", less),(but "+", more)]] hst
# (nlist,hst) 	= horlistForm s (fun.value list) hst  
= ({changed	= fun.changed || nlist.changed
   ,value	= nlist.value
   ,form	= [fun.form <||> nlist.form]
   },hst)
where
	but s =  LButton (defpixel / 6) s

	more []   = [defaultval]
	more list = list ++ [last list]

	less [x:xs] = xs
	less [] = []

horlistForm 		:: !FormId ![a] 		!*HSt -> (Form [a],!*HSt) 	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
horlistForm _  [] hst  
= ({changed	= False
   ,value	= []
   ,form	= []
   },hst)
horlistForm formid [x:xs] hst
# (nxs,hst) = horlistForm formid xs hst
# (nx, hst) = mkEditForm nformid x hst
= ({changed	= nxs.changed || nx.changed 
   ,value	= [nx.value:nxs.value]
   ,form	= [nx.form <=> nxs.form]
   },hst)
where
	nformid = {formid & id = formid.id +++ toString (length xs)}
			
vertlistForm 		:: !FormId ![a] 		!*HSt -> (Form [a],!*HSt) 	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
vertlistForm _ [] hst  
= ({changed	= False
   ,value	= []
   ,form	= []
   },hst)
vertlistForm formid [x:xs] hst
# (nxs,hst) = vertlistForm formid xs hst
# (nx, hst) = mkEditForm nformid x hst
= ({changed	= nxs.changed || nx.changed 
   ,value	= [nx.value:nxs.value]
   ,form	= [nx.form <||> nxs.form]
   },hst)
where
	nformid = {formid & id = formid.id +++ toString (length xs)}

table_hv_Form 		:: !FormId ![[a]] 	!*HSt -> (Form [[a]],!*HSt) 	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
table_hv_Form _ [] hst
= ({changed	= False
   ,value	= []
   ,form	= []
   },hst)
table_hv_Form formid [x:xs] hst
# (nxs,hst)	= table_hv_Form formid xs hst
# (nx, hst)	= horlistForm nformid x hst
= ({changed	= nxs.changed || nx.changed 
   ,value	= [nx.value:nxs.value]
   ,form	= [nx.form <||> nxs.form]
   },hst)
where
	nformid = {formid & id = formid.id +++ toString (length xs)}
   
t2EditForm  :: !FormId !(a,b) !*HSt -> ((Form a,Form b),!*HSt) |  gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
																	 &  gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC b
t2EditForm formid (a,b) hst
# (forma,hst) = mkEditForm nformida a hst 
# (formb,hst) = mkEditForm nformidb b hst
= ((forma,formb),hst) 
where
	nformida = {formid & id = formid.id +++ "t21"}
	nformidb = {formid & id = formid.id +++ "t22"}

t3EditForm  :: !FormId !(a,b,c) !*HSt -> ((Form a,Form b,Form c),!*HSt) |  gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
																	   &  gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC b
																	   &  gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC c
t3EditForm formid (a,b,c) hst
# (forma,hst) = mkEditForm nformida a hst 
# (formb,hst) = mkEditForm nformidb b hst
# (formc,hst) = mkEditForm nformidc c hst
= ((forma,formb,formc),hst) 
where
	nformida = {formid & id = formid.id +++ "t31"}
	nformidb = {formid & id = formid.id +++ "t32"}
	nformidc = {formid & id = formid.id +++ "t33"}

t4EditForm  :: !FormId !(a,b,c,d) !*HSt -> ((Form a,Form b,Form c,Form d),!*HSt) |  gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
																	   &  gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC b
																	   &  gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC c
																	   &  gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC d
t4EditForm formid (a,b,c,d) hst
# (forma,hst) = mkEditForm nformida a hst 
# (formb,hst) = mkEditForm nformidb b hst
# (formc,hst) = mkEditForm nformidc c hst
# (formd,hst) = mkEditForm nformidd d hst
= ((forma,formb,formc,formd),hst) 
where
	nformida = {formid & id = formid.id +++ "t41"}
	nformidb = {formid & id = formid.id +++ "t42"}
	nformidc = {formid & id = formid.id +++ "t43"}
	nformidd = {formid & id = formid.id +++ "t44"}
   
TableFuncBut 		:: !FormId ![[(Button, a -> a)]] !*HSt 
													  -> (Form (a -> a) ,!*HSt)
TableFuncBut formid list hst = TableFuncBut` 0 formid list hst
where
	TableFuncBut` n _ [] hst
	= ({changed	= False
	   ,value	= id
	   ,form	= []
	   },hst)

	TableFuncBut` n formid [row:rows] hst 
	# (rows,hst)	= TableFuncBut` n formid rows hst
	# (row,  hst)	= RowFuncBut` n formid row hst
	= ({changed		= rows.changed || row.changed 
	   ,value		= rows.value o row.value
	   ,form		= [row.form <||> rows.form]
	   },hst)
	where
		RowFuncBut` :: !Int !FormId [(Button, a -> a)] !*HSt -> (Form (a -> a),!*HSt)
		RowFuncBut` n _ [] hst 
		= ({changed	= False
		   ,value	= id
		   ,form	= []
		   },hst)
		RowFuncBut` n formid [x:xs] hst 
		# (rowfun,hst) 	= RowFuncBut` n formid xs hst
		# (fun   ,hst)	= FuncBut False n formid x hst
		= ({changed		= rowfun.changed || fun.changed 
		   ,value		= rowfun.value o fun.value
		   ,form		= [fun.form <=> rowfun.form]
		   },hst)


FuncBut :: !Bool !Int !FormId !(Button, a -> a) !*HSt -> (Form (a -> a),!*HSt)
FuncBut init i formid (button=:LButton _ name ,cbf) hst = mkViewForm nformid id bimap hst
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
	nformid = {formid & id = formid.id +++ name +++ toString i}
FuncBut init i formid (button=:PButton _ ref ,cbf) hst = mkViewForm nformid id bimap hst
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
	nformid = {formid & id = formid.id +++ toString i +++ ref}
FuncBut init i s (Pressed,cbf) hst = FuncBut init i s (LButton 10 "??",cbf) hst

ListFuncBut :: !Bool !FormId [(Button, a -> a)] !*HSt -> (Form (a -> a),!*HSt)
ListFuncBut b s list hst = ListFuncBut` b 0 s list hst 
where
	ListFuncBut` b n s [] hst
	= ({changed	= False
	   ,value	= id
	   ,form	= []
	   },hst)
	ListFuncBut` b n s [x:xs] hst 
	# (rowfun,hst) 	= ListFuncBut` b (n+1) s xs hst
	# (fun   ,hst)	= FuncBut b n s x hst
	= ({changed	= rowfun.changed || fun.changed
	   ,value	= fun.value o rowfun.value
	   ,form	= [BodyTag fun.form:rowfun.form]
	   },hst)

ListFuncBut2 :: !Bool !FormId [(Mode,Button, a -> a)] !*HSt -> (Form (a -> a),!*HSt)
ListFuncBut2 b s list hst = ListFuncBut` b 0 s list hst 
where
	ListFuncBut` b n s [] hst
	= ({changed	= False
	   ,value	= id
	   ,form	= []
	   },hst)
	ListFuncBut` b n s [(bmode,but,func):xs] hst 
	# (rowfun,hst) 	= ListFuncBut` b (n+1) s xs hst
	# (fun   ,hst)	= FuncBut b n {s & mode = bmode} (but,func) hst
	= ({changed	= rowfun.changed || fun.changed
	   ,value	= fun.value o rowfun.value
	   ,form	= [BodyTag fun.form:rowfun.form]
	   },hst)

ListFuncCheckBox :: !Bool !FormId [(CheckBox, Bool [Bool] a -> a)] !*HSt 
									-> (Form (a -> a,[Bool]),!*HSt)
ListFuncCheckBox init s defs hst 
# (check,hst) = ListFuncCheckBox` init s defs hst
# (f,bools) = check.value
= ({changed	= False
   ,value	= (f bools,bools)
   ,form	= check.form
   },hst)
where
	ListFuncCheckBox` :: !Bool !FormId [(CheckBox, Bool [Bool] a -> a)] !*HSt 
										-> (Form ([Bool] a -> a,[Bool]),!*HSt)
	ListFuncCheckBox` init s [] hst
	= ({changed	= False
	   ,value	= (\_ a -> a,[])
	   ,form	= []
	   },hst)
	ListFuncCheckBox` init s [x:xs] hst 
	# (rowfun,hst) 	= ListFuncCheckBox` init s xs hst
	# (fun   ,hst)	= FuncCheckBox init s x hst
	# (rowfunv,boolsv) = rowfun.value
	# (funv,nboolv) = fun.value
	= ({changed	= rowfun.changed || fun.changed
	   ,value	= (funcomp funv rowfunv,[nboolv:boolsv])
	   ,form	= [BodyTag fun.form:rowfun.form]
	   },hst)
	where
		funcomp f g = \bools a = f bools (g bools a)
	
		FuncCheckBox init formid (checkbox,cbf) hst = mkViewForm nformid (\_ a -> a,False) bimap hst
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
		
			nformid = {formid & id = formid.id +++ case checkbox of 
														(CBChecked name) = name
														(CBNotChecked name) = name}

// the radio buttons implementation is currently more complicated than strictly necessary
// browsers demand the same name to be used for every member in the radio group
// the current implementation requires different names
// we therefore ourselves have to determine and remember which radio button in the family is set


ListFuncRadio :: !Int !FormId [Int a -> a] !*HSt 
									-> (Form (a -> a,Int),!*HSt)
ListFuncRadio i formid defs hst 
# (ni,hst)		= mkStoreForm formid (set (abs i) 0) (set i)  hst	// determine which radio to select

# (radio,hst) 	= ListFuncRadio` ni.value 0 formid defs hst	// determine if radio changed by user
# (f,nni)		= radio.value
# (f,i) 		= if (nni>=0) (f nni, nni) (id,ni.value)		// if so, select function, otherwise set to old radio
# (i,hst)		= mkStoreForm formid (set i i) (set i)  hst	// store current selected radio for next round
= ({changed	= ni.changed || radio.changed
   ,value	= (f,i.value)
   ,form	= radio.form
   },hst)
where
	radio i j 
	| i == j 	= RBChecked formid.id
	| otherwise = RBNotChecked formid.id

	set i j 
	| i >= 0 && i < length defs = i		// set to new radio buttun 
	| otherwise = j						// set to old radio button

	ListFuncRadio` :: !Int !Int !FormId [Int a -> a] !*HSt 
										-> (Form (Int a -> a,Int),!*HSt)
	ListFuncRadio` i j _ [] hst
	= ({changed	= False
	   ,value	= (\_ a -> a,-1)
	   ,form	= []
	   },hst)
	ListFuncRadio` i j formid [f:fs] hst 
	# (listradio,hst) 	= ListFuncRadio` i (j+1) formid fs hst
	# (funcradio,hst)	= FuncRadio i j formid f hst
	# (rowfun,rri) 		= listradio.value
	# (fun,ri) 			= funcradio.value
	= ({changed	= listradio.changed || funcradio.changed
	   ,value	= (funcomp fun rowfun,max ri rri)
	   ,form	= [BodyTag funcradio.form:listradio.form]
	   },hst)


	where
		funcomp f g = \i a = f i (g i a)
	
		FuncRadio i j formid cbf hst = mkViewForm nformid (\_ a -> a,-1) bimap  hst
		where
			bimap =	{ toForm 	= \_ v -> radio i j
					, updForm	= \b v -> if b (RBChecked formid.id) (otherradio v)
					, fromForm	= \b v -> if b (cbf,j) (\_ a -> a,-1)
					, resetForm	= Nothing
					}
			otherradio v
			| StrippedCheckUpdateId == formid.id = RBNotChecked formid.id
			| otherwise = v
			
			nformid = {formid & id = formid.id +++ "_" +++ toString j}

FuncMenu :: !Int !FormId [(String, a -> a)] !*HSt 
													 -> (Form (a -> a,Int),!*HSt)
FuncMenu index s defs hst = mkViewForm s (id,init index) bimap  hst
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


listForm 			:: !FormId ![a] 		!*HSt -> (Form [a],!*HSt) 	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
listForm _  [] hst
= ({changed	= False
   ,value	= []
   ,form	= []
   },hst)
listForm formid [x:xs] hst
# (nxs,hst) = listForm formid xs hst
# (nx, hst) = mkEditForm nformid x hst
= ({changed	= nx.changed || nxs.changed
   ,value	= [nx.value:nxs.value]
   ,form	= [BodyTag nx.form:nxs.form]
   },hst)
where
	nformid = {formid & id = formid.id +++ toString (length xs)}

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
	