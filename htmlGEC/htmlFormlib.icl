implementation module htmlFormlib

// Handy collection of Form's
// (c) MJP 2005

import StdEnv, StdHtml

derive gForm []; derive gUpd []

// easy creation of an html page

mkHtml:: String [BodyTag] *HSt -> (Html,*HSt)
mkHtml s tags hst 	= (simpleHtml s tags,hst)

simpleHtml:: String [BodyTag] -> Html
simpleHtml s tags 	= Html (header s) (body tags)
where
	header s		= Head [`Hd_Std [Std_Title s]] [] 
	body tags		= Body [] tags

// operators for lay-out of html bodys ...

// Place two bodies next to each other

(<=>) infixl 5 :: [BodyTag] [BodyTag] -> BodyTag
(<=>) b1 b2 = (BodyTag b1) <.=.> (BodyTag b2)

(<.=.>) infixl 5 :: BodyTag BodyTag -> BodyTag
(<.=.>) b1 b2 =  STable [Tbl_CellPadding (Pixels 0), Tbl_CellSpacing (Pixels 0)] [[b1,b2]]

// Place second body below first

(<||>) infixl 4	:: [BodyTag] [BodyTag] -> BodyTag	// Place a above b
(<||>) b1 b2 = (BodyTag b1) <.||.> (BodyTag b2)

(<.||.>) infixl 4	:: BodyTag BodyTag -> BodyTag	// Place a above b
(<.||.>) b1 b2 =  STable [Tbl_CellPadding (Pixels 0), Tbl_CellSpacing (Pixels 0)] [[b1],[b2]]

(<=|>) infixl 4	:: [BodyTag] [BodyTag] -> BodyTag	// Place a above b
(<=|>) b1 b2 =  STable [Tbl_CellPadding (Pixels 0), Tbl_CellSpacing (Pixels 0)] [[be1,be2] \\ be1 <- b1 & be2 <- b2]

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

// frequently used variants of mkViewForm

mkEditForm:: !FormId !(Init d) !*HSt -> (Form d,!*HSt) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC d
mkEditForm formid data hst
= mkViewForm formid data
	{toForm = toViewId , updForm = \_ v -> v , fromForm = \_ v -> v , resetForm = Nothing}  hst

mkSelfForm  :: !FormId 	!(Init d) !(d -> d) !*HSt -> (Form d,!*HSt) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC d
mkSelfForm formid initdata cbf  hst
= mkViewForm formid initdata 
	{toForm = toViewId , updForm = update , fromForm = \_ v -> v , resetForm = Nothing}  hst
where
	update b val
	| b.isChanged 	= cbf val
	| otherwise 	= val
	
mkStoreForm :: !FormId !(Init d) !(d -> d) !*HSt -> (Form d,!*HSt) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC d
mkStoreForm formid data cbf  hst
= mkViewForm formid data 
	{toForm = toViewId , updForm = \_ v = cbf v , fromForm = \_ v -> v, resetForm = Nothing}  hst

mkApplyEditForm	:: !FormId !(Init d) !d !*HSt -> (Form d,!*HSt) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC d
mkApplyEditForm formid initval inputval  hst
= mkViewForm formid initval 
	{toForm =  toViewId , updForm = update , fromForm = \_ v -> v, resetForm = Nothing}  hst
where
	update b val
	| b.isChanged 	= val
	| otherwise 	= inputval

mkBimapEditor :: !FormId !(Init d) !(Bimap d v) !*HSt -> (Form d,!*HSt) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC v
mkBimapEditor formid d {map_to,map_from} hst
= mkViewForm formid d 	{ toForm 	= toViewMap map_to
						, updForm 	= \_ v -> v
						, fromForm 	= \_ v -> map_from v
						, resetForm = Nothing
						} hst 

mkSubStateForm :: !FormId !subState state (subState state -> state) !*HSt -> (state,![BodyTag],!*HSt)
							| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC subState
mkSubStateForm formid subState state upd hst 
# (nsubState,hst) 		= mkEditForm formid (Init subState) hst
# (commitBut,hst)		= FuncBut (extendFormId formid "CommitBut") (Init (LButton defpixel "commit",id)) hst
# (cancelBut,hst)		= FuncBut (extendFormId formid "CancelBut") (Init (LButton defpixel "cancel",id)) hst
# (nsubState,hst) 		= if cancelBut.changed 
								(mkEditForm formid (Set subState) hst)
								(nsubState,hst)
= 	( 	if commitBut.changed (upd nsubState.value state) state
	,	[ BodyTag nsubState.form
		, Br
		, BodyTag commitBut.form
		, BodyTag cancelBut.form
		]
	, 	hst)

// Form collection:

horlistForm :: !FormId !(Init [a]) !*HSt -> (Form [a],!*HSt) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
horlistForm formid list hSt = layoutListForm (\f1 f2 -> [f1 <=> f2]) mkEditForm formid list hSt
			
vertlistForm :: !FormId !(Init [a]) !*HSt -> (Form [a],!*HSt) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
vertlistForm formid list hSt = layoutListForm (\f1 f2 -> [f1 <||> f2]) mkEditForm formid list hSt

table_hv_Form :: !FormId !(Init [[a]]) !*HSt -> (Form [[a]],!*HSt) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
table_hv_Form formid listoflists hSt = layoutListForm (\f1 f2 -> [f1 <||> f2]) horlistForm formid listoflists hSt


t2EditForm  :: !FormId !(Init (a,b)) !*HSt -> ((Form a,Form b),!*HSt) |  gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
																	 &  gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC b
t2EditForm formid ab hst
# (forma,hst) = mkEditForm nformida (PropInit ab a) hst 
# (formb,hst) = mkEditForm nformidb (PropInit ab b) hst
= ((forma,formb),hst) 
where
	(a,b) = GetInit ab
	nformida = extendFormId formid "t21"
	nformidb = extendFormId formid "t22"

t3EditForm  :: !FormId !(Init (a,b,c)) !*HSt -> ((Form a,Form b,Form c),!*HSt) |  gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
																	   &  gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC b
																	   &  gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC c
t3EditForm formid abc hst
# (forma,hst) = mkEditForm nformida (PropInit abc a) hst 
# (formb,hst) = mkEditForm nformidb (PropInit abc b) hst
# (formc,hst) = mkEditForm nformidc (PropInit abc c) hst
= ((forma,formb,formc),hst) 
where
	(a,b,c) = GetInit abc
	nformida = extendFormId formid "t31"
	nformidb = extendFormId formid "t32"
	nformidc = extendFormId formid "t33"

t4EditForm  :: !FormId !(Init (a,b,c,d)) !*HSt -> ((Form a,Form b,Form c,Form d),!*HSt) |  gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
																	   &  gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC b
																	   &  gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC c
																	   &  gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC d
t4EditForm formid abcd hst
# (forma,hst) = mkEditForm nformida (PropInit abcd a) hst 
# (formb,hst) = mkEditForm nformidb (PropInit abcd b) hst
# (formc,hst) = mkEditForm nformidc (PropInit abcd c) hst
# (formd,hst) = mkEditForm nformidd (PropInit abcd d) hst
= ((forma,formb,formc,formd),hst) 
where
	(a,b,c,d) = GetInit abcd
	nformida = extendFormId formid "t41"
	nformidb = extendFormId formid "t42"
	nformidc = extendFormId formid "t43"
	nformidd = extendFormId formid "t44"
	
counterForm 	:: !FormId !(Init a) !*HSt -> (Form a,!*HSt) | +, -, one, gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
counterForm name i hst = mkViewForm name i bimap hst
where
	bimap =	{ toForm 	= toViewMap (\n -> (n,down,up))
			, updForm	= updCounter`
			, fromForm	= \_ (n,_,_) -> n
			, resetForm = Nothing
			}

	updCounter` b val
	| b.isChanged 	= updCounter val
	| otherwise 	= val

	updCounter (n,Pressed,_)  = (n - one,down,up)
	updCounter (n,_,Pressed) 	= (n + one,down,up)
	updCounter else 			= else

	up 		= LButton (defpixel / 6) "+"
	down	= LButton (defpixel / 6) "-"

listForm :: !FormId !(Init [a]) !*HSt -> (Form [a],!*HSt) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
listForm formid xs hSt = layoutListForm (\f1 f2 -> [BodyTag f1:f2]) mkEditForm formid xs hSt

layoutListForm :: !([BodyTag] [BodyTag] -> [BodyTag]) 
                  !(FormId !(Init a) *HSt -> (Form a,*HSt))
                  !FormId !(Init [a]) !*HSt -> (Form [a],!*HSt) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
layoutListForm layoutF formF formid list hst
= case GetInit list of
	[] = ({changed	= False
		   ,value	= []
		   ,form	= []
		   },hst)
	[x:xs]
	# (nxs,hst) = layoutListForm layoutF formF formid (PropInit list xs) hst
	# (nx, hst) = formF nformid (PropInit list x) hst
	= ({changed	= nx.changed || nxs.changed
	   ,value	= [nx.value:nxs.value]
	   ,form	= layoutF nx.form nxs.form
	   },hst)
	where
		nformid = {formid & id = formid.id <$ length xs}

FuncBut :: !FormId !(Init (Button, a -> a)) !*HSt -> (Form (a -> a),!*HSt)
FuncBut formid init hst = FuncButNr 0 formid init hst 

FuncButNr :: !Int !FormId !(Init (Button, a -> a)) !*HSt -> (Form (a -> a),!*HSt)
FuncButNr i formid init hst
= case GetInit init of
	(Pressed,cbf)	= FuncButNr i formid (Set (LButton 10 "??",cbf)) hst
	(button,cbf)	= mkViewForm nformid (Set id) hbimap hst
	where
		hbimap =	{ toForm 	= \_ v -> toViewId (PropInit init button) v
					, updForm	= \_ v -> v
					, fromForm	= \_ but -> case but of 
											Pressed  -> cbf
											_		 -> id
					, resetForm	= Just (const button)
					}
		nformid = case button of
					LButton _ name -> {formid & id = formid.id <$ name <$ i}
					PButton _ ref  -> {formid & id = formid.id <$ i <$ ref}

TableFuncBut :: !FormId !(Init [[(Button, a -> a)]]) !*HSt -> (Form (a -> a) ,!*HSt)
TableFuncBut formid xs hSt
	= layoutIndexForm (\f1 f2 -> [f1 <||> f2]) 
		(layoutIndexForm (\f1 f2 -> [BodyTag f1:f2]) FuncButNr id (o)) 
			id (o) 0 formid xs hSt

ListFuncBut2 :: !FormId !(Init [(Mode,Button, a -> a)]) !*HSt -> (Form (a -> a),!*HSt)
ListFuncBut2 s list hst = ListFuncBut` 0 s (GetInit list) hst 
where
	ListFuncBut` _ s [] hst
	= ({changed	= False
	   ,value	= id
	   ,form	= []
	   },hst)
	ListFuncBut` n s [(bmode,but,func):xs] hst 
	# (rowfun,hst) 	= ListFuncBut` (n+1) s xs hst
	# (fun   ,hst)	= FuncButNr n {s & mode = bmode} (PropInit list (but,func)) hst
	= ({changed	= rowfun.changed || fun.changed
	   ,value	= fun.value o rowfun.value
	   ,form	= [BodyTag fun.form:rowfun.form]
	   },hst)

TableFuncBut2 :: !FormId !(Init [[(Mode,Button, a -> a)]]) !*HSt -> (Form (a -> a) ,!*HSt)
TableFuncBut2 formid list hSt = TableFuncBut2` 0 formid (GetInit list) hSt
where
	TableFuncBut2` n formid [] hSt 	
		= ({changed	= False
		   ,value	= id
		   ,form	= []
		   },hSt)
	TableFuncBut2` n formid [x:xs] hSt 
	# (nx,hSt)	=	ListFuncBut2 {formid & id = formid.id +++ toString n} (PropInit list x) hSt
	# (nxs,hSt)	=	TableFuncBut2` (n+1) formid xs hSt
	= ({changed = nx.changed || nxs.changed
	   ,value   = nx.value o nxs.value
	   ,form    = [ nx.form <||> nxs.form ]
	   },hSt)


//	Generalized form of ListFuncBut:
layoutIndexForm :: !([BodyTag] [BodyTag] -> [BodyTag]) 
                   	!(Int FormId !(Init x) *HSt -> (Form y,*HSt))
                   	 y (y y -> y)
                   	 !Int !FormId !(Init [x]) !*HSt -> (Form y,!*HSt)
layoutIndexForm layoutF formF r combineF n formid list hSt
= case (GetInit list) of
	[]	= ({changed=False, value=r, form=[]},hSt)
	[x:xs]
	# (xsF,hSt)	= layoutIndexForm layoutF formF r combineF (n+1) formid (PropInit list xs) hSt
	# (xF, hSt)	= formF n formid (PropInit list x) hSt
	= ({changed = xsF.changed || xF.changed
	   ,value   = combineF xsF.value xF.value
	   ,form    = layoutF xF.form xsF.form
	   },hSt)

ListFuncBut :: !FormId !(Init [(Button, a -> a)]) !*HSt -> (Form (a -> a),!*HSt)
ListFuncBut formid xs hSt
	= layoutIndexForm (\f1 f2 -> [BodyTag f1:f2]) FuncButNr id (o) 0 formid xs hSt

ListFuncCheckBox :: !FormId !(Init [(CheckBox, Bool [Bool] a -> a)]) !*HSt 
									-> (Form (a -> a,[Bool]),!*HSt)
ListFuncCheckBox formid init hst 
# (check,hst) = ListFuncCheckBox` formid (GetInit init) hst
# (f,bools) = check.value
= ({changed	= False
   ,value	= (f bools,bools)
   ,form	= check.form
   },hst)
where
	ListFuncCheckBox` :: !FormId ! [(CheckBox, Bool [Bool] a -> a)] !*HSt 
										-> (Form ([Bool] a -> a,[Bool]),!*HSt)
	ListFuncCheckBox` formid [] hst
	= ({changed	= False
	   ,value	= (\_ a -> a,[])
	   ,form	= []
	   },hst)
	ListFuncCheckBox` formid [x:xs] hst 
	# (rowfun,hst) 	= ListFuncCheckBox` formid xs hst
	# (fun   ,hst)	= FuncCheckBox formid x hst
	# (rowfunv,boolsv) = rowfun.value
	# (funv,nboolv) = fun.value
	= ({changed	= rowfun.changed || fun.changed
	   ,value	= (funcomp funv rowfunv,[nboolv:boolsv])
	   ,form	= [BodyTag fun.form:rowfun.form]
	   },hst)
	where
		funcomp f g = \bools a = f bools (g bools a)
	
		FuncCheckBox formid (checkbox,cbf) hst = mkViewForm nformid (PropInit init (\_ a -> a,False)) bimap hst
		where
			bimap =	{ toForm 	= \_ v -> toViewId (PropInit init checkbox) v
					, updForm	= \b v -> if (not (toBool init) && b.isChanged) (toggle v) v // if (not init && b.isChanged) (toggle v) v
					, fromForm	= \b v -> if b.isChanged ((docbf  v),toBool v) (\_ a -> a,toBool v)
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


ListFuncRadio :: !FormId !(Init (Int,[Int a -> a])) !*HSt 
									-> (Form (a -> a,Int),!*HSt)
ListFuncRadio formid alldefs hst 
# (ni,hst)		= mkStoreForm formid (PropInit alldefs (setradio (abs i) 0)) (setradio i)  hst	// determine which radio to select
# (radio,hst) 	= ListFuncRadio` ni.value 0 formid defs hst	// determine if radio changed by user
# (f,nni)		= radio.value
# (f,i) 		= if (nni>=0) (f nni, nni) (id,ni.value)		// if so, select function, otherwise set to old radio
# (i,hst)		= mkStoreForm formid (PropInit alldefs (setradio i i)) (setradio i)  hst	// store current selected radio for next round
= ({changed	= ni.changed || radio.changed
   ,value	= (f,i.value)
   ,form	= radio.form
   },hst)
where
	(i,defs) = GetInit alldefs

	radio i j 
	| i == j 	= RBChecked formid.id
	| otherwise = RBNotChecked formid.id

	setradio i j 
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
	
		FuncRadio i j formid cbf hst = mkViewForm nformid (PropInit alldefs (\_ a -> a,-1)) bimap  hst
		where
			bimap =	{ toForm 	= \_ v -> radio i j
					, updForm	= \b v -> if b.isChanged (RBChecked formid.id) (otherradio b v)
					, fromForm	= \b v -> if b.isChanged (cbf,j) (\_ a -> a,-1)
					, resetForm	= Nothing
					}
			otherradio b v
			| stripname b.changedId == formid.id = RBNotChecked formid.id
			| otherwise = v
			
			nformid = {formid & id = formid.id +++ "_" +++ toString j}

			stripname name = mkString (takeWhile ((<>) '_') (mkList name))

FuncMenu :: !FormId !(Init (Int,[(String, a -> a)])) !*HSt 
													 -> (Form (a -> a,Int),!*HSt)
FuncMenu s alldefs hst = mkViewForm s (PropInit alldefs (id,init index)) bimap  hst
where
	(index,defs) = GetInit alldefs

	menulist = PullDown (1,defpixel) (init index,map fst defs) 

	bimap =	{ toForm 	= toViewMap (\x -> menulist)
			, updForm	= \_ v -> v
			, fromForm	= \b v=:(PullDown _ (nindex,_)) -> if b.isChanged (snd (defs!!nindex),nindex) (id,nindex)
			, resetForm	= Nothing
			}

	init index
	| abs index >= 0 && abs index < length defs = abs index
	| otherwise = 0

browseButtons :: !(Init !Int) !Int !Int !Int !FormId !*HSt -> (Form Int,!*HSt)
browseButtons curindex step length nbuttuns formid hst
# (nindex, hst)		= mkStoreForm formid curindex id hst
# (calcnext, hst)	= browserForm nindex.value hst
# (nindex, hst)		= mkStoreForm formid curindex calcnext.value  hst
# (shownext, hst)	= browserForm nindex.value hst
= ({changed = calcnext.changed
   ,value	= nindex.value
   ,form	= shownext.form},hst)
where
	browserForm :: !Int *HSt -> (Form (Int -> Int),!*HSt) 
	browserForm index hst
		= ListFuncBut2 formid (Init (browserButtons index step length)) hst
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
