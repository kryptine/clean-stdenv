implementation module htmlFormlib

// Handy collection of Form's
// Form library similar to the AGEC lib
// (c) MJP 2005

import StdEnv
import StdHtml

// operators for lay-out of html bodys ...

// Place two bodies next to each other

(<=>) infixl 5 :: BodyTag BodyTag -> BodyTag
(<=>) b1 b2 =  STable [Tbl_CellPadding (Pixels 0), Tbl_CellSpacing (Pixels 0)] [[b1,b2]]

// Place second body below first

(<||>) infixl 4	:: BodyTag BodyTag -> BodyTag	// Place a above b
(<||>) b1 b2 =  STable [Tbl_CellPadding (Pixels 0), Tbl_CellSpacing (Pixels 0)] [[b1],[b2]]

mkSTable :: [[BodyTag]] -> BodyTag
mkSTable table
= Table []	(mktable table)
where
	mktable table 	= [Tr [] (mkrow rows) \\ rows <- table]	
	mkrow rows 		= [Td [Td_VAlign Alo_Top, Td_Width (Pixels defpixel)] [row] \\ row <- rows] 


// Form collection:

counterForm 	:: !FormId !Mode a !*HSt -> ((a,!BodyTag),!*HSt) | +, -, one,  gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} a
counterForm name mode i hst = mkViewForm name mode bimap i hst
where
	bimap =	{ toForm 	= \n _ -> (n,down,up)
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


horlist2Form :: !FormId !Mode a ![a] !*HSt -> (([a],!BodyTag),!*HSt) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} a
horlist2Form s mode defaultval list hst 
# ((fun,butbody),hst) 	= TableFuncBut  s mode [[(but "-", less),(but "+", more)]] hst
# ((nlist,nbody),hst) 	= horlistForm s mode (fun list) hst  
= ((nlist,butbody <||> nbody),hst)
where
	but s =  LButton (defpixel / 6) s

	more []   = [defaultval]
	more list = list ++ [last list]

	less [x:xs] = xs
	less [] = []
	
horlistForm 	:: !FormId !Mode ![a] 	!*HSt -> (([a],!BodyTag),!*HSt) 	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} a
horlistForm s mode [] hst  = (([],EmptyBody),hst)
horlistForm s mode [x:xs] hst
# ((xs,xsbody),hst) = horlistForm s mode xs hst
# ((x, xbody), hst) = mkEditForm (s +++ toString (length xs)) mode x hst
= (([x:xs],xbody <=> xsbody),hst)
	
vertlistForm :: !FormId !Mode ![a] !*HSt -> (([a],!BodyTag),!*HSt) | gForm{|*|} a & gUpd{|*|}  a & gPrint{|*|} a & gParse{|*|} a 
vertlistForm s mode [] hst  = (([],EmptyBody),hst)
vertlistForm s mode [x:xs] hst
# ((xs,xsbody),hst)	= vertlistForm s mode xs hst
# ((x, xbody), hst)	= mkEditForm (s +++ toString (length xs)) mode x hst
= (([x:xs],xbody <||> xsbody),hst)

table_hv_Form :: !FormId !Mode ![[a]] !*HSt -> (([[a]],!BodyTag),!*HSt) | gForm{|*|} a & gUpd{|*|}  a & gPrint{|*|} a & gParse{|*|} a 
table_hv_Form s mode [] hst = (([],EmptyBody),hst)
table_hv_Form s mode [x:xs] hst
# ((xs,xsbody),hst)	= table_hv_Form s mode xs hst
# ((x, xbody), hst)	= horlistForm (s +++ toString (length xs)) mode x hst
= (([x:xs],xbody <||> xsbody),hst)

TableFuncBut :: !FormId !Mode ![[(Button, a -> a)]] !*HSt -> ((a -> a,!BodyTag),!*HSt)
TableFuncBut s mode [] hst = ((id,EmptyBody),hst)
TableFuncBut s mode [row:rows] hst 
# ((rowsfun,rowsb),hst) = TableFuncBut s mode rows hst
# ((rowfun,rowb),  hst)	= RowFuncBut s mode row hst
= ((rowfun o rowsfun,rowb <||> rowsb),hst)
where
	RowFuncBut :: !FormId !Mode [(Button, a -> a)] !*HSt -> ((a -> a,!BodyTag),!*HSt)
	RowFuncBut s mode [] hst = ((id,EmptyBody),hst)
	RowFuncBut s mode [x:xs] hst 
	# ((rowfun,rowb),hst) 	= RowFuncBut s mode xs hst
	# ((fun,oneb)   ,hst)	= FuncBut False s mode x hst
	= ((fun o rowfun,oneb <=> rowb),hst)

FuncBut :: !Bool !FormId !Mode !(Button, a -> a) !*HSt -> ((a -> a,!BodyTag),!*HSt)
FuncBut init s mode (button=:LButton _ name ,cbf) hst = mkViewForm (s +++ name) mode bimap id hst
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
FuncBut init s mode (button=:PButton _ ref ,cbf) hst = mkViewForm (s +++ ref) mode bimap id hst
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
FuncBut init s mode (Pressed,cbf) hst = FuncBut init s mode (LButton 10 "??",cbf) hst

ListFuncBut :: !Bool !FormId !Mode [(Button, a -> a)] !*HSt -> ((a -> a,![BodyTag]),!*HSt)
ListFuncBut b s mode [] hst = ((id,[]),hst)
ListFuncBut b s mode [x:xs] hst 
# ((rowfun,rowb),hst) 	= ListFuncBut b s mode xs hst
# ((fun,oneb)   ,hst)	= FuncBut b s mode x hst
= ((fun o rowfun,[oneb:rowb]),hst)

ListFuncCheckBox :: !Bool !FormId !Mode [(CheckBox, Bool [Bool] a -> a)] !*HSt 
									-> (((a -> a,[Bool]),![BodyTag]),!*HSt)
ListFuncCheckBox init s mode defs hst 
# (((f,bools),body),hst) = ListFuncCheckBox` init s mode defs hst
= (((f bools,bools),body),hst)
where
	ListFuncCheckBox` :: !Bool !FormId !Mode [(CheckBox, Bool [Bool] a -> a)] !*HSt 
										-> ((([Bool] a -> a,[Bool]),![BodyTag]),!*HSt)
	ListFuncCheckBox` init s mode [] hst = (((\_ a -> a,[]),[]),hst)
	ListFuncCheckBox` init s mode [x:xs] hst 
	# (((rowfun,bools),rowb),hst) 	= ListFuncCheckBox` init s mode xs hst
	# (((fun,nbool),oneb)   ,hst)	= FuncCheckBox init s mode x hst
	= (((funcomp fun rowfun,[nbool:bools]),[oneb:rowb]),hst)
	where
		funcomp f g = \bools a = f bools (g bools a)
	
		FuncCheckBox init s mode (checkbox,cbf) hst = mkViewForm s` mode bimap (\_ a -> a,False) hst
		where
			bimap =	{ toForm 	= \_ v -> case v of
											Nothing = checkbox
											(Just v) = if init checkbox v
					, updForm	= \b v -> if b (toggle v) v
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
									-> (((a -> a,Int),![BodyTag]),!*HSt)
ListFuncRadio i s mode defs hst 
# ((ni,_),hst)			= mkStoreForm s (set i) (set (abs i) 0) hst	// determine which radio to select
# (((f,nni),body),hst) 	= ListFuncRadio` ni 0 s mode defs hst	// determine if radio changed by user
# (f,i) 				= if (nni>=0) (f nni, nni) (id,ni)		// if so, select function, otherwise set to old radio
# ((i,_),hst)			= mkStoreForm s (set i) (set i i) hst	// store current selected radio for next round
= (((f,i),body),hst)
where
	radio i j 
	| i == j 	= RBChecked s
	| otherwise = RBNotChecked s

	set i j 
	| i >= 0 && i < length defs = i		// set to new radio buttun 
	| otherwise = j						// set to old radio button

	ListFuncRadio` :: !Int !Int !FormId !Mode [Int a -> a] !*HSt 
										-> (((Int a -> a,Int),![BodyTag]),!*HSt)
	ListFuncRadio` i j s mode [] hst = (((\_ a -> a,-1),[]),hst)
	ListFuncRadio` i j s mode [f:fs] hst 
	# (((rowfun,rri),listr),hst) 	= ListFuncRadio` i (j+1) s mode fs hst
	# (((fun,ri),oner)   ,hst)		= FuncRadio i j s mode f hst
	= (((funcomp fun rowfun,max ri rri),[oner:listr]),hst)
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
													 -> (((a -> a,Int),BodyTag),!*HSt)
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


listForm 	:: !FormId !Mode ![a] 	!*HSt -> (([a],![BodyTag]),!*HSt) 	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} a
listForm s mode [] hst  = (([],[]),hst)
listForm s mode [x:xs] hst
# ((xs,xsbody),hst) = listForm s mode xs hst
# ((x, xbody), hst) = mkEditForm (s +++ toString (length xs)) mode x hst
= (([x:xs],[xbody:xsbody]),hst)
