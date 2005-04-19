implementation module htmlHGEClib

// Handy collection of HGEC's
// HGEC library similar to the AGEC lib
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


// HGEC collection:

counterHGEC 	:: !String !HMode a !*HSt -> ((a,!BodyTag),!*HSt) | +, -, one,  gHGEC{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} a
counterHGEC name mode i hst = mkViewHGEC name mode bimap i hst
where
	bimap =	{ toHGEC 	= toCounter
			, updHGEC	= updCounter`
			, fromHGEC	= fromCounter
			, resetHGEC = Nothing
			}

	toCounter n _ = (n,down,up)

	fromCounter _ (n,_,_) = n

	updCounter` True val = updCounter val
	updCounter` _ val = val

	updCounter (n,Pressed,_)  = (n - one,down,up)
	updCounter (n,_,Pressed) 	= (n + one,down,up)
	updCounter else 			= else

	up 		= LButton (defpixel / 6) "+"
	down	= LButton (defpixel / 6) "-"


horlist2HGEC :: !String !HMode a ![a] !*HSt -> (([a],!BodyTag),!*HSt) | gHGEC{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} a
horlist2HGEC s mode defaultval list hst 
# ((fun,butbody),hst) 	= TableFuncBut  s mode [[(but "-", less),(but "+", more)]] hst
# ((nlist,nbody),hst) 	= horlistHGEC s mode (fun list) hst  
= ((nlist,butbody <||> nbody),hst)
where
	but s =  LButton (defpixel / 6) s

	more []   = [defaultval]
	more list = list ++ [last list]

	less [x:xs] = xs
	less [] = []
	
horlistHGEC 	:: !String !HMode ![a] 	!*HSt -> (([a],!BodyTag),!*HSt) 	| gHGEC{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} a
horlistHGEC s mode [] hst  = (([],EmptyBody),hst)
horlistHGEC s mode [x:xs] hst
# ((xs,xsbody),hst) = horlistHGEC s mode xs hst
# ((x, xbody), hst) = mkEditHGEC (s +++ toString (length xs)) mode x hst
= (([x:xs],xbody <=> xsbody),hst)
	
vertlistHGEC :: !String !HMode ![a] !*HSt -> (([a],!BodyTag),!*HSt) | gHGEC{|*|} a & gUpd{|*|}  a & gPrint{|*|} a & gParse{|*|} a 
vertlistHGEC s mode [] hst  = (([],EmptyBody),hst)
vertlistHGEC s mode [x:xs] hst
# ((xs,xsbody),hst)	= vertlistHGEC s mode xs hst
# ((x, xbody), hst)	= mkEditHGEC (s +++ toString (length xs)) mode x hst
= (([x:xs],xbody <||> xsbody),hst)

table_hv_HGEC :: !String !HMode ![[a]] !*HSt -> (([[a]],!BodyTag),!*HSt) | gHGEC{|*|} a & gUpd{|*|}  a & gPrint{|*|} a & gParse{|*|} a 
table_hv_HGEC s mode [] hst = (([],EmptyBody),hst)
table_hv_HGEC s mode [x:xs] hst
# ((xs,xsbody),hst)	= table_hv_HGEC s mode xs hst
# ((x, xbody), hst)	= horlistHGEC (s +++ toString (length xs)) mode x hst
= (([x:xs],xbody <||> xsbody),hst)

TableFuncBut :: !String !HMode ![[(Button, a -> a)]] !*HSt -> ((a -> a,!BodyTag),!*HSt)
TableFuncBut s mode [] hst = ((id,EmptyBody),hst)
TableFuncBut s mode [row:rows] hst 
# ((rowsfun,rowsb),hst) = TableFuncBut s mode rows hst
# ((rowfun,rowb),  hst)	= RowFuncBut s mode row hst
= ((rowfun o rowsfun,rowb <||> rowsb),hst)
where
	RowFuncBut :: !String !HMode [(Button, a -> a)] !*HSt -> ((a -> a,!BodyTag),!*HSt)
	RowFuncBut s mode [] hst = ((id,EmptyBody),hst)
	RowFuncBut s mode [x:xs] hst 
	# ((rowfun,rowb),hst) 	= RowFuncBut s mode xs hst
	# ((fun,oneb)   ,hst)	= FuncBut s mode x hst
	= ((fun o rowfun,oneb <=> rowb),hst)

FuncBut :: !String !HMode !(Button, a -> a) !*HSt -> ((a -> a,!BodyTag),!*HSt)
FuncBut s mode (button=:LButton _ name ,cbf) hst = mkViewHGEC (s +++ name) mode bimap id hst
where
	bimap =	{ toHGEC 	= \_ _	-> button
			, updHGEC	= \_ v -> v
			, fromHGEC	= \_ but -> case but of 
									Pressed -> cbf
									_		  -> id
			, resetHGEC	= Just (\_ 	-> button)
			}
FuncBut s mode (button=:PButton _ ref ,cbf) hst = mkViewHGEC (s +++ ref) mode bimap id hst
where
	bimap =	{ toHGEC 	= \_ _	-> button
			, updHGEC	= \_ v -> v
			, fromHGEC	= \_ but -> case but of 
									Pressed -> cbf
									_		  -> id
			, resetHGEC	= Just (\_ 	-> button)
			}
FuncBut s mode (Pressed,cbf) hst = FuncBut s mode (LButton 10 "??",cbf) hst

ListFuncBut :: !String !HMode [(Button, a -> a)] !*HSt -> ((a -> a,![BodyTag]),!*HSt)
ListFuncBut s mode [] hst = ((id,[]),hst)
ListFuncBut s mode [x:xs] hst 
# ((rowfun,rowb),hst) 	= ListFuncBut s mode xs hst
# ((fun,oneb)   ,hst)	= FuncBut s mode x hst
= ((fun o rowfun,[oneb:rowb]),hst)

ListFuncCheckBox :: !Bool !String !HMode [(CheckBox, Bool [Bool] a -> a)] !*HSt 
									-> (((a -> a,[Bool]),![BodyTag]),!*HSt)
ListFuncCheckBox init s mode defs hst 
# (((f,bools),body),hst) = ListFuncCheckBox` init s mode defs hst
= (((f bools,bools),body),hst)
where
	ListFuncCheckBox` :: !Bool !String !HMode [(CheckBox, Bool [Bool] a -> a)] !*HSt 
										-> ((([Bool] a -> a,[Bool]),![BodyTag]),!*HSt)
	ListFuncCheckBox` init s mode [] hst = (((\_ a -> a,[]),[]),hst)
	ListFuncCheckBox` init s mode [x:xs] hst 
	# (((rowfun,bools),rowb),hst) 	= ListFuncCheckBox` init s mode xs hst
	# (((fun,nbool),oneb)   ,hst)	= FuncCheckBox init s mode x hst
	= (((funcomp fun rowfun,[nbool:bools]),[oneb:rowb]),hst)
	where
		funcomp f g = \bools a = f bools (g bools a)
	
		FuncCheckBox init s mode (checkbox,cbf) hst = mkViewHGEC s` mode bimap (\_ a -> a,False) hst
		where
			bimap =	{ toHGEC 	= \_ v -> case v of
											Nothing = checkbox
											(Just v) = if init checkbox v
					, updHGEC	= \b v -> if b (toggle v) v
					, fromHGEC	= \b v -> if b ((docbf  v),toBool v) (\_ a -> a,toBool v)
					, resetHGEC	= Nothing
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


ListFuncRadio :: !Int !String !HMode [Int a -> a] !*HSt 
									-> (((a -> a,Int),![BodyTag]),!*HSt)
ListFuncRadio i s mode defs hst 
# ((ni,_),hst)			= mkStoreHGEC s (set i) (set (abs i) 0) hst	// determine which radio to select
# (((f,nni),body),hst) 	= ListFuncRadio` ni 0 s mode defs hst	// determine if radio changed by user
# (f,i) 				= if (nni>=0) (f nni, nni) (id,ni)		// if so, select function, otherwise set to old radio
# ((i,_),hst)			= mkStoreHGEC s (set i) (set i i) hst	// store current selected radio for next round
= (((f,i),body),hst)
where
	radio i j 
	| i == j 	= RBChecked s
	| otherwise = RBNotChecked s

	set i j 
	| i >= 0 && i < length defs = i		// set to new radio buttun 
	| otherwise = j						// set to old radio button

	ListFuncRadio` :: !Int !Int !String !HMode [Int a -> a] !*HSt 
										-> (((Int a -> a,Int),![BodyTag]),!*HSt)
	ListFuncRadio` i j s mode [] hst = (((\_ a -> a,-1),[]),hst)
	ListFuncRadio` i j s mode [f:fs] hst 
	# (((rowfun,rri),listr),hst) 	= ListFuncRadio` i (j+1) s mode fs hst
	# (((fun,ri),oner)   ,hst)		= FuncRadio i j s mode f hst
	= (((funcomp fun rowfun,max ri rri),[oner:listr]),hst)
	where
		funcomp f g = \i a = f i (g i a)
	
		FuncRadio i j s mode cbf hst = mkViewHGEC s` mode bimap (\_ a -> a,-1) hst
		where
			bimap =	{ toHGEC 	= \_ v -> radio i j
					, updHGEC	= \b v -> if b (RBChecked s) (otherradio v)
					, fromHGEC	= \b v -> if b (cbf,j) (\_ a -> a,-1)
					, resetHGEC	= Nothing

					}
			s` = s +++ "_" +++ toString j

			otherradio v
			| StrippedCheckUpdateId == s = RBNotChecked s
			| otherwise = v


listHGEC 	:: !String !HMode ![a] 	!*HSt -> (([a],![BodyTag]),!*HSt) 	| gHGEC{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} a
listHGEC s mode [] hst  = (([],[]),hst)
listHGEC s mode [x:xs] hst
# ((xs,xsbody),hst) = listHGEC s mode xs hst
# ((x, xbody), hst) = mkEditHGEC (s +++ toString (length xs)) mode x hst
= (([x:xs],[xbody:xsbody]),hst)
