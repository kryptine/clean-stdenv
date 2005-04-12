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

	updCounter (n,CHPressed,_)  = (n - one,down,up)
	updCounter (n,_,CHPressed) 	= (n + one,down,up)
	updCounter else 			= else

	up 		= CHButton (defpixel / 6) "+"
	down	= CHButton (defpixel / 6) "-"


horlist2HGEC :: !String !HMode a ![a] !*HSt -> (([a],!BodyTag),!*HSt) | gHGEC{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} a
horlist2HGEC s mode defaultval list hst 
# ((fun,butbody),hst) 	= TableFuncBut  s mode [[(but "-", less),(but "+", more)]] hst
# ((nlist,nbody),hst) 	= horlistHGEC s mode (fun list) hst  
= ((nlist,butbody <||> nbody),hst)
where
	but s =  CHButton (defpixel / 6) s

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

TableFuncBut :: !String !HMode ![[(CHButton, a -> a)]] !*HSt -> ((a -> a,!BodyTag),!*HSt)
TableFuncBut s mode [] hst = ((id,EmptyBody),hst)
TableFuncBut s mode [row:rows] hst 
# ((rowsfun,rowsb),hst) = TableFuncBut s mode rows hst
# ((rowfun,rowb),  hst)	= RowFuncBut s mode row hst
= ((rowfun o rowsfun,rowb <||> rowsb),hst)
where
	RowFuncBut :: !String !HMode [(CHButton, a -> a)] !*HSt -> ((a -> a,!BodyTag),!*HSt)
	RowFuncBut s mode [] hst = ((id,EmptyBody),hst)
	RowFuncBut s mode [x:xs] hst 
	# ((rowfun,rowb),hst) 	= RowFuncBut s mode xs hst
	# ((fun,oneb)   ,hst)	= FuncBut s mode x hst
	= ((fun o rowfun,oneb <=> rowb),hst)

FuncBut :: !String !HMode !(CHButton, a -> a) !*HSt -> ((a -> a,!BodyTag),!*HSt)
FuncBut s mode (button=:CHButton _ name ,cbf) hst = mkViewHGEC (s +++ name) mode bimap id hst
where
	bimap =	{ toHGEC 	= \_ _	-> button
			, updHGEC	= \_ v -> v
			, fromHGEC	= \_ but -> case but of 
									CHPressed -> cbf
									_		  -> id
			, resetHGEC	= Just (\_ 	-> button)
			}
FuncBut s mode (button=:ChButtonPict _ ref ,cbf) hst = mkViewHGEC (s +++ ref) mode bimap id hst
where
	bimap =	{ toHGEC 	= \_ _	-> button
			, updHGEC	= \_ v -> v
			, fromHGEC	= \_ but -> case but of 
									CHPressed -> cbf
									_		  -> id
			, resetHGEC	= Just (\_ 	-> button)
			}
FuncBut s mode (CHPressed,cbf) hst = FuncBut s mode (CHButton 10 "??",cbf) hst

ListFuncBut :: !String !HMode [(CHButton, a -> a)] !*HSt -> ((a -> a,![BodyTag]),!*HSt)
ListFuncBut s mode [] hst = ((id,[]),hst)
ListFuncBut s mode [x:xs] hst 
# ((rowfun,rowb),hst) 	= ListFuncBut s mode xs hst
# ((fun,oneb)   ,hst)	= FuncBut s mode x hst
= ((fun o rowfun,[oneb:rowb]),hst)

ListFuncCheckBox :: !Bool !String !HMode [(CheckBox, Bool -> [Bool] -> a -> a)] !*HSt 
									-> (((a -> a,[Bool]),![BodyTag]),!*HSt)
ListFuncCheckBox init s mode defs hst 
# (((f,bools),body),hst) = ListFuncCheckBox` init s mode defs hst
= (((f bools,bools),body),hst)
where
	ListFuncCheckBox` :: !Bool !String !HMode [(CheckBox, Bool -> [Bool] -> a -> a)] !*HSt 
										-> ((([Bool] -> a -> a,[Bool]),![BodyTag]),!*HSt)
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
		
			docbf (CHChecked name) 		= cbf True
			docbf (CHNotChecked name) 	= cbf False
		
			toggle (CHChecked name) 	= CHNotChecked name
			toggle (CHNotChecked name) 	= CHChecked name
	
		
			s` = s +++ case checkbox of 
							(CHChecked name) = name
							(CHNotChecked name) = name

listHGEC 	:: !String !HMode ![a] 	!*HSt -> (([a],![BodyTag]),!*HSt) 	| gHGEC{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} a
listHGEC s mode [] hst  = (([],[]),hst)
listHGEC s mode [x:xs] hst
# ((xs,xsbody),hst) = listHGEC s mode xs hst
# ((x, xbody), hst) = mkEditHGEC (s +++ toString (length xs)) mode x hst
= (([x:xs],[xbody:xsbody]),hst)
