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
# ((fun,butbody),hst) 	= assignTableFuncBut  s mode [[(but "-", less),(but "+", more)]] hst
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

assignTableFuncBut :: !String !HMode ![[(CHButton, a -> a)]] !*HSt -> ((a -> a,!BodyTag),!*HSt)
assignTableFuncBut s mode [] hst = ((id,EmptyBody),hst)
assignTableFuncBut s mode [row:rows] hst 
# ((rowsfun,rowsb),hst) = assignTableFuncBut s mode rows hst
# ((rowfun,rowb),  hst)	= assignRowFuncBut s mode row hst
= ((rowfun o rowsfun,rowb <||> rowsb),hst)
where
	assignRowFuncBut :: !String !HMode [(CHButton, a -> a)] !*HSt -> ((a -> a,!BodyTag),!*HSt)
	assignRowFuncBut s mode [] hst = ((id,EmptyBody),hst)
	assignRowFuncBut s mode [x:xs] hst 
	# ((rowfun,rowb),hst) 	= assignRowFuncBut s mode xs hst
	# ((fun,oneb)   ,hst)	= assignFuncBut s mode x hst
	= ((fun o rowfun,oneb <=> rowb),hst)

	assignFuncBut :: !String !HMode !(CHButton, a -> a) !*HSt -> ((a -> a,!BodyTag),!*HSt)
	assignFuncBut s mode (button=:CHButton size name ,cbf) hst = mkViewHGEC (s +++ name) mode bimap id hst
	where
		bimap =	{ toHGEC 	= \_ _	-> button
				, updHGEC	= \_ v -> v
				, fromHGEC	= \_ but -> case but of 
										CHPressed -> cbf
										_		  -> id
				, resetHGEC	= Just (\_ 	-> button)
				}
	assignFuncBut s mode (CHPressed,cbf) hst = assignFuncBut s mode (CHButton 10 "??",cbf) hst


