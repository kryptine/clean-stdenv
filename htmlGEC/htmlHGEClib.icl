implementation module htmlHGEClib

// Handy collection of HGEC's
// HGEC library similar to the AGEC lib
// (c) MJP 2005

import StdEnv
import StdHtml

// operators for lay-out of html bodys ...

// Place two bodies next to each other

(<->) infixl 5 :: Body Body -> Body
(<->) b1 b2 =  Table [Tbl_CellPadding 0, Tbl_CellSpacing 0] [[b1,b2]]

// Place second body below first

(<|>) infixl 4	:: Body Body -> Body	// Place a above b
(<|>) b1 b2 =  Table [Tbl_CellPadding 0, Tbl_CellSpacing 0] [[b1],[b2]]

// HGEC collection:

counterHGEC 	:: String (HMode a) a 	HSt -> (a,(Body,HSt)) 		| +, -, one,  gHGEC{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} a
counterHGEC name mode i hst 
# (nc, result) = mkHGEC name (Edit updCounter) (toCounter i) hst
= (fromCounter nc, result)
where
	toCounter n = (n,down,up)

	fromCounter (n,_,_) = n

	updCounter (n,CHPressed,_)  = (n - one,down,up)
	updCounter (n,_,CHPressed) 	= (n + one,down,up)
	updCounter else 			= else

	up 		= CHButton defsize "+"
	down	= CHButton defsize "-"

horlistHGEC :: String (HMode a) [a] HSt -> ([a],(Body,HSt)) | gHGEC{|*|} a & gUpd{|*|}  a & gPrint{|*|} a & gParse{|*|} a 
horlistHGEC s mode [] hst  = ([],(EmptyBody,hst))
horlistHGEC s mode [x:xs] hst
# (xs,(xsbody,hst)) 	= horlistHGEC s mode xs hst
# (x, (xbody,   hst))  	= mkHGEC (s +++ toString (length xs)) (setelem mode) x hst
= ([x:xs],(xbody <-> xsbody,hst))
where
	setelem Set = Set
	setelem _   = Edit id
	
vertlistHGEC :: String (HMode a) [a] HSt -> ([a],(Body,HSt)) | gHGEC{|*|} a & gUpd{|*|}  a & gPrint{|*|} a & gParse{|*|} a 
vertlistHGEC s mode [] hst  = ([],(EmptyBody,hst))
vertlistHGEC s mode [x:xs] hst
# (xs,(xsbody,hst)) 	= vertlistHGEC s mode xs hst
# (x, (xbody,   hst))  	= mkHGEC (s +++ toString (length xs)) (setelem mode) x hst
= ([x:xs],(xbody <|> xsbody,hst))
where
	setelem Set = Set
	setelem _   = Edit id

table_hv_HGEC :: String (HMode a) [[a]] HSt -> ([[a]],(Body,HSt)) | gHGEC{|*|} a & gUpd{|*|}  a & gPrint{|*|} a & gParse{|*|} a 
table_hv_HGEC s mode [] hst = ([],(EmptyBody,hst))
table_hv_HGEC s mode [x:xs] hst
# (xs,(xsbody,hst)) 	= table_hv_HGEC s mode xs hst
# (x, (xbody,  hst))  	= horlistHGEC (s +++ toString (length xs)) (setelem mode) x hst
= ([x:xs],(xbody <|> xsbody,hst))
where
	setelem Set = Set
	setelem _   = Edit id



