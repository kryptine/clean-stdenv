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

horlistHGEC :: String [a] HSt -> ([a],(Body,HSt)) | gHGEC{|*|} a & gUpd{|*|}  a & gPrint{|*|} a & gParse{|*|} a 
horlistHGEC s [] hst  = ([],(EmptyBody,hst))
horlistHGEC s [x:xs] hst
# (xs,(xsbody,hst)) 	= horlistHGEC s xs hst
# (x, (xbody,   hst))  	= mkHGEC (s +++ toString (length xs)) id x hst
= ([x:xs],(xbody <-> xsbody,hst))
	
vertlistHGEC :: String [a] HSt -> ([a],(Body,HSt)) | gHGEC{|*|} a & gUpd{|*|}  a & gPrint{|*|} a & gParse{|*|} a 
vertlistHGEC s [] hst  = ([],(EmptyBody,hst))
vertlistHGEC s [x:xs] hst
# (xs,(xsbody,hst)) 	= vertlistHGEC s xs hst
# (x, (xbody,   hst))  	= mkHGEC (s +++ toString (length xs)) id x hst
= ([x:xs],(xbody <|> xsbody,hst))

table_hv_HGEC :: String [[a]] HSt -> ([[a]],(Body,HSt)) | gHGEC{|*|} a & gUpd{|*|}  a & gPrint{|*|} a & gParse{|*|} a 
table_hv_HGEC s [] hst = ([],(EmptyBody,hst))
table_hv_HGEC s [x:xs] hst
# (xs,(xsbody,hst)) 	= table_hv_HGEC s xs hst
# (x, (xbody,  hst))  	= horlistHGEC (s +++ toString (length xs)) x hst
= ([x:xs],(xbody <|> xsbody,hst))



