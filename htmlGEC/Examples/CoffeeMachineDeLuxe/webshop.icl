module webshop

import StdEnv, StdHtml

import CDdatabaseHandler

// demo application showing a web shop programmed in Clean using the iData - HtmlGec library
// MJP 2005

derive gForm  PageMode, Item, CD, Track, Duration, []
derive gUpd   PageMode, Item, CD, Track, Duration, []
derive gPrint PageMode, Item, CD, Track, Duration
derive gParse PageMode, Item, CD, Track, Duration

:: PageMode 	= HomePage | ShopPage | BasketPage | OrderInfoPage

Start world 
#  (world,items) = readCDdatabase world
= doHtml (webshopentry items) world

webshopentry database hst
# ((setpage,pagebutbody),hst) 	= ListFuncBut False "pagebut" Edit pagebuttons hst	
# ((curpage,_)	  		,hst) 	= mkStoreForm "curpage" setpage ShopPage hst
# (page,hst) 					= case curpage of
									HomePage 		-> doHomePage database hst
									ShopPage 		-> doShopPage database hst
									BasketPage 		-> doBasketPage database hst
									OrderInfoPage 	-> doInfoPage database hst
= mkHtml "My Web Shop"
		[ STable [] [[H1 [] "Welcome to Clean's Web Shop":pagebutbody]]
		, Hr []
		, Br
		, BodyTag page
//		, traceHtmlInput
		] hst
where
	pagebuttons  = 
		[ (but "Home", 		\page -> HomePage)
		, (but "Shop",		\page -> ShopPage)
		, (but "Basket", 	\page -> BasketPage)
		, (but "OrderInfo", \page -> OrderInfoPage)
		]

doHomePage database hst
# (_,hst)						= basketstore id hst	
# (_,hst) 						= browsestore id hst
= (	[ bTxt "HomePage"
	], hst)

doShopPage database hst
# ((next,browsebuttons)	,hst)	= ListFuncBut False "br" Edit (browseButtons (length database)) hst
# ((i,_)	,hst) 				= browsestore next hst
# ((addCD,addbuttons)	,hst)	= ListFuncBut False "items" Edit (addToBasketButtons [i..i+step-1]) hst
# ((basket,_)	  		,hst) 	= basketstore addCD hst
# ((infonr,infobuttons)	,hst)	= ListFuncBut False "info" Edit (informationButtons [i..i+step-1]) hst
= (	[ bTxt "ShopPage"
	, Br
	, Br
	, mkTable (database%(i,i+step-1)) infobuttons addbuttons 
	, Br
	, mkrow browsebuttons
	, Br
	, bTxt ("number of items found: " +++ (toString (length database)))
	, Br
	, showBasketTop database basket
	, Br
	, if (infonr -1  >= 0) (bTxt ("information requested of item " +++ toString (infonr -1))) (bTxt "?")
	], hst)

doBasketPage database hst
# (_,hst) 					= browsestore id hst
# ((basket,_),hst)			= basketstore id hst	
# ((delCD,delbuttons),hst)	= ListFuncBut False "items" Edit (removeFromBasketButtons basket) hst
# ((basket,_),hst)			= basketstore delCD hst	
# ((infonr,infobuttons)	,hst)	= ListFuncBut False "binfo" Edit (informationButtons basket) hst
= (	[ bTxt "BasketPage"
	, Br
	, bTxt "Current contents of your basket:"
	, Br, Br
	, mkTable [database!!itemnr \\ itemnr <- basket] infobuttons delbuttons
	, if (infonr -1  >= 0) (bTxt ("information requested of item " +++ toString (infonr -1))) (bTxt "?")
	], hst)

doInfoPage database hst
# (_,hst)						= basketstore id hst	
# (_,hst) 						= browsestore id hst
= (	[ bTxt "Order Information"
	], hst)


// stores

:: DataBase		:== [(Item,CD)]
:: Basket		:== [Int]			// item nrs selected
:: Selection	:== [(Item,CD)]		// selection of database items

basketstore :: (Basket -> Basket) *HSt -> ((Basket,BodyTag),!*HSt)
basketstore f hst = mkStoreForm "basket" f [] hst

browsestore :: (Int -> Int) *HSt -> ((Int,BodyTag),!*HSt)
browsestore f hst = mkStoreForm "browse" f 0 hst

// buttons

addToBasketButtons :: [Int] -> [(Button,Basket -> Basket)]
addToBasketButtons basket = [(butp "basket.gif" ,\basket -> [itemnr:basket]) \\ itemnr <- basket]

removeFromBasketButtons :: Basket -> [(Button,Basket -> Basket)]
removeFromBasketButtons basket = [(butp "trash.gif" , removeFromBasket itemnr) \\ itemnr <- basket]
where
	removeFromBasket :: Int Basket -> Basket
	removeFromBasket itemnr [] = []
	removeFromBasket itemnr [bitemnr:items]
	| itemnr == bitemnr 	= items
	| otherwise = [bitemnr: removeFromBasket itemnr items]

informationButtons :: [Int] -> [(Button,Int -> Int)]
informationButtons basket = [(butp "info.gif" ,\_ -> itemnr ) \\ itemnr <- basket]

browseButtons :: Int -> [(Button,Int -> Int)]
browseButtons length = [(but ("-" +++ toString step),prev),(but ("+" +++ toString step),next)]
where
	next i = if (length > i+ step ) (i+step) i
	prev i = if (i >= step ) (i - step) i

step = 5	


// making the tables

showBasketTop :: DataBase Basket -> BodyTag
showBasketTop database [] 			= bTxt "Basket is empty"
showBasketTop database [itemnr:_] 	= BodyTag
							[ bTxt ("Latest item put into basket was: ")
							: ItemRow item ++ CDRow cd
							]
where
	(item,cd) = database!!itemnr

mkTable :: [(Item,CD)] [BodyTag] [BodyTag] -> BodyTag
mkTable items infobuttons deladdbuttons
	= table
		[ ItemHeader ++ CDHeader ++ mkButtonRow EmptyBody ++ mkButtonRow EmptyBody
		: [ItemRow item ++ CDRow cd ++ mkButtonRow infobutton ++ mkButtonRow deladdbutton
			\\ (item,cd) <- items 
			& infobutton <- infobuttons
			& deladdbutton <- deladdbuttons ]
		]				
where
	table rows 	= Table [Tbl_Width tableWidth, Tbl_Bgcolor (`HexColor bgcolor), Tbl_Border 1
						, `Tbl_Events [OnLoad "openwindow()"]
						] 
					[Tr [] row \\ row <- rows]
	tableWidth	= Percent 100
	bgcolor 	= (Hexnum H_6 H_6 H_9 H_9 H_C H_C)

mkButtonRow :: BodyTag -> [BodyTag]
mkButtonRow button
=	[ Td [Td_Width buttonWidth] [button]
	]
where
	buttonWidth 	= Pixels 50


ItemHeader :: [BodyTag]
ItemHeader = 
	mkItemRow "Item" "Prize" "Stock" 

ItemRow :: Item -> [BodyTag] 
ItemRow item
	= mkItemRow (toString item.itemnr) (toString item.prize) (toString item.instock)

mkItemRow :: String String String -> [BodyTag]
mkItemRow itemnr prize instock
=	[ Td [Td_Width itemnrWidth] 	[bTxt itemnr]
	, Td [Td_Width prizeWidth] 		[bTxt prize]
	, Td [Td_Width instockWidth] 	[bTxt instock]
	]
where
	itemnrWidth 	= Pixels 50
	prizeWidth 		= Pixels 50
	instockWidth 	= Pixels 50

CDHeader :: [BodyTag]
CDHeader = 
	mkCDRow "Artist" "Album" "Year" "Duration" 

CDRow :: CD -> [BodyTag]
CDRow cd
	= mkCDRow cd.group cd.album (toString cd.year) (toString cd.totaltime) 

mkCDRow :: String String String String -> [BodyTag]
mkCDRow group album year duration
=	[ Td [Td_Width groupWidth] 		[abTxt group]
	, Td [Td_Width albumWidth] 		[abTxt album]
	, Td [Td_Width yearWidth] 		[abTxt year]
	, Td [Td_Width durationWidth] 	[abTxt duration]
	]
where
	groupWidth 		= Pixels 140
	albumWidth 		= Pixels 400
	yearWidth 		= Pixels 50
	durationWidth 	= Pixels 50

// small utility stuf ...

mkHtml s tags hst 	= (Html (header s) (body tags),hst)
header s 			= Head [`Hd_Std [Std_Title s]] [Hd_Script [] newWindowScript2]
body tags 			= Body [`Batt_Events	 [OnLoad "openwindow()"]] tags
bTxt				= B []
abTxt s				= B [] (allAphaNum s)
allAphaNum string 	= string //{if (isControl s) ' ' s \\ s <-: string }		

but s				= LButton defpixel s
butp s				= PButton (defpixel/2,defpixel/2) ("images/" +++ s)

mkcol bodies 		= foldr (<||>) EmptyBody bodies 
mkrow bodies 		= foldr (<=>)  EmptyBody bodies 
ziprow body1 body2	= [b1 <=> b2 \\ b1 <- body1 & b2 <- body2]

/*
<SCRIPT LANGUAGE="JavaScript">
function openindex()
      { 
OpenWindow=window.open("", "newwin", "height=250, width=250,toolbar=no,scrollbars="+scroll+",menubar=no");
OpenWindow.document.write("<TITLE>Title Goes Here</TITLE>")
OpenWindow.document.write("<BODY BGCOLOR=pink>")
OpenWindow.document.write("<h1>Hello!</h1>")
OpenWindow.document.write("This text will appear in the window!")
OpenWindow.document.write("</BODY>")
OpenWindow.document.write("</HTML>")

OpenWindow.document.close()
self.name="main"
     }
</SCRIPT>

*/
newWindowScript ::  !Html !*File -> *File
newWindowScript html file
=		file <+
		"function openwindow()\r" <+
		"{ OpenWindow=window.open(\"\", \"newwin\", \"height=250, width=250,toolbar=no,menubar=no\");\r" <+
		"OpenWindow.document.write(\"" <+ html <+ "\");\r" <+
		"  OpenWindow.document.close();\r" <+
		"  self.name=\"webshop.php\";\r }"

newWindowScript2 ::  String
newWindowScript2
=		"function openwindow()\r" +++
		"{ OpenWindow=window.open(\"\", \"newwin\", \"height=250, width=250,toolbar=no,menubar=no\");\r" +++
		source2 +++
		"  OpenWindow.document.close();\r" +++
		"  self.name=\"webshop.php\";\r }"
where
	source = foldr (+++) "" ["OpenWindow.document.write(\"" +++ s +++ "\");\r" \\ s <- elem]
	source2 = "OpenWindow.document.write(\"" +++ (foldr (+++) "" elem) +++ "\");\r"
	elem = 	[ "<HTML>"
			, "<TITLE>Title Goes Here</TITLE>"
			, "<BODY BGCOLOR=pink>"
			, "<h1>Everybody should use Clean</h1>"
			, "</BODY>" 
			, "</HTML>"]
