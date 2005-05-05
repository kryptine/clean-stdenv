module webshop

import StdEnv, StdHtml

import CDdatabaseHandler

// demo application showing a web shop programmed in Clean using the iData - HtmlGec library
// MJP 2005

derive gForm  PageMode, Item, CD, Track, Duration, []
derive gUpd   PageMode, Item, CD, Track, Duration, []
derive gPrint PageMode, Item, CD, Track, Duration
derive gParse PageMode, Item, CD, Track, Duration

:: PageMode 	= HomePage | ShopPage | BasketPage | OrderPage

Start world 
#  (world,items) = readCDdatabase world
= doHtml (webshopentry items) world

// main entry shop

webshopentry database hst
# ((setpage,pagebutbody),hst) 	= ListFuncBut False "pagebut" Edit pagebuttons hst	
# ((curpage,_)	  		,hst) 	= mkStoreForm "curpage" setpage ShopPage hst
# (page,hst) 					= case curpage of
									HomePage 	-> doHomePage database hst
									ShopPage 	-> doShopPage database hst
									BasketPage 	-> doBasketPage database hst
									OrderPage 	-> doOrderPage database hst
= (mkHtml "My Web Shop"
		[ STable [] [[H1 [] "Welcome to Clean's Web Shop":pagebutbody]]
		, Hr []
		, Br
		, BodyTag page
//		, traceHtmlInput
		], hst)
where
	pagebuttons  = 
		[ (but "Home", 		\page -> HomePage)
		, (but "Shop",		\page -> ShopPage)
		, (but "Basket", 	\page -> BasketPage)
		, (but "OrderInfo", \page -> OrderPage)
		]

// home page

doHomePage database hst
# (_,hst)						= basketstore id hst	
# (_,hst) 						= browsestore id hst
= (	[ bTxt "HomePage"
	], hst)

// shop page

doShopPage database hst
# ((next,browsebuttons)	,hst)	= ListFuncBut False "br" Edit (browseButtons (length database)) hst
# ((i,_)	,hst) 				= browsestore next hst
# ((addCD,addbuttons)	,hst)	= ListFuncBut False "items" Edit (addToBasketButtons [i..i+step-1]) hst
# ((basket,_)	  		,hst) 	= basketstore addCD hst
# ((infonr,infobuttons)	,hst)	= ListFuncBut False "info" Edit (informationButtons [i..i+step-1]) hst
= (	[ bTxt "ShopPage"
	, Br
	, mkTable (database%(i,i+step-1)) infobuttons addbuttons 
	, Br
	, mkrow browsebuttons
	, Br
	, bTxt ("number of items found : " +++ (toString (length database)))
	, Br, Br
	, showBasketTop database basket
	, doScript database (infonr -1)
	], hst)

// basket page

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
	, doScript database (infonr -1)
	], hst)

// order page

doOrderPage database hst
# (_,hst)						= basketstore id hst	
# (_,hst) 						= browsestore id hst
= (	[ bTxt "Order Information"
	], hst)

// page showing CD information will appear in extra window

doScript database itemnr
| itemnr < 0 = EmptyBody
| otherwise  = Script [] (myScript body)
where
	body = [ STable tableAttr	[ [bTxt "Group:", bTxt cd.group]
								, [bTxt "Album:", bTxt cd.album]
								, [bTxt "Year:",  bTxt (toString cd.year)]
								]
			, Br
			, STable tableAttr 	([ [bTxt ("Track " +++ toString nr)
								  ,bTxt title
								  ,bTxt (toString playtime)
								  ] \\ {nr,title,playtime} <- cd.tracks 
								] ++
								[ [bTxt "Total Time",EmptyBody, bTxt (toString cd.totaltime)]
								])
			, Br
			, STable tableAttr 	[ [bTxt "Item number:", bTxt (toString item.itemnr)] ]
			, Br
			, STable tableAttr	[ [bTxt ("Buy it now for only " +++ showPrize item.prize)] ]
			]
	(item,cd) = database!!itemnr
	tableAttr = [Tbl_Border 1, Tbl_Bgcolor (`Colorname Yellow)]


myScript :: [BodyTag] -> Script
myScript body = openWindowScript scriptName 250 800 False False True True False False 
					(mkHtml "CD information window" body)

onloadBody = `Batt_Events [OnLoad (SScript scriptName)]

scriptName = "openwindow()"

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

// Table making functions

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
	table rows 	= Table [Tbl_Width tableWidth, Tbl_Bgcolor (`HexColor bgcolor), Tbl_Border 1] 
					[Tr [] row \\ row <- rows]
	tableWidth	= Percent 100

	ItemHeader :: [BodyTag]
	ItemHeader = 
		mkItemRow "Item" "Prize" "Stock" 

	CDHeader :: [BodyTag]
	CDHeader = 
		mkCDRow "Artist" "Album" "Year" "Duration" 

	mkButtonRow :: BodyTag -> [BodyTag]
	mkButtonRow button
	=	[ Td [Td_Width buttonWidth] [button]
		]
	where
		buttonWidth 	= Pixels 50

ItemRow :: Item -> [BodyTag] 
ItemRow item
	= mkItemRow (toString item.itemnr) (showPrize item.prize) (toString item.instock)

mkItemRow :: String String String -> [BodyTag]
mkItemRow itemnr prize instock
=	[ Td [Td_Width itemnrWidth] 	[bTxt itemnr]
	, Td [Td_Width prizeWidth] 		[bTxt prize]
	, Td [Td_Width instockWidth] 	[bTxt instock]
	]
where
	itemnrWidth 	= Pixels 40
	prizeWidth 		= Pixels 100
	instockWidth 	= Pixels 50

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

mkHtml s tags 	 	= Html (header s) (body tags)
header s 			= Head [`Hd_Std [Std_Title s]] []
body tags 			= Body [onloadBody] tags
bTxt				= B []
abTxt s				= B [] (allAphaNum s)
allAphaNum string 	= string		

but s				= LButton defpixel s
butp s				= PButton (defpixel/2,defpixel/2) ("images/" +++ s)

bgcolor 			= (Hexnum H_6 H_6 H_9 H_9 H_C H_C)

mkcol bodies 		= foldr (<||>) EmptyBody bodies 
mkrow bodies 		= foldr (<=>)  EmptyBody bodies 
ziprow body1 body2	= [b1 <=> b2 \\ b1 <- body1 & b2 <- body2]




