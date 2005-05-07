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

:: Basket		:== [Int]			// item nrs selected
:: CD_Selection	:== CD_Database		// selection of database items

Start world 
#  (world,items) = readCD_Database world
= doHtml (webshopentry items) world

// main entry shop

webshopentry database hst
# hst 			= instalGlobalStores hst
# (dopage,hst) 	= ListFuncBut False "pagebut" Edit pagebuttons hst	
# (curpage,hst) = mkStoreForm "curpage" dopage.value HomePage hst
# (page,hst) 	= case curpage.value of
					HomePage 	-> doHomePage database hst
					ShopPage 	-> doShopPage database hst
					BasketPage 	-> doBasketPage database hst
					OrderPage 	-> doOrderPage database hst
= (mkHtml "My Web Shop"
		[ STable [] [[H1 [] "Welcome to Clean's Web Shop":dopage.body]]
		, Hr []
		, Br
		, BodyTag page
		, Br
//		, traceHtmlInput
		], hst)
where
	pagebuttons  = 
		[ (but "Home", 		\page -> HomePage)
		, (but "Shop",		\page -> ShopPage)
		, (but "Basket", 	\page -> BasketPage)
		, (but "OrderInfo", \page -> OrderPage)
		]

// stores which are shared between the pages
// wil keep information persistent when switching between pages
// each store can be addressed as wanted in any page as desired as well

instalGlobalStores :: *HSt -> *HSt
instalGlobalStores hst
# (_,hst) 		= browsestore id hst
# (_,hst)		= searchstore hst
# (_,hst)		= basketstore id hst	
# (_,hst)		= optionstore hst
# (_,hst)		= stepstore hst
= hst

basketstore :: (Basket -> Basket) *HSt -> (Form Basket,!*HSt)
basketstore f hst = mkStoreForm "basket" f [] hst

browsestore :: (Int -> Int) *HSt -> (Form Int,!*HSt)
browsestore f hst = mkStoreForm "browse" f 0 hst

searchstore :: *HSt -> (Form String,!*HSt)
searchstore hst = mkEditForm "searchstring" Edit "" hst

optionstore :: *HSt -> (Form (SearchOption -> SearchOption,Int),!*HSt)
optionstore hst = FuncMenu -1 "searchoption" Edit optionbuttons hst
where
	optionbuttons :: [(String,SearchOption -> SearchOption)]
	optionbuttons = [("Album", 	\_ 		-> AnyAlbum)
					,("Artist", \_ 		-> AnyArtist)
					,("Song", 	\_ 		-> AnySong)]

stepstore :: *HSt -> (Form Int,!*HSt)
stepstore hst = mkEditForm "stepsize" Edit 5 hst

// home page

doHomePage database hst
= (	[ bTxt "HomePage"
	], hst)

// shop page

doShopPage database hst
# (searchstring,hst)= searchstore hst
# (option,hst)		= optionstore hst
# (found,selection)	= searchDatabase ([AnyAlbum,AnyArtist,AnySong]!!(snd (option.value))) (searchstring.value) database
# (step,hst)		= stepstore hst
# (nextprev	,hst)	= ListFuncBut False  "browsebuttons" Edit (browseButtons step.value (length selection)) hst
# (i,hst) 			= browsestore (nextprev.value o \i -> if (searchstring.changed || option.changed) 0 i) hst
# (add,hst)			= ListFuncBut False "items" Edit (addToBasketButtons i.value step.value selection) hst
# (basket,hst) 		= basketstore add.value hst
# (info,hst)		= ListFuncBut False "info" Edit (informationButtons [item.itemnr \\ (item,cd) <- selection]%(i.value,i.value+step.value-1)) hst
= (	[ STable [] [[bTxt "Search:",toBody option]
				,[bTxt "Name:",  toBody searchstring, if found (bTxt (toString (length selection) +++ " Items Found"))
											     			   (bTxt "No Items Found")]
				,[bTxt "Browse Step:",toBody step]
				,[nextprev.body!!0,nextprev.body!!1]
				]
	, Br, Br
	, mkTable (selection%(i.value,i.value+step.value-1)) info.body add.body 
	, Br, Br
	, showBasketTop database basket.value
	, doScript database (info.value -1)
	], hst)
where
	
	browseButtons :: Int Int -> [(Button,Int -> Int)]
	browseButtons step length = [(but ("-" +++ toString step),prev),(but ("+" +++ toString step),next)]
	where
		next i = if (length > i+ step ) (i+step) i
		prev i = if (i >= step ) (i - step) i

	addToBasketButtons :: Int Int CD_Selection -> [(Button,Basket -> Basket)]
	addToBasketButtons i step selection 
		= [(butp "basket.gif" ,\basket -> [item.itemnr:basket]) \\ (item,cd) <- selection]%(i,i+step-1)

	showBasketTop :: CD_Database Basket -> BodyTag
	showBasketTop database [] 			= bTxt "Basket is empty"
	showBasketTop database [itemnr:_] 	= BodyTag
								[ bTxt ("Latest item put into basket was: ")
								, Br
								: ItemRow item ++ CDRow cd
								]
	where
		(item,cd) = database!!itemnr

informationButtons :: [Int] -> [(Button,Int -> Int)]
informationButtons basket = [(butp "info.gif" ,\_ -> itemnr ) \\ itemnr <- basket]

// basket page

doBasketPage database hst
# (basket,hst)	= basketstore id hst	
# (delete,hst)	= ListFuncBut False "items" Edit (removeFromBasketButtons basket.value) hst
# (nbasket,hst)	= basketstore delete.value hst	
# (info,hst)	= ListFuncBut False "binfo" Edit (informationButtons nbasket.value) hst
= (	[ bTxt "BasketPage"
	, Br
	, bTxt "Current contents of your basket:"
	, Br, Br
	, mkTable [database!!itemnr \\ itemnr <- nbasket.value] info.body delete.body
	, doScript database (info.value -1)
	], hst)
where
	removeFromBasketButtons :: Basket -> [(Button,Basket -> Basket)]
	removeFromBasketButtons basket = [(butp "trash.gif" , removeFromBasket itemnr) \\ itemnr <- basket]
	where
		removeFromBasket :: Int Basket -> Basket
		removeFromBasket itemnr [] = []
		removeFromBasket itemnr [bitemnr:items]
		| itemnr == bitemnr 	= items
		| otherwise = [bitemnr: removeFromBasket itemnr items]

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
	body = [ STable tableAttr 	[ [bTxt "Item number:", bTxt (toString item.itemnr)] ]
			, Br
			, STable tableAttr	[ [bTxt "Group:", bTxt cd.group]
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
			, STable tableAttr	[ [bTxt ("Buy it now for only " +++ showPrize item.prize)] ]
			]
	(item,cd) = database!!itemnr
	tableAttr = [Tbl_Border 1, Tbl_Bgcolor (`Colorname Yellow)]


myScript :: [BodyTag] -> Script
myScript body = openWindowScript scriptName 800 400 False False True True False False 
					(mkHtml "CD information window" body)

onloadBody = `Batt_Events [OnLoad (SScript scriptName)]

scriptName = "openwindow()"

// Table making functions

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

ziprow body1 body2	= [b1 <=> b2 \\ b1 <- body1 & b2 <- body2]




