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
#  (world,database) = readCD_Database world
= doHtml (webshopentry database) world

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
// and keep information persistent when switching between pages
// each store can be re-addressed again in any page to get access to the contents

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
stepstore hst = mkSelfForm "stepsize" (\step -> if (step > 1) step 5) 5 hst

// home page

doHomePage database hst
= (	[ bTxt "HomePage"
	], hst)

// shop page

doShopPage database hst
# (searchstring,hst)= searchstore hst
# (option,hst)		= optionstore hst
# (step,hst)		= stepstore hst
# (i,hst)			= browsestore id hst

# (found,selection)	= searchDatabase ([AnyAlbum,AnyArtist,AnySong]!!(snd (option.value))) (searchstring.value) database
# (shownext, hst)	= ListFuncBut False  "browsebuttons" Edit (browseButtons i.value step.value (length selection)) hst
# (i,hst) 			= browsestore (shownext.value o \i -> if (searchstring.changed || option.changed) 0 i) hst
# (shownext, hst)	= ListFuncBut False  "browsebuttons" Edit (browseButtons i.value step.value (length selection)) hst

# (add,hst)			= ListFuncBut False "items" Edit (addToBasketButtons i.value step.value selection) hst
# (basket,hst) 		= basketstore add.value hst
# (info,hst)		= ListFuncBut False "info" Edit (informationButtons [item.itemnr \\ {item} <- selection]%(i.value,i.value+step.value)) hst
= (	[ STable [] [[bTxt "Search:",toBody option, Img [Img_Src "images/loep.gif"]]
				,[bTxt "Name:",  toBody searchstring, if found (bTxt (toString (length selection) +++ " Items Found"))
											     			   (bTxt "No Items Found")]
				,[bTxt "#Items:",toBody step]
				]
	, Br
	, STable [] [shownext.body]
	, Br, Br 
	, mkTable (i.value+1,length selection) (selection%(i.value,i.value+step.value)) info.body add.body 
	, Br, Br
	, if (isEmpty basket.value)
			(bTxt "Your Basket is empty")
			(BodyTag [ bTxt ("Last item put into basket was: ")
					 , mkTable (1,length basket.value) [database!!(hd basket.value)] info.body [EmptyBody]
					 ])
	, doScript database (info.value -1)
	], hst)
where
	browseButtons :: Int Int Int -> [(Button,Int -> Int)]
	browseButtons init step length = 
		if (init - range >= 0) 	   [(sbut "--", set (init - range))] [] 
		++
		take nbuttuns [(sbut (toString (i+1)),set i) \\ i <- [start init 0,(start init 0)+step .. length-1]] 
		++ 
		if (init + range <= length + 1) [(sbut "++", set (init + range))] []
	where
		set j i = j
		range = nbuttuns * step
		start i j= if (i < range) j (start (i-range) (j+range))
		nbuttuns = 10
		 

	addToBasketButtons :: Int Int [CD_Selection] -> [(Button,Basket -> Basket)]
	addToBasketButtons i step selection 
		= [(butp "basket.gif" ,\basket -> [data.item.itemnr:basket]) \\ data <- selection]%(i,i+step-1)



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
	, if (isEmpty nbasket.value)
			(bTxt "Your Basket is empty")
			(BodyTag [ bTxt ("Current contents of your basket:")
					, Br, Br
					, mkTable (1,length nbasket.value) [database!!itemnr \\ itemnr <- nbasket.value] info.body delete.body
					])
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
	{item,cd} = database!!itemnr
	tableAttr = [Tbl_Border 1, Tbl_Bgcolor (`Colorname Yellow)]


myScript :: [BodyTag] -> Script
myScript body = openWindowScript scriptName 700 400 False False True True False False 
					(mkHtml "CD information window" body)

onloadBody = `Batt_Events [OnLoad (SScript scriptName)]

scriptName = "openwindow()"

// Function to display contents of selected items, database, basket

mkTable :: (Int,Int) [CD_Selection] [BodyTag] [BodyTag] -> BodyTag
mkTable (cnt,max) items infobuttons deladdbuttons
	= table
		[ empty ++ ItemHeader ++ CDHeader ++ empty ++ empty
		: [CntRow i max ++ ItemRow item ++ CDRow cd ++ mkButtonRow infobutton ++ mkButtonRow deladdbutton
			\\ i <- [cnt..]
			& {item,cd} <- items 
			& infobutton <- infobuttons
			& deladdbutton <- deladdbuttons ]
		]				
where
	table rows 	= Table [Tbl_Width tableWidth, Tbl_Bgcolor (`HexColor bgcolor), Tbl_Border 1] 
					[Tr [] row \\ row <- rows]
	tableWidth	= Percent 100
	ItemHeader 	= mkItemRow "Item" "Prize"
	CDHeader 	= mkCDRow "Artist" "Album" "Year" "Duration" 

	CntRow i max = [Td [Td_Width indexWidth] [bTxt (toString i +++ "/" +++ toString max)]] 
	where indexWidth = Pixels 50

	ItemRow :: Item -> [BodyTag] 
	ItemRow item
		= mkItemRow (toString item.itemnr) (showPrize item.prize)

	mkItemRow :: String String -> [BodyTag]
	mkItemRow itemnr prize
	=	[ Td [Td_Width itemnrWidth] 	[bTxt itemnr]
		, Td [Td_Width prizeWidth] 		[bTxt prize]
		]
	where
		itemnrWidth 	= Pixels 40
		prizeWidth 		= Pixels 100

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

	mkButtonRow button
	=	[Td [Td_Width buttonWidth] [button]] where buttonWidth = Pixels 50
	
	empty = mkButtonRow EmptyBody

// small utility stuf ...

mkHtml s tags 	 	= Html (header s) (body tags)
header s 			= Head [`Hd_Std [Std_Title s]] []
body tags 			= Body [onloadBody] tags
bTxt				= B []
abTxt s				= B [] (allAphaNum s)
allAphaNum string 	= string		

but s				= LButton defpixel s
butp s				= PButton (defpixel/2,defpixel/2) ("images/" +++ s)
sbut s				= LButton (defpixel/3) s

bgcolor 			= (Hexnum H_6 H_6 H_9 H_9 H_C H_C)

ziprow body1 body2	= [b1 <=> b2 \\ b1 <- body1 & b2 <- body2]




