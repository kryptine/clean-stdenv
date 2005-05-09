module webshop

import StdEnv, StdHtml

import CDdatabaseHandler

// demo application showing a web shop programmed in Clean using the iData - HtmlGec library
// MJP 2005

derive gForm  CurrentPage, Item, CD, Track, Duration, []
derive gUpd   CurrentPage, Item, CD, Track, Duration, []
derive gPrint CurrentPage, Item, CD, Track, Duration
derive gParse CurrentPage, Item, CD, Track, Duration

:: CurrentPage 	= HomePage | ShopPage | BasketPage | OrderPage

:: Basket		:== [Int]			// item nrs selected
:: CD_Selection	:== CD_Database		// selection of database items

Start world 
#  (world,database) = readCD_Database world
= doHtml (webshopentry database) world

// main entry shop

webshopentry database hst
# (index,hst) 	= indexStore id hst
# (step,hst)	= stepStore hst
# (search,hst)	= searchStore hst
# (option,hst)	= optionStore hst
# (basket,hst)	= basketStore id hst	
# (dopage,hst) 	= ListFuncBut False "pagebut" Edit pagebuttons hst	
# (curpage,hst) = currentpageStore dopage.value hst
# (page,hst) 	= case curpage.value of
					HomePage 	-> doHomePage   curpage (database,basket) (index,step) (search,option) hst
					ShopPage 	-> doShopPage   curpage (database,basket) (index,step) (search,option) hst
					BasketPage 	-> doBasketPage curpage (database,basket) (index,step) (search,option) hst
					OrderPage 	-> doOrderPage  curpage (database,basket) (index,step) (search,option) hst
= (mkHtml "My Web Shop"
		[ STable [] [[Img [Img_Src "images/cdshoptitle.gif"]:dopage.body]]
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

// storages which are shared between the pages
// and in this way keep information persistent when the user is switching between pages
// each store can be examined again in any page to get access to its contents

currentpageStore :: (CurrentPage -> CurrentPage) *HSt -> (Form CurrentPage,!*HSt)
currentpageStore f hst = mkStoreForm "curpage" f HomePage hst

basketStore :: (Basket -> Basket) *HSt -> (Form Basket,!*HSt)
basketStore f hst = mkStoreForm "basket" f [] hst

indexStore :: (Int -> Int) *HSt -> (Form Int,!*HSt)
indexStore f hst = mkStoreForm "index" f 0 hst

searchStore :: *HSt -> (Form String,!*HSt)
searchStore hst = mkEditForm "searchstring" Edit "" hst

optionStore :: *HSt -> (Form (SearchOption -> SearchOption,Int),!*HSt)
optionStore hst = FuncMenu -1 "searchoption" Edit optionbuttons hst
where
	optionbuttons :: [(String,SearchOption -> SearchOption)]
	optionbuttons = [("Album", 	\_ 		-> AnyAlbum)
					,("Artist", \_ 		-> AnyArtist)
					,("Song", 	\_ 		-> AnySong)]

stepStore :: *HSt -> (Form Int,!*HSt)
stepStore hst = mkSelfForm "stepsize" (\step -> if (step > 0) step 5) 5 hst

// home page

doHomePage curpage (database,basket) (index,step) (search,option) hst
= (	[ maptext 	[ "Welkom to the Clean CD shop!"
				, ""
				, "Our Dean wants that we make more money with Clean, otherwise we will be killed."
				, "We have therefore decided to sell Peter's CD collection for bargain prices."
				, ""
				, "By the way, this application also gives a nice demo what you can do with Clean..."
				, "Have fun."
				]
	], hst)

// shop page

doShopPage curpage (database,basket) (index,step) (search,option) hst

# (found,selection)	= searchDatabase ([AnyAlbum,AnyArtist,AnySong]!!(snd (option.value))) (search.value) database

# (shownext, hst)	= ListFuncBut False  "browsebuttons" Edit (browseButtons index.value step.value (length selection)) hst

# (nindex,hst) 		= indexStore (shownext.value o \i -> if (search.changed || option.changed) 0 index.value) hst
# (shownext, hst)	= ListFuncBut False  "browsebuttons" Edit (browseButtons nindex.value step.value (length selection)) hst

# (add,hst)			= ListFuncBut False "additems" Edit (addToBasketButtons nindex.value step.value selection) hst
# (info,hst)		= ListFuncBut False "info" Edit (informationButtons [item.itemnr \\ {item} <- selection]%(nindex.value,nindex.value+step.value)) hst

# (basket,hst) 		= basketStore add.value hst
# (binfo,hst)		= ListFuncBut False "basketinfo" Edit (informationButtons basket.value) hst
= (	[([ STable [] [[bTxt "Search:",toBody option, Img [Img_Src "images/loep.gif"]]
				,[bTxt "Name:",  toBody search, if found (bTxt (toString (length selection) +++ " Items Found"))
											     			   (bTxt "No Items Found")]
				,[bTxt "#Items:",toBody step]
				]]
	  <=>
		 [STable [] [shownext.body]])
	, Br, Br 
	, mkTable (nindex.value+1,length selection) (selection%(nindex.value,nindex.value+step.value)) info.body add.body 
	, Br, Br
	, if (isEmpty basket.value)
			(bTxt "Your Basket is empty")
			(BodyTag [ bTxt ("Last item put into basket was: ")
					 , mkTable (1,length basket.value) [database!!(hd basket.value)] binfo.body [EmptyBody]
					 ])
	, doScript database (info.value -1)
	, doScript database (binfo.value -1)
	], hst)
where
	browseButtons :: Int Int Int -> [(Button,Int -> Int)]
	browseButtons init step length = 
		if (init - range >= 0) 	   [(sbut "--", set (init - range))] [] 
		++
		take nbuttuns [(sbut (toString (i+1)),set i) \\ i <- [startval,startval+step .. length-1]] 
		++ 
		if (startval + range < length) [(sbut "++", set (init + range))] []
	where
		set j i = j
		range = nbuttuns * step
		start i j= if (i < range) j (start (i-range) (j+range))
		nbuttuns = 10
		startval = start init 0

	addToBasketButtons :: Int Int [CD_Selection] -> [(Button,Basket -> Basket)]
	addToBasketButtons i step selection 
		= [(butp "basket.gif" ,\basket -> [data.item.itemnr:basket]) \\ data <- selection]%(i,i+step-1)

informationButtons :: [Int] -> [(Button,Int -> Int)]
informationButtons basket = [(butp "info.gif" ,\_ -> itemnr ) \\ itemnr <- basket]

// basket page

doBasketPage curpage (database,basket) (index,step) (search,option) hst
# (delete,hst)	= ListFuncBut False "delitems" Edit (removeFromBasketButtons basket.value) hst
# (nbasket,hst)	= basketStore delete.value hst	
# (info,hst)	= ListFuncBut False "binfo" Edit (informationButtons nbasket.value) hst

# (order,hst) 	= ListFuncBut False "buybut" Edit [(but "order",\_ -> OrderPage)] hst	
//# (_,hst) 		= currentpageStore order.value hst


= (	[ if (isEmpty nbasket.value)
			(bTxt "Your Basket is empty")
			(BodyTag [ bTxt ("Current contents of your basket:")
					, Br, Br
					, mkTable (1,length nbasket.value) [database!!itemnr \\ itemnr <- nbasket.value] info.body delete.body
					, Br
					, STable [] [[ bTxt "Total Prize:"
								, toHtml (showPrize (sum [(database!!itemnr).item.prize \\ itemnr <- nbasket.value]))
								, EmptyBody, toBody order
								]]
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

doOrderPage curpage (database,basket) (index,step) (search,option) hst
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
maptext	texts		= BodyTag (flatten [[bTxt text, Br] \\ text <- texts])



