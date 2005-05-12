module webshop

import StdEnv, StdHtml

import CDdatabaseHandler

// demo application showing a web shop programmed in Clean using the iData - HtmlGec library
// MJP 2005

Start world 
#  (world,database) = readCD_Database world		// read the database (lazily)
= doHtml (webshopentry database) world			// goto the main page

// globally used definitions

derive gForm  CurrentPage, Item, CD, Track, Duration, PersonalData, []
derive gUpd   CurrentPage, Item, CD, Track, Duration, PersonalData, []
derive gPrint CurrentPage, Item, CD, Track, Duration, PersonalData
derive gParse CurrentPage, Item, CD, Track, Duration, PersonalData

:: CurrentPage 	= HomePage | ShopPage | BasketPage | OrderPage | ThanksPage

:: Basket		:== [Int]			// item nrs selected
:: CD_Selection	:== CD_Database		// selection of database items


// storages which are shared between the pages
// and in this way keep information persistent when the user is switching between pages
// each store can be examined again in any page to get access to its contents

:: SharedForms
 =	{ currentPage	:: Form CurrentPage		// page to display
	, index			:: Form Int				// current item number to show
	, step			:: Form Int				// numbers of items to display on a page
	, searchString	:: Form String			// current search string 
	, searchOption	:: Form (SearchOption -> SearchOption,Int) // current option
 	, basket 		:: Form [Int]			// items stored in basket 
	, personalData	:: Form PersonalData	// all data of the costumer
 	} 

sharedForms :: *HSt -> (SharedForms,!*HSt)
sharedForms hst
# (curpage,hst) 		= currentpageForm id hst
# (index,hst) 			= indexForm id hst
# (step,hst)			= stepForm hst
# (searchString,hst)	= searchForm hst
# (searchOption,hst)	= optionForm hst
# (personalData,hst)	= personalDataForm hst
# (basket,hst)	= basketForm id hst	
= ( { currentPage	= curpage
	, index			= index
	, step			= step
	, searchString	= searchString
	, searchOption	= searchOption
 	, basket 		= basket 
	, personalData	= personalData
 	},hst) 

currentpageForm :: (CurrentPage -> CurrentPage) *HSt -> (Form CurrentPage,!*HSt)
currentpageForm f hst = mkStoreForm "curpageswirch" f HomePage hst

indexForm :: (Int -> Int) *HSt -> (Form Int,!*HSt)
indexForm f hst = mkStoreForm "index" f 0 hst

stepForm :: *HSt -> (Form Int,!*HSt)
stepForm hst = mkSelfForm "stepsize" (\step -> if (step > 0 && step < 10) step 5) 5 hst

searchForm :: *HSt -> (Form String,!*HSt)
searchForm hst = mkEditForm "searchstring" Edit "" hst

optionForm :: *HSt -> (Form (SearchOption -> SearchOption,Int),!*HSt)
optionForm hst = FuncMenu -1 "searchoption" Edit optionbuttons hst
where
	optionbuttons :: [(String,SearchOption -> SearchOption)]
	optionbuttons = [("Album", 	\_ 		-> AnyAlbum)
					,("Artist", \_ 		-> AnyArtist)
					,("Song", 	\_ 		-> AnySong)]

basketForm :: (Basket -> Basket) *HSt -> (Form Basket,!*HSt)
basketForm f hst = mkStoreForm "basket" f [] hst

personalDataForm hst = mkEditForm "personal" Edit initPersInfo hst

:: PersonalData =
	{ name 				:: TextInput
	, address			:: TextInput
	, city				:: TextInput
	, state				:: TextInput
	, zipCode			:: (TextInput,TextInput)
	, country			:: PullDownMenu
	, ccCompagny		:: PullDownMenu
	, ccNumber			:: (TextInput,TextInput,TextInput,TextInput)
	, ccExpiringDate	:: (PullDownMenu,PullDownMenu)
	, cardholdersName	:: TextInput
	}	

initPersInfo =	
	{ name 				= TS 30 ""
	, address			= TS 30 ""
	, city				= TS 30 ""
	, state				= TS 30 ""
	, zipCode			= (TI 2 1234,TS 1 "")
	, country			= PullDown (1,100) (0,["Belgium", "Netherlands","United Kingdom"])
	, ccCompagny		= PullDown (1,100) (0,["MasterCard", "VisaCard"])
	, ccNumber			= (TI 2 1234, TI 2 1234, TI 2 1234,TI 2 1234)
	, ccExpiringDate	= ( PullDown (1,40) (0,[toString m \\ m <- [1 .. 12]])
						  , PullDown (1,60) (0,[toString y \\ y <- [2005 .. 2014]])
						  )
	, cardholdersName	= TS 30 ""
	}	

showBasket :: Bool [Int] [CD_Database] [BodyTag] [BodyTag] -> BodyTag
showBasket onlytop basket database infobuts deletebuts
| isEmpty basket = 	bTxt "Your Basket is empty"
| onlytop = 		BodyTag
				  	[ bTxt "Last Item put into basket:"
					, mkTable (1,length basket) [database!!(hd basket)] infobuts deletebuts
					]
| otherwise			= BodyTag
				  	[ bTxt "Contents of your basket:"
					, mkTable (1,length basket) [database!!itemnr \\ itemnr <- basket] infobuts deletebuts
					, Br
					, STable [] [[ bTxt "Total Prize:"
								, toHtml (showPrize (sum [(database!!itemnr).item.prize \\ itemnr <- basket]))
								]]
					]

// main entry of the shop

webshopentry database hst
# (sharedForms,hst) = sharedForms hst					// include all shared forms
# (selPage,hst) 	= pageSelectionForm hst				// is a new page selected
# (curPage,hst) 	= currentpageForm selPage.value hst // determine current page
# (page,hst) 		= case curPage.value of				// include this page
						HomePage 	-> doHomePage   database sharedForms hst
						ShopPage 	-> doShopPage   database sharedForms hst
						BasketPage 	-> doBasketPage database sharedForms hst
						OrderPage 	-> doOrderPage  database sharedForms hst
= (mkHtml "My Web Shop"
		[ STable [] [[Img [Img_Src "images/cdshoptitle.gif"]:selPage.body]]
		, Hr []
		, Br
		, BodyTag page		// code of selected page
		, Br
//		, traceHtmlInput
		], hst)
where
	pageSelectionForm hst = ListFuncBut False "pagebut" Edit pagebuttons hst
	where
		pagebuttons  = 
			[ (but "Home", 		\page -> HomePage)
			, (but "Shop",		\page -> ShopPage)
			, (but "Basket", 	\page -> BasketPage)
			, (but "OrderInfo", \page -> OrderPage)
			]

// home page

doHomePage database sf hst
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

doShopPage database sf hst
# (found,selection)	= searchDatabase ([AnyAlbum,AnyArtist,AnySong]!!(snd (sf.searchOption.value))) (sf.searchString.value) database
# (shownext, hst)	= browserForm sf.index.value sf.step.value (length selection) hst
# (nindex,hst) 		= indexForm (shownext.value o \i -> if (sf.searchString.changed || sf.searchOption.changed) 0 sf.index.value) hst
# (shownext, hst)	= browserForm nindex.value sf.step.value (length selection) hst
# (add,hst)			= addToBasketForm nindex.value sf.step.value selection hst
# (info,hst)		= InformationForm "listinfo" ([item.itemnr \\ {item} <- selection]%(nindex.value,nindex.value+sf.step.value)) hst
# (basket,hst) 		= basketForm add.value hst
# (binfo,hst)		= InformationForm "basketinfo" basket.value hst
= (	[([ STable [] [[bTxt "Search:",toBody sf.searchOption, Img [Img_Src "images/loep.gif"]]
				,[bTxt "Name:",  toBody sf.searchString, if found (bTxt (toString (length selection) +++ " Items Found"))
											     			   (bTxt "No Items Found")]
				,[bTxt "#Items:",toBody sf.step]
				]]
	  <=>
		 [STable [] [shownext.body]])
	, Br, Br 
	, mkTable (nindex.value+1,length selection) (selection%(nindex.value,nindex.value+sf.step.value)) info.body add.body 
	, Br, Br
	, showBasket True basket.value database info.body [EmptyBody]
	, doScript database (info.value -1)
	, doScript database (binfo.value -1)
	], hst)
where
	browserForm :: !Int !Int !Int *HSt -> (Form (Int -> Int),!*HSt) 
	browserForm index step length hst
		= ListFuncBut False  "browserbuttons" Edit (browserButtons index step length) hst
	where
		browserButtons :: !Int !Int !Int -> [(Button,Int -> Int)]
		browserButtons init step length = 
			if (init - range >= 0) 	   [(sbut "--", set (init - range))] [] 
			++
			take nbuttuns [(sbut (toString (i+1)),set i) \\ i <- [startval,startval+step .. length-1]] 
			++ 
			if (startval + range < length - 1) [(sbut "++", set (startval + range))] []
		where
			set j i = j
			range = nbuttuns * step
			start i j= if (i < range) j (start (i-range) (j+range))
			nbuttuns = 10
			startval = start init 0

	addToBasketForm :: !Int !Int [CD_Selection] *HSt -> (Form (Basket -> Basket),!*HSt)
	addToBasketForm index step selection hst
		= ListFuncBut False "additems" Edit (addToBasketButtons index step selection) hst
	where
		addToBasketButtons :: Int Int [CD_Selection] -> [(Button,Basket -> Basket)]
		addToBasketButtons i step selection 
			= [(butp "basket.gif" ,\basket -> [data.item.itemnr:basket]) \\ data <- selection]%(i,i+step-1)

InformationForm :: String [Int] *HSt -> (Form (Int -> Int),!*HSt)
InformationForm formid itemlist hst = ListFuncBut False formid Edit (informationButtons itemlist) hst
where
	informationButtons :: [Int] -> [(Button,Int -> Int)]
	informationButtons itemlist = [(butp "info.gif" ,\_ -> itemnr ) \\ itemnr <- itemlist]

// basket page

doBasketPage database sf hst
# (delete,hst)	= ListFuncBut False "delitems" Edit (removeFromBasketButtons sf.basket.value) hst
# (nbasket,hst)	= basketForm delete.value hst	
# (info,hst)	= InformationForm "basketinfo2" nbasket.value hst


# (order,hst) 	= ListFuncBut False "buybut" Edit [(but "toOrder",\_ -> OrderPage)] hst	
# (_,hst) 		= currentpageForm order.value hst   // this is too much ????
= ( [ showBasket False nbasket.value database info.body delete.body
	, doScript database (info.value -1)
	, toBody order
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

doOrderPage database sf hst
# persData = sf.personalData
# (confirm,hst) 	= ListFuncBut False "confirm" Edit [(but "confirm",\_ -> ThanksPage)] hst	
# (_,hst)			= currentpageForm confirm.value hst
= (	[ showBasket False sf.basket.value database (repeat EmptyBody) (repeat EmptyBody)
	, Br
	, bTxt "All fields must be filled with your data:"
	, toBody persData
	, Br
	, bTxt "Confirm your order:\t\t", toBody confirm
	], hst)
	
// thanks page

thanksPage hst
= ( [ maptext 	[ "Your order has been processed!"
				, "Thanks for playing with our demo shop."
				, ""
				, "Probably we have to find another way to earn money."
				]
	],hst)
	
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



