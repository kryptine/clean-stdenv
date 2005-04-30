module webshop

import StdEnv, StdHtml

// demo application showing a web shop programmed in Clean using the iData - HtmlGec library
// MJP 2005

derive gForm  PageMode, CD, []
derive gUpd   PageMode, CD, []
derive gPrint PageMode, CD
derive gParse PageMode, CD

:: PageMode 	= HomePage | ShopPage | BasketPage | OrderInfoPage

Start world  = doHtml webshopentry world

webshopentry hst
# ((setpage,pagebutbody),hst) 	= ListFuncBut False "pagebut" Edit pagebuttons hst	
# ((curpage,_)	  		,hst) 	= mkStoreForm "curpage" setpage HomePage hst
# (page,hst) 					= case curpage of
									HomePage 		-> doHomePage hst
									ShopPage 		-> doShopPage hst
									BasketPage 		-> doBasketPage hst
									OrderInfoPage 	-> doInfoPage hst
= mkHtml "My Web Shop"
		[ STable [] [[H1 [] "Welcome to Clean's Web Shop":pagebutbody]]
		, Hr []
		, Br
		, BodyTag page
		] hst
where
	pagebuttons  = 
		[ (but "Home", 		\page -> HomePage)
		, (but "Shop",		\page -> ShopPage)
		, (but "Basket", 	\page -> BasketPage)
		, (but "OrderInfo", \page -> OrderInfoPage)
		]

doHomePage hst
# (_,hst)						= basketstore id hst	
= (	[ bTxt "HomePage"
	], hst)

doShopPage hst
# ((addCD,addbuttons)	,hst)	= ListFuncBut False "items" Edit (addToBasketButtons database) hst
# ((basket,_)	  		,hst) 	= basketstore addCD hst
= (	[ bTxt "ShopPage"
	, Br, Br
	, showCDs (zip2 database addbuttons)
	, Br
	, showBasketTop basket
	], hst)

doBasketPage hst
# ((basket,_),hst)			= basketstore id hst	
# ((delCD,delbuttons),hst)	= ListFuncBut False "items" Edit (removeFromBasketButtons basket) hst
# ((basket,_),hst)			= basketstore delCD hst	
= (	[ bTxt "BasketPage"
	, Br
	, bTxt "Current contents of your basket:"
	, Br, Br
	, showCDs (zip2 basket delbuttons)
	], hst)

doInfoPage hst
# (_,hst)						= basketstore id hst	
= (	[ bTxt "Order Information"
	], hst)

// CD test database

database =	[ CD 12 "pink floyd"   1998 "atom heart mother" 750
			, CD 15 "roger waters" 1998 "in the flesh"      195
			]

// basket store related

:: Basket		:== [CD]
:: DataBase		:== [CD]

:: CD			= CD ItemNumber Artist Year Title Prize
:: ItemNumber	:== Int
:: Artist		:== String
:: Year			:== Int
:: Title		:== String
:: Prize		:== Int  // In Eurocents

basketstore :: (Basket -> Basket) *HSt -> ((Basket,BodyTag),!*HSt)
basketstore f hst = mkStoreForm "basket" f [] hst

addToBasketButtons :: DataBase -> [(Button,Basket -> Basket)]
addToBasketButtons database = [addToBasketButton item \\ item <- database]
where
	addToBasketButton :: CD -> (Button,Basket -> Basket)
	addToBasketButton (CD j a y t p) = (butp "basket.gif" , addToBasket database j)

	addToBasket :: DataBase Int Basket -> Basket
	addToBasket [] i basket = basket
	addToBasket [cd=:(CD j a y t p):cds] i basket
	| i == j 	= [cd:basket]
	| otherwise = addToBasket cds i basket

removeFromBasketButtons :: Basket -> [(Button,Basket -> Basket)]
removeFromBasketButtons database = [removeFromBasketButton item \\ item <- database]
where
	removeFromBasketButton :: CD -> (Button,Basket -> Basket)
	removeFromBasketButton (CD j a y t p) = (butp "trash.gif" , removeFromBasket database j)

	removeFromBasket :: Basket Int Basket -> Basket
	removeFromBasket [] i basket = basket
	removeFromBasket [cd=:(CD j a y t p):cds] i basket
	| i == j 	= cds
	| otherwise = [cd: removeFromBasket cds i basket]

showBasketTop :: Basket -> BodyTag
showBasketTop [] 		= bTxt "Basket is empty"
showBasketTop [x:xs] 	= BodyTag
							[ bTxt ("Latest item put into basket was: ")
							, CDRow (x,EmptyBody)
							]

showCDs :: [(CD,BodyTag)] -> BodyTag
showCDs items = 
	BodyTag [ mkCDRow "ITEM" "ARTIST" "YEAR" "ALBUM" "PRICE" EmptyBody
			: map CDRow items
			]

CDRow :: (CD,BodyTag) -> BodyTag
CDRow (CD itemNumber artist year title prize,body)
	= mkCDRow (toString itemNumber) artist (toString year) title (toString prize) body

mkCDRow itemNumber artist year title prize body
=	Table [Tbl_Width tableWidth, Tbl_Bgcolor (`HexColor bgcolor), Tbl_Border 1]
		[ Td [Td_Width itemWidth] 	[bTxt itemNumber]
		, Td [Td_Width artistWidth] [bTxt artist]
		, Td [Td_Width titleWidth] [bTxt title]
		, Td [Td_Width yearWidth] [bTxt year]
		, Td [Td_Width prizeWidth] [bTxt prize]
		, Td [Td_Width basketWidth] [body]
		]
where
	bgcolor 	=	(Hexnum H_6 H_6 H_9 H_9 H_C H_C)
	tableWidth 	= Pixels 750
	itemWidth 	= Pixels 55
	artistWidth = Pixels 140
	yearWidth 	= Pixels 50
	titleWidth 	= Pixels 400
	prizeWidth 	= Pixels 55
	basketWidth = Pixels 50


// small utility stuf ...

mkHtml s tags hst 	= (Html (header s) (body tags),hst)
header s 			= Head [`Hd_Std [Std_Title s]] [] 
body tags 			= Body [] tags
bTxt				= B []

but s				= LButton defpixel s
butp s				= PButton (defpixel/2,defpixel/2) ("images/" +++ s)

mkcol bodies 		= foldr (<||>) EmptyBody bodies 
mkrow bodies 		= foldr (<=>)  EmptyBody bodies 
ziprow body1 body2	= [b1 <=> b2 \\ b1 <- body1 & b2 <- body2]