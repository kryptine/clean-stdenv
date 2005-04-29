module webshop

import StdEnv
import StdHtml

derive gForm  PageMode, []
derive gUpd   PageMode, []
derive gPrint PageMode
derive gParse PageMode

:: PageMode = HomePage | ShopPage | BasketPage
:: Item		= Item Int

Start world  = doHtml webshopentry world

webshopentry hst
# ((setpage,pagebutbody),hst) 	= ListFuncBut False "pagebut" Edit pagebuttons hst	
# ((curpage,_)	  		,hst) 	= mkStoreForm "curpage" setpage HomePage hst
# (page,hst) 					= case curpage of
									HomePage 	-> doHomePage hst
									ShopPage 	-> doShopPage hst
									BasketPage 	-> doBasketPage hst
= mkHtml "My Web Shop"
		[ STable [] [[H1 [] "Welcome to Clean's Web Shop":pagebutbody]]
		, Hr []
		, Br
		, BodyTag page
		] hst
where
	pagebuttons  = 
		[ (but "Home", 		\page -> HomePage)
		, (but "toShop",	\page -> ShopPage)
		, (but "toBasket", 	\page -> BasketPage)
		]

doHomePage hst
# (_,hst)						= basketstore id hst	
= (	[ bTxt "HomePage"
	], hst)

doShopPage hst
# ((bought,butlist)	,hst)		= ListFuncBut False "items" Edit itembuttons hst
# ((basket,_)	  		,hst) 	= basketstore (removeDup o bought) hst
= (	[ bTxt "ShopPage"
	, Br, Br
	, mkcol (ziprow butlist [bTxt ("  Item " +++ toString i) \\ i <- [1..]])
	, Br
	, case basket of 
			[] -> bTxt "Basket is empty"
			[x:xs] -> bTxt ("Latest item put into basket was item nr " +++ toString x)
	], hst)
where
	itembuttons = [(but ("Put" +++ toString i), \list -> [i:list]) \\ i <- [1..10]]		

doBasketPage hst
# ((basket,_),hst)				= basketstore id hst	
= (	[ bTxt "BasketPage"
	, Br
	, bTxt "You have put the following items in the basket:"
	, Br, Br
	, mkcol (map toHtml basket)
	], hst)


basketstore :: ([Int] -> [Int]) *HSt -> (([Int],BodyTag),!*HSt)
basketstore f hst = mkStoreForm "basket" f [] hst


// small utility stuf ...

mkHtml s tags hst 	= (Html (header s) (body tags),hst)
header s 			= Head [`Hd_Std [Std_Title s]] [] 
body tags 			= Body [] tags
bTxt				= B []
but s				= LButton defpixel s
mkcol bodies 		= foldr (<||>) EmptyBody bodies 
mkrow bodies 		= foldr (<=>)  EmptyBody bodies 
ziprow body1 body2	= [b1 <=> b2 \\ b1 <- body1 & b2 <- body2]