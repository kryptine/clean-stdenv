module coffeemachine

import StdEnv
import StdHtml

derive gForm  MachineState, Output, Product
derive gUpd   MachineState, Output, Product
derive gPrint MachineState, Output, Product
derive gParse MachineState, Output, Product

Start world  = doHtml coffeemachine world

coffeemachine hst
# ((input,buttons)		,hst) = ListFuncBut "cb" Edit allbuttons hst	
# (((option,options),_)	,hst) = ListFuncCheckBox False "op" Edit (optionbuttons False False) hst	
# ((machine,_)	  		,hst) = mkStoreHGEC "hidden" (option o input) initmachine hst
# ((_,checkbox)			,hst) = ListFuncCheckBox True "op" Edit (optionbuttons machine.milk machine.sugar) hst	
# ((_,prizebody)  		,hst) = listHGEC "prize" Display prizes hst	
# ((_,statebody)  		,hst) = listHGEC "cont" Display (mstate machine) hst	
= mkHtml "Coffee Machine"
		[ H1 [] "Fancy Coffee Machine ..."
		, Br
		, BodyTag
			[ mkSTable [[bTxt "Content:", bTxt "Value:",bTxt "Input:"]]
			, statebody!!StateMoney <=> mkrow (buttons%MoneyButtons)
			, statebody!!StateBeans <=> buttons!!BeansButton
			, statebody!!StateTrash <=> buttons!!TrashButton
			, Br
			, bTxt "Options: "
			, Br
			, checkbox!!MilkOption  <=> bTxt "Milk"
			, checkbox!!SugarOption <=> bTxt "Sugar"
			, Br
			, mkSTable [[bTxt "Product:", bTxt "Prize:"]]
			, mkcol (buttons%ProductButtons) <=> mkcol prizebody
			, Br
			, bTxt "Message: ", bTxt (print machine.out options)
			] <=> displayMachineImage machine.out 
		, Br
		] hst
where
	allbuttons  = 
		[ (butp "CoffeeBeans.jpg",  \m -> CoffeeMachine (AddBeans,		m))
		, (but "Empty_Trash", 		\m -> CoffeeMachine (EmptyTrash,	m))
		, (but "Coffee",			\m -> CoffeeMachine (Ask Coffee,	m))
		, (but "Capuccino", 		\m -> CoffeeMachine (Ask Capuccino,	m))
		, (but "Espresso", 			\m -> CoffeeMachine (Ask Espresso,	m))
		]
		++
		[moneybuttons n \\ n <- [200, 100, 50, 10, 5]]
		where
			moneybuttons n = (butp (toString n +++ ".gif"), \m -> CoffeeMachine (InsertCoin n, m))

			but s	= LButton defpixel s
			butp s	= PButton (defpixel/2,defpixel/2) ("images/" +++ s)

	optionbuttons milk sugar= 
		[ (check milk  "Milk",  \b _ m -> CoffeeMachine (AskMilk b,  m))
		, (check sugar "Sugar", \b _ m -> CoffeeMachine (AskSugar b, m))
		]
	where
		check True = CBChecked
		check False = CBNotChecked
		
	prizes = [cost Coffee,cost Capuccino, cost Espresso]
	
	mstate machine = 	[ ("money ",machine.money)
						, ("beans ",machine.beans)
						, ("trash ",machine.trash)
						]

	displayMachineImage (Prod x) 	= machineImage 4
	displayMachineImage (Message s) = machineImage 0

	machineImage i	= Img [Img_Src ("images/coffeemachine0" +++ toString i +++ ".jpg"), Img_Width (RelLength 560) ,Img_Height (RelLength 445)]

	mkcol bodies 	= foldr (<||>) EmptyBody bodies 
	mkrow bodies 	= foldr (<=>)  EmptyBody bodies 

	mkHtml s tags hst 	= (Html (header s) (body tags),hst)
	header s 			= Head [`Hd_Std [Std_Title s]] [] 
	body tags 			= Body [] tags
	bTxt				= B []

	print output [milkoption,sugaroption] 
	= printoutput output
	where
		printoutput (Message s)      = s
		printoutput (Prod Coffee)    = "Enjoy your coffee" 		+++ printoptions milkoption sugaroption
		printoutput (Prod Capuccino) = "Enjoy your capuccino"	+++ printoptions milkoption sugaroption
		printoutput (Prod Espresso)  = "Enjoy your espresso"	+++ printoptions milkoption sugaroption

		printoptions milk sugar 
		| milk && sugar = " with milk and sugar"
		| milk  		= " with milk"
		| sugar 		= " with sugar"
		printoptions _ _  = ""
	
	StateMoney 		= 0
	StateBeans 		= 1
	StateTrash 		= 2
	BeansButton		= 0
	TrashButton		= 1
	ProductButtons	= (2,4)
	MoneyButtons 	= (5,9)
	MilkOption		= 0
	SugarOption		= 1


// Coffee machine with standard options ...

::	Client					// Client actions:
	=	InsertCoin Int		// insert a coin of int cents
	|	Ask Product			// ask for product
	|	AddBeans			// add beans in machine
	|	EmptyTrash			// empty bean trash of machine
	|	AskMilk Bool		// milk yes or no
	|	AskSugar Bool		// sugar yes or no
	|	Idle				// does nothing

::	MachineState			// CoffeeMachine:
	=	{ money	:: Int		// nr of coins (maxCoins)
		, beans	:: Int		// amount of beans (maxBeans)
		, trash	:: Int		// amount of bean-trash (maxTrash)
		, milk	:: Bool		// milk wanted
		, sugar :: Bool		// sugar wanted
		, out	:: Output	// output of machine
		}

::	Product	=   Coffee | Capuccino | Espresso
::	Msg		:== String		// Errors or customer-friendly information
::	Output	=   Message Msg | Prod Product

initmachine = 	{ money = 0
				, beans = 6
				, trash = 0
				, milk  = False
				, sugar = False
				, out 	= Message "Welcome."
				} 

//	Finite State Handling of this Coffee Machine

CoffeeMachine :: (Client,MachineState) -> MachineState
CoffeeMachine (InsertCoin n, m=:{money})
	| money >= maxCoins				= { m &                        out = Message "Coin not accepted." }
	| otherwise						= { m & money = money+n,       out = Message "Thank you." }
CoffeeMachine (EmptyTrash, m)		= { m & trash = 0,             out = Message "Trash emptied." }
CoffeeMachine (AddBeans, m=:{beans})                
	| beans > maxBeans-beanBag		= { m &                        out = Message "Too many beans." }
	| otherwise						= { m & beans = beans+beanBag, out = Message "Beans refilled." }
CoffeeMachine (AskMilk b, m)		= { m & milk = b,              out = Message (if b "Milk will be added" "No Milk")}
CoffeeMachine (AskSugar b, m)		= { m & sugar = b,             out = Message (if b "Sugar will be added" "No Sugar")}
CoffeeMachine (Ask p,m=:{money,beans,trash})
	| beans < beancost p			= { m &                        out = Message "Not enough Beans." }
	| money < cost p				= { m &                        out = Message "Not enough money inserted." }
	| trash + ptrash p > maxTrash	= { m &                        out = Message "Trash full." }
	| otherwise						= { m & out   = Prod p
									      , beans = beans - beancost p
									      , money = money - cost p
									      , trash = trash + ptrash p
									      , milk  = False
									      , sugar = False
									  }
CoffeeMachine (_,m)					= m

maxCoins	:== 1000	// max. number of money in machine
maxBeans	:== 20		// max. amount of coffeebeans in machine
maxTrash	:== 5		// max. amount of coffeetrash in machine
beanBag		:== 10		// unit of bean refill

// The number of coins that a product costs
cost :: Product -> Int 
cost Coffee     = 100
cost Capuccino  = 175
cost Espresso   = 150

// The number of beans that a product costs
beancost :: Product -> Int 
beancost Coffee    = 2
beancost Capuccino = 3
beancost Espresso  = 3

// Amount of trash generated by product
ptrash :: Product -> Int 
ptrash _ = 1                      

