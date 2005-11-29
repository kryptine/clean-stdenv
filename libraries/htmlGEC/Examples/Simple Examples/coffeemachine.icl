module coffeemachine

import StdEnv
import StdHtml

derive gForm  Machine, Output, Product
derive gUpd   Machine, Output, Product
derive gPrint Machine, Output, Product
derive gParse Machine, Output, Product

//Start world  = doHtml coffeemachine world
Start world  = doHtmlServer coffeemachine world

coffeemachine hst
# (command,hst)	= TableFuncBut (nFormId "cb") commandbuttons hst	
# (option,hst)	= TableFuncBut (nFormId "ob") optionbuttons  hst	
# (machine,hst)	= mkStoreForm (nFormId "hidden") initmachine (option.value o command.value)  hst
= mkHtml "Coffee Machine"
		[ H1 [] "Coffee Machine: "
		, [toHtml (displaycontents  machine.value)] <=> command.form
		, BodyTag option.form
		, Br
		, B [] (displayoutput machine.value)
		] hst
where
	commandbuttons  = Init
		[	[(but "Insert_Coins",	\m -> CoffeeMachine (InsertCoin,	m))]
		,	[(but "Add_beans",   	\m -> CoffeeMachine (AddBeans,		m))]
		,	[(but "Empty_Trash", 	\m -> CoffeeMachine (EmptyTrash,	m))]
		]

	optionbuttons  = Init
		[	[(but "Coffee",			\m -> CoffeeMachine (Ask Coffee,	m))
			,(but "Capuccino",   	\m -> CoffeeMachine (Ask Capuccino,	m))
			,(but "Espresso", 		\m -> CoffeeMachine (Ask Espresso,	m))
			]
		]

	but s = LButton defpixel s

	initmachine = Init {money=0,beans=6,trash=0,out=Message "Welcome."} 

	displayoutput {out} = toString out

	displaycontents {money,beans,trash}
		= ("money ",money) <|> 
		  ("beans ",beans) <|> 
		  ("trash ",trash) 

	

// The defintion below is copied from the GEC coffeemachine, and slightly improved...

::	Client					// Client actions:
	=	InsertCoin			// insert a coin
	|	Ask Product			// ask for product
	|	AddBeans			// add beans in machine
	|	EmptyTrash			// empty bean trash of machine
	|	Idle				// does nothing
::	Machine					// CoffeeMachine:
	=	{ money	:: Int		// nr of coins (maxCoins)
		, beans	:: Int		// amount of beans (maxBeans)
		, trash	:: Int		// amount of bean-trash (maxTrash)
		, out	:: Output	// output of machine
		}
::	Product	=   Coffee | Capuccino | Espresso
::	Msg		:== String	// Errors or customer-friendly information
::	Output	=   Message Msg | Prod Product

//	CoffeeMachine is the self-correcting function on the model data of the Client-Coffee Machine:
CoffeeMachine :: (Client,Machine) -> Machine
CoffeeMachine (InsertCoin, m=:{money})
	| money >= maxCoins				= { m &                        out = Message "Coin not accepted." }
	| otherwise						= { m & money = money+1,       out = Message "Thank you." }
CoffeeMachine (EmptyTrash, m)		= { m & trash = 0,             out = Message "Trash emptied." }
CoffeeMachine (AddBeans, m=:{beans})                
	| beans > maxBeans-beanBag		= { m &                        out = Message "Too many beans." }
	| otherwise						= { m & beans = beans+beanBag, out = Message "Beans refilled." }
CoffeeMachine (Ask p,m=:{money,beans,trash})
	| beans < beancost p			= { m &                        out = Message "Not enough beans." }
	| money < cost p				= { m &                        out = Message "Not enough coins." }
	| trash + ptrash p > maxTrash	= { m &                        out = Message "Trash full." }
	| otherwise						= { m & out   = Prod p
									      , beans = beans - beancost p
									      , money = money - cost p
									      , trash = trash + ptrash p
									  }
CoffeeMachine (_,m)					= m

maxCoins	:== 10		// max. number of coins in machine
maxBeans	:== 20		// max. amount of coffeebeans in machine
maxTrash	:== 5		// max. amount of coffeetrash in machine
beanBag		:== 10		// unit of bean refill

// The number of coins that a product costs
cost :: Product -> Int 
cost Coffee     = 1
cost Capuccino  = 2
cost Espresso   = 3

// The number of beans that a product costs
beancost :: Product -> Int 
beancost Coffee    = 2
beancost Capuccino = 1
beancost Espresso  = 1

// Amount of trash generated by product
ptrash :: Product -> Int 
ptrash _ = 1                      


instance toString Output where
	toString (Message s)      = s
	toString (Prod Coffee)    = "Coffee"
	toString (Prod Capuccino) = "Capuccino"
	toString (Prod Espresso)  = "Espresso"

