module coffeemachine

import StdEnv
import StdIO
import genericgecs
import StdGEC, StdGECExt, StdAGEC
import basicAGEC, calcAGEC

/****************************************************************************************
*
*	An example function needs to be of type (PSt Void) -> (PSt Void).
*
*****************************************************************************************/

Start :: *World -> *World
Start world 
	= startIO MDI Void 
	  coffeemachine
	  [ProcessClose closeProcess] world

/*****************************************************************************************
*
*	The Client-Coffee Machine model:
*
******************************************************************************************/

::	Client					// Client actions:
	=	InsertCoin			// insert a coin
	|	Get Product			// ask for product
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

//	dataModelFun is the self-correcting function on the model data of the Client-Coffee Machine:
dataModelFun :: (Client,Machine) -> (Client,Machine)
dataModelFun (InsertCoin, m=:{money})
	| money >= maxCoins				= (Idle, { m &                        out = Message "Coin not accepted." } )
	| otherwise						= (Idle, { m & money = money+1,       out = Message "Thank you." } )
dataModelFun (EmptyTrash, m)		= (Idle, { m & trash = 0,             out = Message "Trash emptied." } )
dataModelFun (AddBeans, m=:{beans})                
	| beans > maxBeans-beanBag		= (Idle, { m &                        out = Message "Too many beans." } )
	| otherwise						= (Idle, { m & beans = beans+beanBag, out = Message "Beans refilled." } )
dataModelFun (Get p,m=:{money,beans,trash})
	| beans < beancost p			= (Idle, { m &                        out = Message "Not enough beans." } )
	| money < cost p				= (Idle, { m &                        out = Message "Not enough coins." } )
	| trash + ptrash p > maxTrash	= (Idle, { m &                        out = Message "Trash full." } )
	| otherwise						= (Idle, { m & out   = Prod p
									             , beans = beans - beancost p
									             , money = money - cost p
									             , trash = trash + ptrash p
									             }
									  )
dataModelFun (_,m)					= (Idle,m)

derive gGEC Client, Machine, Product, Output


instance toString Output where
	toString (Message s)      = s
	toString (Prod Coffee)    = "Coffee"
	toString (Prod Capuccino) = "Capuccino"
	toString (Prod Espresso)  = "Espresso"

isPressed :: Button -> Bool
isPressed (Button _ _) = False
isPressed Pressed      = True

derive generate Mode, Button, Machine, Output, Product

coffeemachine
	= startCircuit (feedback (edit "Coffee" >>> arr updCoffee)) (toViewModel (Idle,initCoffee))
where
	updCoffee = toViewModel o dataModelFun o fromViewModel
	
//	toViewModel transform the data model to the view model of the Client-Coffee Machine:
	toViewModel (_, m=:{money,beans,trash,out})
		= hidAGEC m <|>
		  Display "money " <-> Display (toString money +++ " ") <-> Button 80 "Insert coin" <|>
		  Display "beans " <-> Display (toString beans +++ " ") <-> Button 80 "Add beans"   <|>
		  Display "trash " <-> Display (toString trash +++ " ") <-> Button 80 "Empty trash" <|>
		  Button (2*defCellWidth/3) "Coffee"  <-> Button (2*defCellWidth/3) "Capuccino" <-> Button (2*defCellWidth/3) "Espresso" <|>
		  Display (toString out +++ ". ")
	
//	fromViewModel transform the view model to the data model of the Client-Coffee Machine:
	fromViewModel (m <|>
                   _ <-> _ <-> btnInsertCoin <|>
                   _ <-> _ <-> btnAddBeans   <|>
                   _ <-> _ <-> btnEmptyTrash <|>
                   btnCoffee <-> btnCapuccino <-> btnEspresso <|> 
                   _)
        = (client,^^m)
	where
		client	= if (isPressed btnInsertCoin) InsertCoin
		         (if (isPressed btnAddBeans)   AddBeans
		         (if (isPressed btnEmptyTrash) EmptyTrash
		         (if (isPressed btnCoffee)     (Get Coffee)
		         (if (isPressed btnCapuccino)  (Get Capuccino)
		         (if (isPressed btnEspresso)   (Get Espresso)
		                                       Idle)))))

	initCoffee = {money=0,beans=6,trash=0,out=Message "Welcome."} 
