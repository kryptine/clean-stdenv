implementation module examplesAGEC

import StdAGEC, modeGEC, buttonGEC, tupleGEC, updownGEC


// Identity 

idGEC :: a -> AGEC a | gGEC {|*|} a 
idGEC j 	= mkAGEC 	{	toGEC	= \i _ ->i
						,	fromGEC = id
						,	value	= j
						,	updGEC	= id
						} "idGEC"


// Hidden Identity

hidGEC :: a -> AGEC a | gGEC {|*|} a 
hidGEC j 	= mkAGEC 	{	toGEC	= \i _ -> Hide i
						,	fromGEC = \(Hide i) -> i
						,	value	= j
						,	updGEC	= id
						} "hidGEC"


// Hidden and constant, cannot be changed

constGEC :: a -> AGEC a
constGEC j 	= mkAGEC 	{	toGEC	= \i _ -> Hide 0
						,	fromGEC = \(Hide 0) -> j
						,	value	= j
						,	updGEC	= id
						} "constGEC"


// apply GEC

applyAGEC :: (b -> a) (AGEC b) -> AGEC a | gGEC {|*|} a & gGEC {|*|} b
applyAGEC fba gecb	= mkAGEC 	{	toGEC	= initgec
								,	fromGEC = \(gecb <|> Display olda) -> fba (^^ gecb)
								,	value	= inita
								,	updGEC	= \(gecb <|> Display olda) -> (gecb <|> Display (fba (^^ gecb)))
								} "applyAGEC"
where
	inita = fba (^^ gecb)									

	initgec _ Undefined = gecb <|> Display inita
	initgec _ (Defined b) = b


// Integer with up down counter

counterGEC :: a -> AGEC a | IncDec a & gGEC {|*|} a 
counterGEC j = mkAGEC 	{	toGEC	= \i _ ->(i,Neutral)
						,	fromGEC = fst
						,	value	= j
						,	updGEC	= updateCounter
						} "counterGEC"
where
	updateCounter (n,UpPressed) 	= (n+one,Neutral)
	updateCounter (n,DownPressed) 	= (n-one,Neutral)
	updateCounter any 		 	 	= any


// buttons with functions attached

calcGEC :: a [[(Button,a->a)]] -> AGEC a | gGEC {|*|} a 
calcGEC a butfun = 	mkAGEC 	{	toGEC	= \a _ -> a <|> tableGEC buts
							,	fromGEC = \(na <|> buts) -> na
							,	value 	= a
							,	updGEC	= calcnewa
							} "calcGEC"
where
	(buts,funs) = ([map fst list \\ list <- butfun],[map snd list \\ list <- butfun])

	calcnewa (na <|> nbuts) =  hd [f na \\ (f,Pressed) <- zip2 (flatten funs) (flatten (^^ nbuts))]
							<|> tableGEC buts


// Integer with calculator buttons

intcalcGEC :: Int -> AGEC Int
intcalcGEC i = 	mkAGEC	{	toGEC	= \ni _ -> calcGEC ni buttons
						,	fromGEC = \b -> ^^ b
						,	value 	= i
						,	updGEC	= id
						} "intcalcGEC"
where
	buttons	  =  [ map mkBut [7..9]
				 , map mkBut [4..6]
				 , map mkBut [1..3]
				 , [mkBut 0, (Button "C",\_->0), (Button "N", \v -> 0 - v)]
				 ]

	mkBut i = (Button (toString i),\v -> v*10 + i)

realcalcGEC :: Real -> AGEC Real
realcalcGEC i = 	mkAGEC	{	toGEC	= newGEC
							,	fromGEC = \b -> fst (^^ b)
							,	value 	= i
							,	updGEC	= id
							} "realcalcGEC"
where
	newGEC ni Undefined 	 = calcGEC (ni ,Hide (True,1.0)) buttons
	newGEC 0.0 (Defined oval)= calcGEC (0.0,Hide (True,1.0)) buttons
	newGEC ni  (Defined oval)= calcGEC (ni,snd (^^ oval)) buttons 

	buttons	  =  [ map mkBut [7..9]
				 , map mkBut [4..6]
				 , map mkBut [1..3]
				 , [mkBut 0]
				 , [ (Button ".", \(v,Hide (_,_))	-> (v,  Hide (False,1.0)))
				   , (Button "C", \(_,hide) 		-> (0.0,Hide (True,1.0)))
				   , (Button "N", \(v,hide) 		-> (0.0 - v,hide))
				   ]
				 ]

	mkBut i =  (  Button (toString i)
				, \(v,Hide (cond,base)) -> if cond (v*10.0 + toReal i,Hide (cond,base))
											     (v+(toReal i/(base*10.0)),Hide(cond,(base*10.0)))
				)


// convert mode to agec

modeGEC :: (Mode a) -> AGEC a | gGEC {|*|} a
modeGEC mode =  mkAGEC 	{ toGEC 	= mkmode mode
						, fromGEC 	= demode
						, updGEC 	= id
						, value 	= demode mode} "modeGEC"
where
	demode (Display a) = a
	demode (Edit a) =  a
	demode (Hide a) =  a

	mkmode (Display om) nm Undefined = Display nm
	mkmode (Edit om)    nm Undefined = Edit nm
	mkmode (Hide om)    nm Undefined = Hide nm
	mkmode mode nm         (Defined om) = mkmode mode nm Undefined


// All elements of a list shown in a row

horlistGEC :: [a] -> AGEC [a] | gGEC {|*|} a  
horlistGEC  list	= mkAGEC	{	toGEC	= tohorlist
								,	fromGEC = fromhorlist
								,	value 	= list
								,	updGEC	= id
								} "horlistGEC"
where
	tohorlist []	 _ = EmptyMode <-> hidGEC []
	tohorlist [x:xs] _ = Edit x    <-> horlistGEC xs

	fromhorlist (EmptyMode <-> xs) = []  
	fromhorlist (Edit x <-> xs)    = [x: ^^ xs]  


// All elements of a list shown in a column

vertlistGEC :: [a] -> AGEC [a] | gGEC {|*|} a  
vertlistGEC  list	= mkAGEC	{	toGEC	= tovertlist
								,	fromGEC = fromvertlist
								,	value 	= list
								,	updGEC	= id
								} "vertlistGEC"
where
	tovertlist []	 _ 	= EmptyMode <|> hidGEC []
	tovertlist [x:xs] _ = Edit x    <|> vertlistGEC xs

	fromvertlist (EmptyMode <|> xs)	= []  
	fromvertlist (Edit x <|> xs)	= [x: ^^ xs]  


// All elements of a list shown in a column

tableGEC :: [[a]] -> AGEC [[a]] | gGEC {|*|} a  
tableGEC  list		= mkAGEC	{	toGEC	= \newlist -> mktable newlist
								,	fromGEC = \table   -> mklist (^^ table)
								,	value 	= list
								,	updGEC	= id
								} "tableGEC"
where
	mktable list	  _ = vertlistGEC [(horlistGEC xs) \\ xs <- list]
	mklist  []			= []	
	mklist  [hor:hors]	= [^^ hor: mklist hors]	


// list components

derive gGEC Actions,Action

:: Action 	= 	{ element_nr :: AGEC Int
				, goto		 :: (Button,Button)
				, actions	 :: Actions
				}
:: Actions  =	Append 
			|	Insert 
			|	Delete 
			|	Copy 
			|	Paste
			|	Choose	


listGEC :: Bool [a] -> AGEC [a] | gGEC {|*|} a  
listGEC finite list  
	= 	mkAGEC	{	toGEC	= mkdisplay
				,	fromGEC = \(_,Hide (_,(list,_))) -> list
				,	value 	= list
				,	updGEC	= \b -> edit (parseListEditor b)
				} "listGEC"
where
	mkdisplay list Undefined 							= display 0 list (list!!0)
	mkdisplay list (Defined(_,Hide (clipboard,(_,i)))) 	= display i list clipboard

	display i list clipboard = (mklistEditor list i,Hide (clipboard,(list,i)))

	mklistEditor list i = 	 list!!ni <|> 
							 mkaction ni  
	where
		mkaction nr			= {	element_nr 	= counterGEC nr
							  , goto		= (Button "0",Button (toString next))
							  , actions 	= Choose 
							  }
		ni					
		| finite			= if (i >= 0 && i <= (length list - 1)) i (if (i<0) (length list - 1) 0)
		| otherwise			= if (i >= 0) i 0

		next
		| finite			= length list - 1
		| otherwise			= i + 100

	parseListEditor  ( listelement <|>
				       {element_nr,goto,actions}, Hide (clipboard,(list,j))) 
		=	((fst goto,snd goto),actions,^^ element_nr,listelement,clipboard,list,j)

	edit ((first,last),_,i,_,clipboard,list,j)	
		| isPressed first	= display 0 list clipboard
		| isPressed last	= display (if finite (length list - 1) (i + 100)) list clipboard
		| i <> j			= display i list clipboard
	edit (_,Insert,i,_,clipboard,list,_)	
							= display i (insertAt i clipboard list) clipboard
	edit (_,Append,i,_,clipboard,list,_)	
							= display (i+1) (insertAt (i+1) clipboard list) clipboard
	edit (_,Delete,i,_,clipboard,list,_)	
							= display i (removeAt i list) clipboard
	edit (_,Copy,i,listelement,_,list,_)	
							= display i list listelement
	edit (_,Paste,i,_,clipboard,list,_)	
							= display i (updateAt i clipboard list) clipboard
	edit (_,_,i,listelement,clipboard,list,_)	
							= display i (updateAt i listelement list) clipboard
	
	isPressed Pressed = True
	isPressed _ = False
