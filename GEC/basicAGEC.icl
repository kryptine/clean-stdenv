implementation module basicAGEC

import StdAGEC, modeAGEC, buttonAGEC, tupleAGEC, updownAGEC

// Identity 

idAGEC :: a -> AGEC a | gGEC {|*|} a 
idAGEC j 	= mkAGEC 	{	toGEC	= \i _ ->i
						,	fromGEC = id
						,	value	= j
						,	updGEC	= id
						} "idGEC"

hidAGEC :: a -> AGEC a  // Just a store, does not require any GEC !
hidAGEC j 	= mkAGEC` 	{	toGEC	= \i _ -> Hide i
						,	fromGEC = \(Hide i) -> i
						,	value	= j
						,	updGEC	= id
						} "hidGEC"

predAGEC :: (a -> Bool) a -> AGEC a | gGEC {|*|} a 
predAGEC pred j = mkAGEC 	{	toGEC	= toPred
							,	fromGEC = \(ni,oi) = ni
							,	value	= j
							,	updGEC	= \(ni,oi) -> test ni (^^ oi)
							}  "predGEC"
where
	toPred ni Undefined          = (ni, hidAGEC ni)
	toPred ni (Defined (oi,hoi)) = test ni oi

	test ni oi = if (pred ni) (ni, hidAGEC ni) (oi, hidAGEC oi)

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


// convert mode to agec

modeAGEC :: (Mode a) -> AGEC a | gGEC {|*|} a
modeAGEC mode =  mkAGEC 	{ toGEC 	= mkmode mode
						, fromGEC 	= demode
						, updGEC 	= id
						, value 	= demode mode} "modeGEC"
where
	demode (Display a) = a
	demode (Edit a) =  a
	demode (Hide a) =  a
	demode EmptyMode =  abort "EmptyMode inspected"

	mkmode (Display om) nm Undefined = Display nm
	mkmode (Edit om)    nm Undefined = Edit nm
	mkmode (Hide om)    nm Undefined = Hide nm
	mkmode mode nm         (Defined om) = mkmode mode nm Undefined

// Integer with up down counter

counterAGEC :: a -> AGEC a | gGEC {|*|} a & IncDec a
counterAGEC j = mkAGEC 	{	toGEC	= \i _ ->(i,Neutral)
						,	fromGEC = fst
						,	value	= j
						,	updGEC	= updateCounter
						} "counterGEC"
where
	updateCounter (n,UpPressed) 	= (n+one,Neutral)
	updateCounter (n,DownPressed) 	= (n-one,Neutral)
	updateCounter any 		 	 	= any


// All elements of a list shown in a row

horlistAGEC :: [a] -> AGEC [a] | gGEC {|*|} a  
horlistAGEC  list	= mkAGEC	{	toGEC	= tohorlist
								,	fromGEC = fromhorlist
								,	value 	= list
								,	updGEC	= id
								}  ("horlistGEC" +++ len)
where
	tohorlist []	 _ = EmptyMode <-> hidAGEC []
	tohorlist [x:xs] _ = Edit x    <-> horlistAGEC xs

	fromhorlist (EmptyMode <-> xs) = []  
	fromhorlist (Edit x <-> xs)    = [x: ^^ xs]  

	len = (toString (length list))


// All elements of a list shown in a column

vertlistAGEC :: [a] -> AGEC [a] | gGEC {|*|} a  
vertlistAGEC  list = vertlistGEC` list
where
	vertlistGEC` list = mkAGEC	{	toGEC	= tovertlist
									,	fromGEC = fromvertlist
									,	value 	= list
									,	updGEC	= id
									} ("vertlistGEC" +++ len)

	tovertlist []	 _ 	= EmptyMode <|> hidAGEC []
	tovertlist [x:xs] _ = Edit x    <|> vertlistGEC` xs

	fromvertlist (EmptyMode <|> xs)	= []  
	fromvertlist (Edit x <|> xs)	= [x: ^^ xs]  

	len = (toString (length list))

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


listAGEC :: Bool [a] -> AGEC [a] | gGEC {|*|} a  
listAGEC finite list  
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
		mkaction nr			= {	element_nr 	= counterAGEC nr
							  , goto		= (Button (defCellWidth/2) "0",Button (defCellWidth/2) (toString next))
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


// All elements of a list shown in a column

table_hv_AGEC :: [[a]] -> AGEC [[a]] | gGEC {|*|} a  
table_hv_AGEC  list		= mkAGEC	{	toGEC	= \newlist -> mktable newlist
								,	fromGEC = \table   -> mklist (^^ table)
								,	value 	= list
								,	updGEC	= id
								} "tableGEC"
where
	mktable list	  _ = vertlistAGEC [(horlistAGEC xs) \\ xs <- list]
	mklist  []			= []	
	mklist  [hor:hors]	= [^^ hor: mklist hors]	

table_vh_AGEC :: [[a]] -> AGEC [[a]] | gGEC {|*|} a  
table_vh_AGEC  list		= mkAGEC	{	toGEC	= \newlist -> mktable newlist
								,	fromGEC = \table   -> mklist (^^ table)
								,	value 	= list
								,	updGEC	= id
								} "tableGEC"
where
	mktable list	  _ = horlistAGEC [(vertlistAGEC xs) \\ xs <- list]
	mklist  []			= []	
	mklist  [hor:hors]	= [^^ hor: mklist hors]	
