implementation module StdAGEC

import genericgecs, guigecs, infragecs
import StdBool, StdFunc, StdList, StdMisc, StdOrdList, StdString, StdTuple
import StdObjectIOExt
import ColourTextControl
import objectloc, gec
import parseprint, GenPrint, StdArray
import StdGeneric, StdGECExt
import store
import StdFunc, StdInt, StdMisc, StdEnum
from   StdObjectIOExt import closeControl
import StdPSt
import testable, parseprint
from   infragecs import basicGEC, :: InfraGEC

// (,) is used to place editors next to each other

gGEC{|(,)|} gGECa gGECb gecArgs=:{gec_value=mtuple,update=tupdate} pSt
	= convert (pairGEC spairGECGUI gGECa gGECb {gecArgs & gec_value=mpair,update=pupdate} pSt)
where
	mpair = case mtuple of
				Just (a,b) = Just (PAIR a b)
				Nothing	   = Nothing

	pupdate reason (PAIR a b) pst = tupdate reason (a,b) pst

	convert (pairhandle,pst) = ({pairhandle & gecSetValue = tupleSetValue pairhandle.gecSetValue
	                                        , gecGetValue = tupleGetValue pairhandle.gecGetValue
	                            },pst)
	
	tupleSetValue pairSetValue upd (a,b)  = pairSetValue upd (PAIR a b)
	tupleGetValue pairGetValue pst
		# (PAIR a b,pst) = pairGetValue pst
		= ((a,b),pst)


	spairGECGUI :: GECGUIFun (PAIR a b) (PSt .ps)
	spairGECGUI = spairGECGUI`
	where
		spairGECGUI` outputOnly pSt
			# (id1,pSt)	= openId pSt
			# (id2,pSt)	= openId pSt
			= customGECGUIFun Nothing [(id1,Just (Left,zero),Just (Right,zero)),(id2,Nothing,Just (Right,zero))] undef NilLS (const id) outputOnly pSt

// some handy buttons

derive generate UpDown

instance parseprint UpDown where
	parseGEC "UpPressed" 	= Just UpPressed
	parseGEC "DownPressed" 	= Just DownPressed
	parseGEC "Neutral"   	= Just Neutral
	
	printGEC UpPressed 		= "UpPressed"
	printGEC DownPressed 	= "DownPressed"
	printGEC Neutral    	= "Neutral"

gGEC{|UpDown|} gecArgs=:{outputOnly} pSt
	= basicGEC typeName tGEC (updownGECGUI typeName (setGECvalue tGEC)) gecArgs pSt1
where
	(tGEC,pSt1)	= openGECId pSt
	typeName	= "UpDown"
	updownGECGUI typeName setValue outputOnly pSt
		# (sId,pSt) = openId  pSt
		# (rId,pSt)	= openRId pSt
		# updownGUI	= { newLS  = 0
					  , newDef =     SliderControl Vertical (PixelWidth 16) {sliderMin = -2^31,sliderMax = 2^31,sliderThumb=0} 
					                               (sliderFun sId)
                                                   [ ControlTip      (":: "+++typeName)
                                                   , ControlId       sId
                                                   ]
				                 :+: Receiver rId (setNewThumb sId) []
	                  }
	    = customGECGUIFun Nothing [] undef updownGUI (update rId) outputOnly pSt
	where
		sliderFun sId sliderMove (v,pSt)
			= case sliderMove of
				SliderDecSmall	= (v+1,setValue YesUpdate UpPressed (appPIO (setSliderThumb sId (v+1)) pSt))
				SliderIncSmall  = (v-1,setValue YesUpdate DownPressed (appPIO (setSliderThumb sId (v-1)) pSt))
		setNewThumb sId b (v,pSt)
			= (v+toInt b,appPIO (setSliderThumb sId (v+toInt b)) pSt)
		update rId b pSt
			= snd (syncSend rId b pSt)
		

instance toInt UpDown where
	toInt UpPressed = 1
	toInt DownPressed = -1
	toInt Neutral    = 0
	
instance toInt Button where
	toInt any = 0

derive generate Button
instance parseprint Button where
	parseGEC any 	= Just undef
	printGEC any		= "any"

gGEC{|Button|} gecArgs=:{gec_value=mv} pSt
	= basicGEC typeName tGEC (buttonGECGUI typeName (setGECvalue tGEC)) gecArgs pSt1
where
	(tGEC,pSt1)	= openGECId pSt
	typeName	= "Button"
	buttonname	= case mv of Just (Button name) = name
							 Nothing			= "??"
	
	buttonGECGUI typeName setValue outputOnly pSt
		# (sId,pSt) = openId  pSt
		# (rId,pSt)	= openRId pSt
		# buttonGUI	=     ButtonControl buttonname  [ ControlTip      (":: "+++typeName)
	                                                , ControlId       sId
	                                                , ControlFunction setButton
	                                                ]
					  :+: Receiver rId (setButton2 sId) []
	    = customGECGUIFun Nothing [] undef buttonGUI (update rId) outputOnly pSt
	where
		setButton (ls,pSt)
			= (ls,setValue YesUpdate Pressed pSt)
		setButton2 sId (Button name) (ls,pSt)
			= (ls,appPIO (setControlText sId name) pSt)
		setButton2 sId Pressed (ls,pSt)
			= (ls,pSt)
		update rId b pSt
			= snd (syncSend rId b pSt)

// an hidden editor will show noting but behavious like an editor

// Mode = Hide: hidden editor will show noting but behavious like an editor
// Mode = Display: Non editable
// Mode = Edit: identity

gGEC{|Mode|} gGECa args=:{gec_value = Just (Display a), update = modeupdate} pSt
= convert (gGECa {args & gec_value = Just a, update = aupdate,outputOnly = OutputOnly} pSt)
where
	convert (ahandle,pst) = ({ahandle & gecSetValue = modeSetValue ahandle
	                                  , gecGetValue = displayGetValue ahandle
	                          },pst)
	displayGetValue ahandle pst
	# (na,pst) = ahandle.gecGetValue pst
	= (Display na,pst)
	
	modeSetValue ahandle upd (Edit a) 		=  ahandle.gecSetValue upd a
	modeSetValue ahandle upd (Display a) 	=  ahandle.gecSetValue upd a
	modeSetValue ahandle upd (Hide a) 		=  ahandle.gecSetValue upd a
	modeSetValue ahandle upd (EmptyMode) 	=  \pst 	=  pst

	aupdate reason na pst = modeupdate reason (Display na) pst

gGEC{|Mode|} gGECa args=:{gec_value = Just (Edit a), update = modeupdate} pSt
= convert (gGECa {args & gec_value = Just a, update = aupdate} pSt)
where
	convert (ahandle,pst) = ({ahandle & gecSetValue = modeSetValue ahandle
	                                  , gecGetValue = modeGetValue ahandle
	                          },pst)
	modeGetValue ahandle pst
	# (na,pst) = ahandle.gecGetValue pst
	= (Edit na,pst)
	
	modeSetValue ahandle upd (Edit a) 		=  ahandle.gecSetValue upd a
	modeSetValue ahandle upd (Display a) 	=  ahandle.gecSetValue upd a
	modeSetValue ahandle upd (Hide a) 		=  ahandle.gecSetValue upd a
	modeSetValue ahandle upd (EmptyMode) 	=  \pst 	=  pst

	aupdate reason na pst = modeupdate reason (Edit na) pst

gGEC{|Mode|} gGECa args=:{gec_value = Just (Hide a), update = modeupdate} pSt
= createDummyGEC OutputOnly (Hide a) modeupdate pSt

gGEC{|Mode|} gGECa args=:{gec_value = Just (EmptyMode), update = modeupdate} pSt
= createDummyGEC OutputOnly (EmptyMode) modeupdate pSt

// Timer driven GEC

:: Timed = Timed (Int ->Int) Int						

//	{location,makeUpValue,outputOnly,gec_value,update}

gGEC{|Timed|} args=:{gec_value=Just (Timed updfun i),update=tiupdate} pSt
# (tid,pSt) 		= openId pSt
# (ahandle,pSt)		= gGEC {|*|} {args & gec_value=Just (Hide i),update=aupdate} pSt
# pSt				= snd (openTimer Void (timer tid ahandle) pSt)
= convert tid (ahandle,pSt)
where
	convert tid (ahandle,pSt) = ({ahandle & gecSetValue = set tid ahandle
	                               		  , gecGetValue = get tid ahandle
	                          	 },pSt)

	set tid ahandle upd (Timed updfun ni) pSt
	# pSt = appPIO (setTimerInterval tid ni) pSt
	= ahandle.gecSetValue upd (Hide ni) pSt

	get tid ahandle pSt
	# (Hide ni,pSt) = ahandle.gecGetValue pSt
	= (Timed updfun ni,pSt)

	aupdate reason (Hide ni) pSt = tiupdate reason (Timed updfun ni) pSt
		
	timer tid ahandle = (Timer i NilLS [TimerId tid, TimerFunction (timeout tid ahandle)])
	where
		timeout tid ahandle _ (lSt,pSt)
		# (Hide i,pSt) = ahandle.gecGetValue pSt
		# ni = updfun i
		# pSt = appPIO (setTimerInterval tid ni) pSt
		= (lSt,ahandle.gecSetValue YesUpdate (Hide ni) pSt)
			
// bimap GEC a b to use a b-editor for constructing an a-value

gGEC{|BimapGEC|} _ gGECb gecArgs pSt
	= bimapgec gGECb gecArgs pSt
bimapgec gGECb gecArgs=:{gec_value=mbimap,update=biupdate} pSt
	= convert (bhandle,pst1)
where
	(bhandle,pst1) = gGECb {gecArgs & gec_value=mb,update=bupdate bhandle} pSt

	convert (bhandle,pst) = ({bhandle & gecSetValue = bimapSetValue bhandle.gecGetValue bhandle.gecSetValue
	                                  , gecGetValue = bimapGetValue bhandle.gecGetValue
	                          },pst)

	mb = case mbimap of
				Just bimap  = Just (bimap.toGEC bimap.value Undefined)
				Nothing	    = Nothing

	bupdate bhandle reason b pst 
		# nb	= bimap`.updGEC  b
		# na	= bimap`.fromGEC nb
		# pst	= bhandle.gecSetValue NoUpdate nb pst
		= biupdate reason {bimap` & value = na} pst

	bimapSetValue bGetValue bSetValue upd bimap pst
		# (b,pst) = bGetValue pst
	 	= bSetValue upd (bimap.toGEC bimap.value (Defined b)) pst
	bimapGetValue bGetValue pst
		# (b,pst) = bGetValue pst
		= ({bimap` & value = bimap`.fromGEC b},pst)
		
	bimap` = case mbimap of 
				Just bimap = bimap
				Nothing		= abort "Cannot make up function value for bimapGEC"

// Abstract editors

:: AGEC a = E. .b :  Hidden (BimapGEC a b) (A. .ps: InfraGEC (BimapGEC a b) (PSt ps))

mkAGEC  :: (BimapGEC a b) -> AGEC a | gGEC{|*|} b
mkAGEC bimapGEC =  Hidden bimapGEC (gGEC{|*->*->*|} undef gGEC{|*|})

^^    :: (AGEC a) -> a
^^ (Hidden bimap ggec) = bimap.value

(^=) infixl  :: (AGEC a) a -> (AGEC a)
(^=) (Hidden bimap ggec) nvalue = (Hidden {bimap & value = nvalue} ggec)

gGEC{|AGEC|} _ gecArgs=:{gec_value=mbimap,update=biupdate} pSt
	= case mbimap of 
		Just abstractGEC=:(Hidden bimapGEC gGECbimapGEC) 
					= convert abstractGEC (gGECbimapGEC {gecArgs & gec_value=Just bimapGEC,update=bupdate abstractGEC} pSt)
		Nothing		= abort "Cannot make up function value for AGEC"
where
	convert abstractGEC (ahandle,pst) 
					= ({ahandle & gecSetValue = AGECSetValue ahandle.gecSetValue ahandle.gecGetValue
	                            , gecGetValue = AGECGetValue abstractGEC ahandle.gecGetValue
	                   },pst)

	AGECSetValue aSetValue aGetValue upd (Hidden nval _) pst  
					= case aGetValue pst of
							(bimap,pst) -> aSetValue upd {bimap & value = nval.value} pst
							(else,pst) -> abort "cannot be"
	AGECGetValue (Hidden bimap gGECb) aGetValue pst
		# (nval,pst) = aGetValue pst
		= (Hidden {bimap & value = nval.value} gGECb,pst)
	
	bupdate (Hidden bimap gGECb) reason nbimap pst 
	= biupdate reason (Hidden {bimap & value = nbimap.value} gGECb) pst

// Identity 

idGEC :: a -> AGEC a | gGEC {|*|} a 
idGEC j 	= mkAGEC 	{	toGEC	= \i _ ->i
						,	fromGEC = id
						,	value	= j
						,	updGEC	= id
						}
// Hidden Identity

hidGEC :: a -> AGEC a | gGEC {|*|} a 
hidGEC j 	= mkAGEC 	{	toGEC	= \i _ -> Hide i
						,	fromGEC = \(Hide i) -> i
						,	value	= j
						,	updGEC	= id
						}

// apply GEC

applyAGEC :: (b -> a) (AGEC b) -> AGEC a | gGEC {|*|} a & gGEC {|*|} b
applyAGEC fba gecb	= mkAGEC 	{	toGEC	= initgec
								,	fromGEC = \(gecb <|> Display olda) -> fba (^^ gecb)
								,	value	= inita
								,	updGEC	= \(gecb <|> Display olda) -> (gecb <|> Display (fba (^^ gecb)))
								}
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
						}
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
							}
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
						}
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
							}
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
						, value 	= demode mode}
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
								}
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
								}
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
								}
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
				}
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

