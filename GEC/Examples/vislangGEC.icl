module vislangGEC

// editor that can be used to design and test another editor --MJP

import tupleAGEC
import StdEnv
import StdIO
import genericgecs
import StdGEC, StdGECExt, GecArrow, StdDynamic 
import basicAGEC, StdAGEC, calcAGEC, dynamicAGEC, noObjectAGEC


:: FL 				:==	Fundefs
:: Fundefs		 	:==	[Fundef]
:: Fundef	 		=	Fun Identifier [Arg] Expressions
:: Identifier		:==	String
:: Arg				:== String
:: Expressions 		:==	[Expression]
:: Expression 		=	A Expressions
					|	D Identifier
					|	R Real
					|	B Bool
					|	I Int


:: MoreArgs			=	OneLess | OneMore | Is
:: LastDef			=	EndOfDefinition 
					|	NewDefinition


FLtoGEC  fundefs 
	= horlistAGEC (map FundeftoGEC fundefs) <|> EndOfDefinition
FundeftoGEC (Fun identifier args expression) 
	= identifier <-> (vertlistAGEC args) <|> Is <-> horlistAGEC (ExpressiontoGEC expression)
ExpressiontoGEC e = e

derive gGEC Fundef, Expression, LastDef, MoreArgs

testje = startCircuit (edit "test") (FLtoGEC [Fun "Factorial" ["n"] [D "Factorial",I 3]])
	

goGui :: (*(PSt u:Void) -> *(PSt u:Void)) *World -> .World
goGui gui world = startIO MDI Void gui [ProcessClose closeProcess] world

Start :: *World -> *World
Start world = goGui testje world  



// small auxilery functions

showAGEC i = (modeAGEC (Display ( i)))

ToString v = toString v +++ " "
