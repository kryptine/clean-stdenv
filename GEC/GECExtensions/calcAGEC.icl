implementation module calcAGEC

import StdAGEC, modeGEC, buttonGEC, tupleGEC, updownGEC, basicAGEC

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

