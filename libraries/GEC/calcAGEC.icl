implementation module calcAGEC

import StdAGEC, modeAGEC, buttonAGEC, tupleAGEC, updownAGEC, basicAGEC

// buttons with functions attached

calcAGEC :: a [[(Button,a->a)]] -> AGEC a | gGEC {|*|} a 
calcAGEC a butfun = 	mkAGEC 	{	toGEC	= \a _ -> a <|> table_hv_AGEC buts
							,	fromGEC = \(na <|> buts) -> na
							,	value 	= a
							,	updGEC	= calcnewa
							} "calcGEC"
where
	(buts,funs) = ([map fst list \\ list <- butfun],[map snd list \\ list <- butfun])

	calcnewa (na <|> nbuts) =  hd [f na \\ (f,Pressed) <- zip2 (flatten funs) (flatten (^^ nbuts))]
							<|> table_hv_AGEC buts

// Integer with calculator buttons

intcalcAGEC :: Int -> AGEC Int
intcalcAGEC i = 	mkAGEC	{	toGEC	= \ni _ -> calcAGEC ni buttons
						,	fromGEC = \b -> ^^ b
						,	value 	= i
						,	updGEC	= id
						} "intcalcGEC"
where
	buttons	  =  [ map mkBut [7..9]
				 , map mkBut [4..6]
				 , map mkBut [1..3]
				 , [mkBut 0, (Button (defCellWidth/3) "C",\_->0), (Button (defCellWidth/3) "N", \v -> 0 - v)]
				 ]

	mkBut i = (Button (defCellWidth/3) (toString i),\v -> v*10 + i)

realcalcAGEC :: Real -> AGEC Real
realcalcAGEC i = 	mkAGEC	{	toGEC	= newGEC
							,	fromGEC = \b -> fst (^^ b)
							,	value 	= i
							,	updGEC	= id
							} "realcalcGEC"
where
	newGEC ni Undefined 	 = calcAGEC (ni ,Hide (True,1.0)) buttons
	newGEC 0.0 (Defined oval)= calcAGEC (0.0,Hide (True,1.0)) buttons
	newGEC ni  (Defined oval)= calcAGEC (ni,snd (^^ oval)) buttons 

	buttons	  =  [ map mkBut [7..9]
				 , map mkBut [4..6]
				 , map mkBut [1..3]
				 , [mkBut 0]
				 , [ (Button (defCellWidth/3) ".", \(v,Hide (_,_))	-> (v,  Hide (False,1.0)))
				   , (Button (defCellWidth/3) "C", \(_,hide) 		-> (0.0,Hide (True,1.0)))
				   , (Button (defCellWidth/3) "N", \(v,hide) 		-> (0.0 - v,hide))
				   ]
				 ]

	mkBut i =  (  Button (defCellWidth/3) (toString i)
				, \(v,Hide (cond,base)) -> if cond (v*10.0 + toReal i,Hide (cond,base))
											     (v+(toReal i/(base*10.0)),Hide(cond,(base*10.0)))
				)

