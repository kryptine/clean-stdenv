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
