module calculator

// simple spreadsheet example
// (c) MJP 2005

import StdEnv
import StdHtml

Start world  = doHtml calculator world

calculator hst
# (val,(cntb,hst)) 	= counterHGEC "cnt" 0 hst
# (cbf,(butb,hst)) 	= oneBut "+2" ((+) 2) hst
# (_,(res,hst)) 	= mkEditHGEC  "res" HDisplay (cbf val) hst
= (Head [ Hd_Title "Calculator"
		] 
		[ H1 "Calculator Example: "
		, cntb, Br, butb, Br
		, T "value returned : "
		, res
		, traceHtmlInput
		] ,hst)

oneBut :: String (a -> a) HSt -> (a -> a,(Body,HSt)) //| gHGEC{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} a
oneBut name cbf hst = mkViewHGEC name bimap id hst
where
	bimap =	{ toHGEC 	= \_ 	-> button
			, updHGEC	= id
			, fromHGEC	= \but -> case but of 
									CHPressed -> cbf
									_		  -> id
			, resetHGEC	= \_ 	-> button
			}
	button = CHButton 10 name	




/*
CHButton defsize "+"
calcAGEC ::  String (HMode a) [[(Button,a->a)]] a  -> ,(Body,HSt)) 	| gHGEC{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|} a
calcAGEC a mode buttable val
# (table, (tableB,hst)) = table_hv_HGEC "table" inittable    	hst

	= mkAGEC { toGEC   = \a _ -> a <|> table_hv_AGEC buts
	         , fromGEC = \(na <|> _) -> na
	         , value   = a
	         , updGEC  = \c ps -> (True,calcnewa c,ps)
			 , pred	   = nopred
	         } "calcGEC"
where
	(buts,funs) = ([map fst list \\ list <- butfun],[map snd list \\ list <- butfun])

	calcnewa (na <|> nbuts) = case [f \\ (f,Pressed) <- zip2 (flatten funs) (flatten (^^nbuts))] of
								[]    ->   na <|> nbuts
								[f:_] -> f na <|> table_hv_AGEC buts
								
*/