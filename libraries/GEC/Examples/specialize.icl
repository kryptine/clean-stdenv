module specialize

import StdEnv, StdIO
import GecArrow, basicArrowEditors, StdAGEC, basicAGEC, StdArrow, noObjectAGEC

derive defval Family, BurgelijkeStaat, Maybe, (,), [], NoObject, YesObject, Persoon, Partner, Kids
derive gGEC BurgelijkeStaat, Bimap

// This Example shows how one can make specialize editors for user defined types
// The default view of a type can be overwritten with a user defined view
// one simply defines a bimap from the original type to an AGEC of that type

Start:: *World -> *World
Start world = startGEC myeditor2 world

import tree
derive gGEC Tree
derive defval Tree

myeditor2 = startCircuit mycircuit defaultv
where
	mycircuit =  edit "Family Tree Editor"

	defaultv :: Tree Family
	defaultv =  defval`

// data domain
:: Family	 	 	=	Family    		Persoon BurgelijkeStaat (Viewbe Partner)
:: BurgelijkeStaat	=	Married			
					|	Divorced		
					|	Single 
:: Partner			=	Partner Persoon Kids 
:: Kids				=	Kids [Family] | NoKids
:: Persoon 			= 	Man 			String 						
					| 	Woman 			String

derive gGEC Persoon
derive gGEC Maybe

//derive gGEC Family
//derive gGEC Kids
//derive gGEC Partner


gGEC{|Family|} gecArgs pSt 
= Specialize defval` familieAGEC gecArgs pSt

familieAGEC :: Family -> AGEC Family
familieAGEC f = mkAGEC (to_BimapGEC bimapFamily f) "Family"
where
	bimapFamily = {map_to = map_to, map_from = map_from}

	map_to (Family p1 Single _) 					      =  (p1 <|> Single       <|> toViewbe Nothing)
	map_to (Family p1=:(Woman v) any  (NoObject Nothing)) =  (p1 <|> any          <|> toViewbe mandef ) 
	map_to (Family p1    	     any  (NoObject Nothing)) =  (p1 <|> any          <|> toViewbe vrouwdef) 
	map_to (Family p1            any   partners)		  =  (p1 <|> any          <|> partners)
	map_from (p1 <|> bs <|> partners) 					  =   Family p1 bs partners

	mandef 		= Just (Partner (Man "")   NoKids) 
	vrouwdef 	= Just (Partner (Woman "") NoKids) 

gGEC{|Kids|} gecArgs pSt 
= Specialize NoKids KidsAGEC gecArgs pSt

KidsAGEC :: Kids -> AGEC(Kids)
KidsAGEC p = mkAGEC (to_BimapGEC bimapKids p) "Kids" 
where
	bimapKids = {map_to = map_to, map_from = map_from}
	where
		map_to (NoKids) 	  = vert2listAGEC defaultfam []
		map_to (Kids familie) = vert2listAGEC defaultfam familie
		map_from alist  = case (^^ alist) of
							[] -> NoKids
							list -> Kids list

		defaultfam = (Family (Man "") Single (NoObject Nothing))

gGEC{|Partner|} gecArgs pSt 
= Specialize defval PartnerAGEC gecArgs pSt
where
	defval = Partner (Man "") NoKids

	PartnerAGEC :: Partner -> AGEC(Partner)
	PartnerAGEC p = mkAGEC (to_BimapGEC bimapPartner p) "Partner" 
	where
		bimapPartner = {map_to = map_to, map_from = map_from}
		where
			map_to (Partner p kids) = p <-> kids
			map_from (p  <-> kids)   = (Partner p kids)

// additional hacking conversions funcions just to hide constructors on one level

:: Viewbe a			:== NoObject (Maybe (YesObject a))

toViewbe :: (Maybe a) -> (Viewbe a)
toViewbe (Just v) =  NoObject (Just (YesObject v)) 
toViewbe Nothing  =  NoObject Nothing 

fromViewbe :: (Viewbe a) -> (Maybe a)
fromViewbe (NoObject (Just (YesObject v))) = Just v 
fromViewbe (NoObject Nothing) = Nothing

//

gGEC{|(->)|} gGECa gGECb args=:{gec_value = Just (id), update = modeupdate} pSt
= createDummyGEC OutputOnly (id) modeupdate pSt
gGEC{|(->)|} gGECa gGECb args=:{gec_value = Nothing, update = modeupdate} pSt
= createDummyGEC OutputOnly (undef) modeupdate pSt


// default values generator

generic defval a ::  a 
defval{|Int|}  				= 0
defval{|Real|}  			= 0.0
defval{|String|}  			= ""
defval{|UNIT|} 			 	= UNIT
defval{|EITHER|} dl dr  	= RIGHT  dr
//defval{|EITHER|} dl dr   	= LEFT   dl
defval{|PAIR|}   dl dr  	= PAIR   dl dr
defval{|CONS|}   dc     	= CONS   dc
defval{|FIELD|}  df     	= FIELD  df
defval{|OBJECT|} do     	= OBJECT do
defval{|AGEC|}   da 	    = undef

defval` = defval {|*|}

// try out

getdict :: (t a) -> ((t a),(InfraGEC a (PSt .ps))) | gGEC{|*|} a & bimap{|*|} ps
getdict ar = (ar,dict)
where
	dict = gGEC {|*|}





/*

// view domain
derive defval Family`


:: Family`	 	 	= F	(<|> 				Persoon 
						 (<|>				BurgelijkeStaat 
						 	(Viewbe (<|>	Partner 
						 			    	(AGEC [Family`])
						 			 )
						 	)
						 ))

myeditor = startCircuit mycircuit defaultv
where
	mycircuit = feedback (arr toView >>>  edit "stamboom" >>> arr fromView)

	defaultv :: Family
	defaultv =  defval`


//bimap{|Family|} = {map_to = toView, map_from = fromView}

toView :: Family -> Family`
toView (Family p1 Single _) 			=  F (p1 <|> Single <|> defval`)
toView (Family p1=:(Woman v) any  Nothing) =  F (p1 <|> any          <|> defaultf defaultm defval`) 
toView (Family p1    	      any  Nothing) =  F (p1 <|> any          <|> defaultf defval`  defval`) 
toView (Family p1 any (Just (p2,kids)))	=  F (p1 <|> any          <|> defaultf p2 (map toView kids))

defaultm = Man "" 
defaultf p k = toViewbe (Just (p <|> vert2listAGEC defval` k))

fromView :: Family` -> Family
fromView (F (p1 <|> bs <|> fam)) = Family p1 bs (convert (fromViewbe fam))
where
	convert Nothing = Nothing
	convert (Just (partner <|> afamily)) = Just (partner,map fromView (^^ afamily))


*/




