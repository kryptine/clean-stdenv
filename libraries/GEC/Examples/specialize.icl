module specialize

import StdEnv, StdIO
import StdGEC

derive defval Family, Status, Maybe, (,), [], NoObject, YesObject, Person, Partner, Kids
derive gGEC Status

// This Example shows how one can make specialize editors for user defined types
// The default view of a type can be overwritten with a user defined view
// one simply defines a bimap from the original type to an AGEC of that type

Start:: *World -> *World
Start world = startGEC myeditor world

myeditor = startCircuit (feedback mycircuit) defaultv
where
	mycircuit =  edit "Family Tree Editor" 

//	defaultv = (1 <-> 2 ) <^|^> 3

	defaultv :: Family
	defaultv =  defval`

:: Family	=	Family  Person Status (Maybe (Person,Kids))
:: Status	=	Married			
			|	Divorced		
			|	Single 
:: Partner	=	Partner Person Kids 
:: Kids		=	Kids [Family] | NoKids
:: Person 	= 	Man		String 						
			| 	Woman 	String

//derive gGEC Family, Kids, Partner, Person, Maybe


gGEC{|Family|} gecArgs pSt 
= Specialize defval` familieAGEC gecArgs pSt

familieAGEC :: Family -> AGEC Family
familieAGEC f = mkAGEC (to_BimapGEC bimapFamily f) "Family"
where
	bimapFamily = {map_to = map_to, map_from = map_from}

	map_to (Family p1 Single _) 						=  p1 <-> notmaried   <|> Single  <|*|> Nothing
	map_to (Family p1=:(Woman v) any  Nothing) 			=  p1 <-> mary man    <|> any     <|*|> Just NoKids 
	map_to (Family p1    	     any  Nothing) 			=  p1 <-> mary woman  <|> any     <|*|> Just NoKids
	map_to (Family p1            any  (Just (p2,kids)))	=  p1 <-> mary p2 	  <|> any     <|*|> Just kids

	map_from (p1 <-> Nothing <|> bs <|*|> _	)		   =   Family p1 bs Nothing
	map_from (p1 <-> Just p2 <|> bs <|*|> (Just kids)) =   Family p1 bs (Just (p2,kids))

	mary p 	= Just p
	man 	= Man ""     
	woman 	= Woman ""   
	notmaried = Nothing

gGEC{|Kids|} gecArgs pSt 
= Specialize NoKids KidsAGEC gecArgs pSt

KidsAGEC :: Kids -> AGEC(Kids)
KidsAGEC p = mkAGEC (to_BimapGEC bimapKids p) "Kids" 
where
	bimapKids = {map_to = map_to, map_from = map_from}
	where
		map_to (NoKids) 	  = (displaykids 0 <|*|> hor2listAGEC defaultfam [])
		map_to (Kids familie) = (displaykids (length familie) <|*|> hor2listAGEC defaultfam (number familie))
		map_from (_ <|*|> alist)  = case (^^ alist) of
								[] -> NoKids
								list -> Kids (unnumber list)

		defaultfam = (Text "  1:" <-> (Family (Man "") Single (Nothing)))

		displaykids 0 = Display "No Children "
		displaykids 1 = Display "1 Child "
		displaykids n = Display (toString n +++ " Children ")

		number kids = [(Text (toString i +++ ":") <-> kid) \\ i <- [1 ..] & kid <- kids]
		unnumber kids = [kid \\ (_ <-> kid) <- kids]

gGEC{|Person|} gecArgs pSt 
= Specialize (Man "") PersonAGEC gecArgs pSt
where
	PersonAGEC :: Person -> AGEC(Person)
	PersonAGEC p = mkAGEC (to_BimapGEC bimapPerson p) "Partner" 
	where
		bimapPerson = {map_to = map_to, map_from = map_from}
		where
			map_to (Man m) = m <|> Male
			map_to (Woman v) = v <|> Female
			map_from (m <|> Male)   = (Man m)
			map_from (v <|> Female)   = (Woman v)

:: Gender = Male | Female
derive gGEC Gender

// additional hacking conversions funcions just to hide constructors on one level

gGEC{|Maybe|} geca gecArgs pSt 
= Specialize Nothing (MaybeAGEC (gGEC{|*->*|} (gGEC{|*->*|} (gGEC{|*->*|} geca)))) gecArgs pSt
where
	MaybeAGEC :: (TgGEC (NoObject [YesObject a]) (PSt .ps)) (Maybe a) -> AGEC (Maybe a)
	MaybeAGEC gecspec n = mkxAGEC gecspec (to_BimapGEC bimapMaybe Nothing) "Maybe"
	where
		bimapMaybe = {map_to = map_to, map_from = map_from}
		where
			map_to (Nothing) =  NoObject [] 
			map_to (Just a)  =  NoObject [YesObject a]
	
			map_from (NoObject [])  		 = Nothing
			map_from (NoObject [YesObject a]) = Just a


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



/*
gGEC{|Partner|} gecArgs pSt 
= Specialize (Partner (Man "") NoKids) PartnerAGEC gecArgs pSt
where
	PartnerAGEC :: Partner -> AGEC(Partner)
	PartnerAGEC p = mkAGEC (to_BimapGEC bimapPartner p) "Partner" 
	where
		bimapPartner = {map_to = map_to, map_from = map_from}
		where
			map_to (Partner p kids) = p <|> kids
			map_from (p  <|> kids)   = (Partner p kids)
*/

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


			map_to (Nothing) = NoObject Nothing 
			map_to (Just a)  = NoObject (Just (YesObject a)) 

			map_from (NoObject Nothing) = Nothing
			map_from (NoObject (Just (YesObject a))) = Just a


gGEC{|(->)|} gGECa gGECb args=:{gec_value = Just (id), update = modeupdate} pSt
= createDummyGEC OutputOnly (id) modeupdate pSt
gGEC{|(->)|} gGECa gGECb args=:{gec_value = Nothing, update = modeupdate} pSt
= createDummyGEC OutputOnly (undef) modeupdate pSt

*/




