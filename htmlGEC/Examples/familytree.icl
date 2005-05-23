module familytree

import StdEnv
import StdHtml

derive gForm  Status, Partner, [], Gender, Maybe, Maybe`
derive gUpd   Family, Status, Partner, Person, Kids, Maybe, [], Gender, Maybe`
derive gPrint Family, Status, Partner, Person, Kids, Maybe, Gender, Maybe`
derive gParse Family, Status, Partner, Person, Kids, Maybe, Gender, Maybe`

:: Family	=	Family  Person Status (Maybe` (Person,Kids))
:: Status	=	Married			
			|	Divorced		
			|	Single 
:: Partner	=	Partner Person Kids 
:: Kids		=	Kids [Family] | NoKids
:: Person 	= 	Man		String 						
			| 	Woman 	String

Start world  = doHtml familytree world

familytree hst
# (p,hst) = personeditor "xx" inittree2 Edit hst
# (tree,hst)= mkEditForm "famtree" inittree Edit hst	
= mkHtml "Family Tree Example"
		[ H1 [] "family Tree Example: "
		, toBody tree
		, Br
		, toHtml tree.changed
		, Br, toBody p
//		, traceHtmlInput
		] hst
where
	inittree = Family (Man "Rinus") Married (Just_ (Woman "Marie-Jose", NoKids))
	inittree2 = Man "Rinus"
	inittree3 = "Rinus" <|> Male

	mkHtml s tags hst 	= (Html (header s) (body tags),hst)
	header s 			= Head [`Hd_Std [Std_Title s]] [] 
	body tags 			= Body [] tags


//derive gForm Family
//derive gForm Person
//derive gForm Kids

// specialization of the family tree to give it a user defined look and feel

:: Gender = Male | Female

gForm{|Person|} formid person mode hst = specialize personeditor formid person mode hst

personeditor formid d mode hst = mkBimapEditor formid d mode {map_to = map_to, map_from = map_from} hst
where
	map_to (Man m) 		= (m <|> Male)
	map_to (Woman w) 	= (w <|> Female)
	
	map_from (m <|> Male)	= (Man m)
	map_from (w <|> Female)	= (Woman w)

gForm{|Family|} formid family mode hst = specialize editor formid family mode hst
where
	editor formid d mode hst
	# (family,hst) = mkBimapEditor formid d mode {map_to = map_to, map_from = map_from} hst
	= ({ changed = family.changed, value = family.value, body = tab (nrkids family.value) family.body},hst)
	
	
	nrkids (Family  p s (Just_ (p2,Kids kids))) = length kids
	nrkids else = 0

	tab n body = [[Pre [] [Txt (mktab n)]] <=> body]
	
	mktab 0 = ""
	mktab n = "          " +++ mktab (n-1)

	map_to (Family p1 			 Single _) 					=  p1 <-> notmaried   <|> Single  <|> Nothing_
	map_to (Family p1=:(Woman v) any    Nothing_) 			=  p1 <-> mary man    <|> any     <|> Just_ NoKids
	map_to (Family p1    	     any  	Nothing_) 			=  p1 <-> mary woman  <|> any     <|> Just_ NoKids
	map_to (Family p1            any  	(Just_ (p2,kids)))	=  p1 <-> mary p2 	  <|> any     <|> Just_ kids

	map_from (p1 <-> Just_ p2 <|> bs <|> Just_ kids) 	=   Family p1 bs (Just_ (p2,kids))
	map_from (p1 <-> Just_ p2 <|> bs <|> Nothing_) 	=   Family p1 bs (Just_ (p2,NoKids))
	map_from (p1 <-> Nothing_ <|> bs <|> _	)		=   Family p1 bs Nothing_

	mary p 	= Just_ p
	man 	= Man ""     
	woman 	= Woman ""   
	notmaried = Nothing_

gForm{|Kids|} formid kids mode hst = specialize editor formid kids mode hst 
where
	editor formid d mode hst 
	# (list,hst) 	= horlist2Form (mkid "hlist") defaultfam mode (fromKids kids) hst
	# (display,hst) = mkEditForm   (mkid "displ") (displaykids (length list.body)) Display  hst
	= ({ changed = list.changed, value = toKids list.value, body = [display.body <||> list.body]},hst)

	mkid s = toString (length (fromKids kids)) +++ s

	defaultfam = Family (Man "") Single (Nothing_)

	displaykids 0 = DisplayMode "No Children "
	displaykids 1 = DisplayMode "1 Child "
	displaykids n = DisplayMode (toString n +++ " Children ")

	fromKids (Kids kids) 	= kids
	fromKids NoKids			= []

	toKids [] = NoKids
	toKids kids = Kids kids

:: Maybe` a = Just_ a | Nothing_

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

defval` = defval {|*|}

generic count a :: a -> Int


