module pickRGB


//	**************************************************************************************************
//
//	This program creates a windows that allows a user to create a RGB colour.
//
//	The program has been written in Clean 1.3.2 and uses the Clean Standard Object I/O library 1.2
//	
//	**************************************************************************************************


import StdEnv, StdIO


::	NoState
	=	NoState


Start :: *World -> *World
Start world
	# (rgbid,world)		= openR2Id  world
	# (ids,  world)		= openIds 7 world
	# pickcontrol		= ColourPickControl rgbid ids initrgb Nothing
	= startColourPicker rgbid pickcontrol world
where
 	initrgb				= {r=MaxRGB,g=MaxRGB,b=MaxRGB}
	startColourPicker rgbid pickcontrol world
		= startIO SDI NoState NoState initialise [ProcessClose closeProcess] world
	where
		initialise pst
			# (rgbsize,pst)	= controlSize pickcontrol True Nothing Nothing Nothing pst
			# wdef			= Window "Pick a colour" pickcontrol
								[	WindowViewSize   rgbsize
								,	WindowPen        [PenBack LightGrey]
								]
			# mdef			= Menu "PickRGB"
								(	MenuItem "MinRGB" [	MenuFunction (noLS (set rgbid BlackRGB))
													  ,	MenuShortKey 'n'
													  ]
								:+:	MenuItem "MaxRGB" [	MenuFunction (noLS (set rgbid WhiteRGB))
													  ,	MenuShortKey 'm'
													  ]
								:+:	MenuSeparator     []
								:+:	MenuItem "Quit"   [	MenuFunction (noLS closeProcess)
													  ,	MenuShortKey 'q'
													  ]
								)	[]
			# (_,pst)		= openWindow undef wdef pst
			# (_,pst)		= openMenu   undef mdef pst
			= pst

		set rid rgb pst		= snd (syncSend2 rid (InSet rgb) pst)


/*	The definition of the text-slider component:	*/

::	RGBPickControl ls ps
	:==	:+: SliderControl TextControl ls ps

RGBPickControl :: RGBColour (String,Id,Id) Id (RGBColour->Int) (Int->RGBColour->RGBColour)
				 (Maybe ItemPos)
	-> RGBPickControl RGBColour (PSt .l .p)
RGBPickControl rgb (text,sid,tid) did get set maybePos
	=	  SliderControl Horizontal length sliderstate slideraction
													[ControlId sid:controlPos]
	  :+: TextControl   (ColourText text (get rgb))	[ControlId tid]
where
	controlPos	= case maybePos of
					Just pos	-> [ControlPos pos]
					_			-> []
	length		= PixelWidth (MaxRGB-MinRGB+1)
	sliderstate	= {sliderMin=MinRGB, sliderMax=MaxRGB, sliderThumb=get rgb}
	
	slideraction :: SliderMove (RGBColour,PSt .l .p) -> (RGBColour,PSt .l .p)
	slideraction move (rgb,pst)
		= (	newrgb
		  ,	appListPIO [ setSliderThumbs [(sid,y)]
		  			   , setControlTexts [(tid,ColourText text y)]
		  			   , SetColourBox did newrgb
		  			   ] pst
		  )
	where
		y		= case move of
					SliderIncSmall	-> min (get rgb+1 ) MaxRGB

					SliderDecSmall	-> max (get rgb-1 ) MinRGB
					SliderIncLarge	-> min (get rgb+10) MaxRGB
					SliderDecLarge	-> max (get rgb-10) MinRGB
					SliderThumb x	-> x
		newrgb	= set y rgb
	
ColourText :: String Int -> String
ColourText text x
	= text+++" "+++toString x



/*	The definition of a colour box:		*/

::	ColourBoxControl ls ps
	:==	CustomControl ls ps

ColourBoxControl :: RGBColour Id (Maybe ItemPos) -> ColourBoxControl .ls .ps
ColourBoxControl rgb did maybePos
	= CustomControl {w=40,h=40} (ColourBoxLook rgb)
			[	ControlId did
			:	case maybePos of (Just pos) -> [ControlPos pos];_->[]
			]

ColourBoxLook :: RGBColour SelectState UpdateState *Picture -> *Picture
ColourBoxLook colour _ {newFrame} picture
	# picture	= setPenColour	(RGB colour) picture
	# picture	= fill			newFrame	 picture
	# picture	= setPenColour	Black		 picture
	# picture	= draw			newFrame	 picture
	= picture

SetColourBox :: Id RGBColour (IOSt .l .p) -> IOSt .l .p
SetColourBox id rgb iost
	= setControlLooks [(id,True,(True,ColourBoxLook rgb))] iost



/*	The definition of the RGB access control:	*/

::	In		=	InGet				| InSet RGBColour
::	Out		=	OutGet RGBColour	| OutSet
::	RGBId	:==	R2Id In Out

::	ColourPickAccess ps	:==	Receiver2 In Out RGBColour ps

ColourPickAccess :: RGBId [(String,Id,Id)] Id -> ColourPickAccess (PSt .l .p)
ColourPickAccess rid rgbpicks did
	= Receiver2 rid accessRGB []
where
	accessRGB :: In (RGBColour,PSt .l .p) -> (Out,(RGBColour,PSt .l .p))
	accessRGB InGet (rgb,ps)
		= (OutGet rgb,(rgb,ps))
	accessRGB (InSet rgb=:{r,g,b}) (_,ps=:{io})
		# io	= SetColourBox    did rgb io
		# io	= setSliderThumbs (map (\(y,(_,sid,_))->(sid,y)) settings) io
		# io	= setControlTexts (map (\(y,(text,_,tid))->(tid,ColourText text y)) settings) io
		= (OutSet,(rgb,{ps & io=io}))
	where
		settings= zip2 [r,g,b] rgbpicks



/*	The definition of the assembled colour picking control:	*/

::	ColourPickControl ls ps
/*	:==	(	CompoundControl
			(	:+: (CompoundControl (ListLS RGBPickControl)))
			(	:+:	ColourBoxControl
					ColourPickAccess
			))
		) ls ps
*/	:==	NewLS
		(	CompoundControl
			(	:+:	(CompoundControl (ListLS (:+: SliderControl TextControl)))
			(	:+:	CustomControl
					(Receiver2 In Out)
			))
		)	ls	ps

ColourPickControl :: RGBId [Id] RGBColour (Maybe ItemPos) -> ColourPickControl .ls (PSt .l .p)
ColourPickControl rgbid ids initrgb maybePos
	= {	newLS = initrgb
	  ,	newDef= CompoundControl
					(	CompoundControl
					(	ListLS
					[	RGBPickControl initrgb rpicks did (\rgb->rgb.r) (\x rgb->{rgb&r=x}) left
					,	RGBPickControl initrgb gpicks did (\rgb->rgb.g) (\x rgb->{rgb&g=x}) left
					,	RGBPickControl initrgb bpicks did (\rgb->rgb.b) (\x rgb->{rgb&b=x}) left
					])	[ControlHMargin 0 0,ControlVMargin 0 0]
					:+: ColourBoxControl initrgb did Nothing
					:+:	ColourPickAccess rgbid [rpicks,gpicks,bpicks] did
					)	(case maybePos of (Just pos) -> [ControlPos pos]; _->[])
	  }
where	
	[rid,rtid,gid,gtid,bid,btid,did:_]	= ids
	(rtext,gtext,btext)					= ("Red","Green","Blue")
	(rpicks,gpicks,bpicks)				= ((rtext,rid,rtid),(gtext,gid,gtid),(btext,bid,btid))
	left								= Just (Left,NoOffset)
