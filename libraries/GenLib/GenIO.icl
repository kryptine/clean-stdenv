implementation module GenIO

import StdEnv, StdGeneric, StdMaybe

//--------------------------------------------------------------------------------
($) infixl 9
($) x y = y o x

(@) infix 8 
(@) x y = x y

:: Pos :== Int

//--------------------------------------------------------------------------------

:: *StringStream 	= { ss_str :: !*String, ss_pos :: !Pos}

instance IStream StringStream where
	streamRead s=:{ss_pos, ss_str}
		#! size_str = size ss_str
		| size_str == ss_pos 
			= (Nothing, {s & ss_str = ss_str})
		| otherwise
			#! ch = ss_str.[ss_pos]
			= (Just ch, {s & ss_str = ss_str, ss_pos = inc ss_pos})
	streamEnd s=:{ss_pos, ss_str}
		#! size_str = size ss_str
		= (size_str == ss_pos, {s & ss_str = ss_str})


instance OStream StringStream where
	streamWrite ch s=:{ss_str, ss_pos}
		#! new_str = realloc_if_needed ss_pos ss_str 
		= {s & ss_str = {new_str & [ss_pos] = ch}, ss_pos = inc ss_pos}
	where
		realloc_if_needed :: Int u:String -> v:String, [u <= v]
		realloc_if_needed pos str
			#! size_str = size str
			| pos == size_str
				#! new_str = createArray ((size_str + 1) * 3 /2) ' '
				#! (new_str, str) = fill 0 size_str new_str str
				= new_str
			| otherwise	
				= str 
		fill i n new_str str 
			| i == n
				= (new_str, str)
			| otherwise	
				#! (ch, str) = str![i] 
				= ({new_str & [i] = ch}, str)
				
instance RandomStream StringStream where
	streamGetPos s=:{ss_pos} = (ss_pos, s)
	streamSetPos pos s = {s & ss_pos = pos}

//-----------------------------------------------------------------------

instance IStream File where
	streamRead f 
		# (ok, c, f) = freadc f
		| ok
			= (Just c, f)
			= (Nothing, f)
	streamEnd f = fend f 

instance OStream File where
	streamWrite c f = fwritec c f

instance RandomStream File where
	streamSetPos pos f
		# (ok, f) = fseek f FSeekSet pos
		| not ok 	
			= abort "fseek failed\n"
			= f
	streamGetPos f = fposition f

//--------------------------------------------------------------------------------
:: Assoc = AssocNone | AssocLeft | AssocRight
:: GenIOEnv 
	= GIOE_None							// initial env
	| GIOE_Record						// record constructor	
	| GIOE_Tuple						// tuple constructor
	| GIOE_Nonfix						// normal nonfix constructor
	| GIOE_Infix String Assoc Int		// infix constructor

//--------------------------------------------------------------------------------

generic gOutput a :: GenIOEnv -> (a -> (*s -> *s)) | IStream s & RandomStream s
/*
gOutput{|Int|} env = undef
gOutput{|Real|} env = undef
gOutput{|Char|} env = undef
gOutput{|Bool|} env = undef
gOutput{|String|} env = undef
gOutput{|UNIT|} env = undef
gOutput{|PAIR|} fx fy env = undef
gOutput{|EITHER|} fl fr env = undef
gOutput{|CONS of d|} f env = undef
gOutput{|FIELD of d|} f env = undef
gOutput{|[]|} f env = undef
gOutput{|{}|} f env = undef
gOutput{|{!}|} f env = undef
*/
//--------------------------------------------------------------------------------

generic gInput a :: GenIOEnv -> (*s -> (Maybe a, *s)) | IStream s & RandomStream s
/*
gInput{|Int|} env = undef
gInput{|Real|} env = undef
gInput{|Char|} env = undef
gInput{|Bool|} env = undef
gInput{|String|} env = undef
gInput{|UNIT|} env = undef
gInput{|PAIR|} fx fy env = undef
gInput{|EITHER|} fl fr env = undef
gInput{|CONS of d|} f env = undef
gInput{|FIELD of d|} f env = undef
gInput{|[]|} f env = undef
gInput{|{}|} f env = undef
gInput{|{!}|} f env = undef
*/
//--------------------------------------------------------------------------------
