implementation module htmlPrintUtil

import StdEnv, ArgEnv
import StdGeneric

:: Url 	:== String

generic gHpr a :: !*File !a -> *File

gHpr{|String|} file s = file <<< s	// the only entry that actualy prints something
											// all others eventually come here converted to string

gHpr{|Int|}    file i = file <<< i
gHpr{|Real|}   file r = file <<< r
gHpr{|Bool|}   file b = file <<< toString b
gHpr{|Char|}   file c = file <<< c
gHpr{|UNIT|}   				file _ 				= file
gHpr{|PAIR|}   gHpra gHprb 	file (PAIR a b) 	= gHprb (gHpra file a) b
gHpr{|EITHER|} gHprl gHprr 	file (LEFT left) 	= gHprl file left
gHpr{|EITHER|} gHprl gHprr 	file (RIGHT right) 	= gHprr file right
gHpr{|OBJECT|} gHpro 		file (OBJECT object)= gHpro file object 

gHpr{|CONS of t|} gPrHtmlc prev (CONS c) // constructor names are printed, prefix Foo_ is stripped
= case t.gcd_name.[0] of
	'`' 	= 	gPrHtmlc prev c	// just skip this constructor name
	else	=	case t.gcd_arity of
					0 = prev <+ " " <+ myprint t.gcd_name	 
					1 = gPrHtmlc (prev <+ " " <+ myprint t.gcd_name <+ " = ") c	
					n = gPrHtmlc (prev <+ " " <+ myprint t.gcd_name         ) c
where
	myprint :: String -> String
	myprint string = {toLower` char \\ char <-: stripprefix string }
	
	toLower` '_' = '-'
	toLower` c = toLower c 

	stripprefix string 
	# list = fromString string
	| isMember '_' list = toString (tl (dropWhile ((<>) '_') list))
	| otherwise 		= string  

gHpr{|[]|} gHlist file list = myfold file list 
where
	myfold file [x:xs] = myfold (gHlist file x) xs
	myfold file [] = file

// outility print functions based on gHpr

print :: !String -> FoF
print a = \f -> fwrites a f

(<+) infixl :: !*File !a -> *File | gHpr{|*|} a
(<+) file new = gHpr{|*|} file new

(<+>) infixl :: !*File FoF -> *File
(<+>) file new = new file

print_to_stdout :: a *NWorld -> *NWorld | gHpr{|*|} a
print_to_stdout value nw=:{worldC,inout}
# inout = inout <+ value
= {nw & inout = inout}

htmlCmnd :: !a !b -> FoF | gHpr{|*|} a & gHpr{|*|} b
htmlCmnd hdr txt =  \file -> closeCmnd hdr (openCmnd hdr "" file <+ txt) 			

openCmnd :: !a !b -> FoF | gHpr{|*|} a & gHpr{|*|} b
openCmnd  hdr attr =  \file -> file <<< "<"  <+ hdr <+ attr <+ ">"

closeCmnd :: !a -> FoF | gHpr{|*|} a
closeCmnd hdr =  \file -> print "</" file <+ hdr <+ ">"

htmlAttrCmnd 	:: !hdr !attr !body -> FoF | gHpr{|*|} hdr & gHpr{|*|} attr & gHpr{|*|} body
htmlAttrCmnd hdr attr txt 
= \file -> closeCmnd hdr (openCmnd hdr attr file <+ txt)


styleCmnd :: !a !b -> FoF | gHpr{|*|} a & gHpr{|*|} b
styleCmnd stylename attr = \file -> print "." file <+ stylename <+ "{" <+ attr <+ "}"

styleAttrCmnd :: !a !b -> FoF | gHpr{|*|} a & gHpr{|*|} b
styleAttrCmnd name value = \file -> print "" file <+ name <+ ": " <+ value <+ ";"

instance FileSystem NWorld
where
	fopen :: !{#Char} !Int !*NWorld -> (!Bool,!*File,!*NWorld)
	fopen string int nworld=:{worldC}
		# (bool,file,worldC) = fopen string int worldC
		= (bool,file,{nworld & worldC = worldC})

	fclose :: !*File !*NWorld -> (!Bool,!*NWorld)
	fclose file nworld=:{worldC}
		# (bool,worldC) = fclose file worldC
		= (bool,{nworld & worldC = worldC})

	stdio :: !*NWorld -> (!*File,!*NWorld)
	stdio nworld=:{worldC}
		# (file,worldC) = stdio worldC
		= (file,{nworld & worldC = worldC})

	sfopen :: !{#Char} !Int !*NWorld -> (!Bool,!File,!*NWorld)
	sfopen string int nworld=:{worldC}
		# (bool,file,worldC) = sfopen string int worldC
		= (bool,file,{nworld & worldC = worldC})

appWorldNWorld :: !.(*World -> *World) !*NWorld -> *NWorld
appWorldNWorld f nw=:{worldC}
	= {nw & worldC=f worldC}

accWorldNWorld :: !.(*World -> *(.a,*World)) !*NWorld -> (.a,!*NWorld)
accWorldNWorld f nw=:{worldC}
	# (a,worldC)	= f worldC
	= (a,{nw & worldC=worldC})
