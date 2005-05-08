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
/*
gHpr{|CONS of t|} gPrHtmlc file (CONS c) // constructor names are printed, prefix Foo_ is stripped
= case t.gcd_name.[0] of
	'`' 	= 	gPrHtmlc file c	// just skip this constructor name
	else	=	case t.gcd_arity of
					0 = file <+   myprint t.gcd_name	 
					1 = gPrHtmlc (file <+ " " <+ myprint t.gcd_name <+ " = ") c	
					n = gPrHtmlc (file <+ " " <+ myprint t.gcd_name         ) c
where
	myprint :: String -> String
	myprint string = {toLower char \\ char <-: stripprefix string } 

	stripprefix string 
	# list = fromString string
	| isMember '_' list = toString (tl (dropWhile ((<>) '_') list))
	| otherwise 		= string  

*/
gHpr{|CONS of t|} gPrHtmlc prev (CONS c) // constructor names are printed, prefix Foo_ is stripped
= case t.gcd_name.[0] of
	'`' 	= 	gPrHtmlc prev c	// just skip this constructor name
	else	=	case t.gcd_arity of
//					0 = prev <+ " \"" <+ myprint t.gcd_name	<+ "\""
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

print_to_stdout :: a *World -> *World | gHpr{|*|} a
print_to_stdout value world
# (file,world) = stdio world
# file = file <+ value
= force_IO file world
where
	force_IO:: !x *World -> *World
	force_IO x w = w

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
styleCmnd stylename attr = \file -> print "." file <+ stylename <+ " { \r" <+ attr <+ " }\r"

styleAttrCmnd :: !a !b -> FoF | gHpr{|*|} a & gHpr{|*|} b
styleAttrCmnd name value = \file -> print "\t" file <+ name <+ ": " <+ value <+ ";\r"


