implementation module htmlPrintUtil

import StdEnv, ArgEnv
import StdGeneric

:: FtoF :== (*File -> *File)
:: Url 	:== String

generic gHpr a :: !FtoF !a -> FtoF

gHpr{|String|} prev s = prev <+> print s	// the only entry that actualy prints something
											// all others eventually come here converted to string

gHpr{|Int|}    prev i = prev <+ toString i
gHpr{|Real|}   prev r = prev <+ toString r
gHpr{|Bool|}   prev b = prev <+ toString b
gHpr{|Char|}   prev c = prev <+ toString c
gHpr{|UNIT|}   				prev _ 				= prev
gHpr{|PAIR|}   gHpra gHprb 	prev (PAIR a b) 	= gHprb (gHpra prev a) b
gHpr{|EITHER|} gHprl gHprr 	prev (LEFT left) 	= gHprl prev left
gHpr{|EITHER|} gHprl gHprr 	prev (RIGHT right) 	= gHprr prev right
gHpr{|OBJECT|} gHpro 		prev (OBJECT object)= gHpro prev object 

gHpr{|CONS of t|} gPrHtmlc prev (CONS c) // constructor names are printed, prefix Foo_ is stripped
= case t.gcd_name.[0] of
	'`' 	= 	gPrHtmlc prev c	// just skip this constructor name
	else	=	case t.gcd_arity of
					0 = prev <+ " \"" <+ myprint t.gcd_name	<+ "\""
//					0 = prev <+   myprint t.gcd_name	 
					1 = gPrHtmlc (prev <+ " " <+ myprint t.gcd_name <+ " = ") c	
					n = gPrHtmlc (prev <+ " " <+ myprint t.gcd_name         ) c
where
	myprint :: String -> String
	myprint string = {toLower char \\ char <-: stripprefix string } 

	stripprefix string 
	# list = fromString string
	| isMember '_' list = toString (tl (dropWhile ((<>) '_') list))
	| otherwise 		= string  


gHpr{|[]|} gHlist prev list = myfold prev list 
where
	myfold prev [x:xs] = myfold (gHlist prev x) xs
	myfold prev [] = prev


// outility print functions based on gHpr

print :: !String -> FtoF
print a = \f -> fwrites a f

(<+) infixl :: !FtoF !a -> FtoF | gHpr{|*|} a
(<+) prev new = gHpr{|*|} prev new

(<+>) infixl :: !FtoF !FtoF -> FtoF
(<+>) prev new = new o prev

:: Ouotes a = Ouotes a // for putting " around a term

gHpr{|Quotes|} gHpra prev (Quotes a) = print "\"" <+> gHpra prev a <+ "\""

:: Spaces a = Spaces a // for putting spaces around a term

gHpr{|Spaces|} gHpra prev (Spaces a) = print " " <+> gHpra prev a <+ " "

print_to_stdout :: a *World -> *World | gHpr{|*|} a
print_to_stdout value world
# (out,world) = stdio world
# out = (print "" <+ value) out
= force_IO out world
where
	force_IO:: !x *World -> *World
	force_IO x w = w

htmlCmnd :: !a !b -> FtoF | gHpr{|*|} a & gHpr{|*|} b
htmlCmnd hdr txt =  (openCmnd hdr "" <+ txt) <+> closeCmnd hdr			

openCmnd :: !a !b -> FtoF | gHpr{|*|} a & gHpr{|*|} b
openCmnd  hdr attr =  print "<"  <+ hdr <+ attr <+ ">"

closeCmnd :: !a -> FtoF | gHpr{|*|} a
closeCmnd hdr =  print "</" <+ hdr <+ ">"

htmlAttrCmnd 	:: !hdr !attr !body  	-> FtoF | gHpr{|*|} hdr & gHpr{|*|} attr & gHpr{|*|} body
htmlAttrCmnd hdr attr txt 
= openCmnd hdr attr <+ txt <+> closeCmnd hdr

htmlAttr :: !String !a -> Spaces FtoF | gHpr{|*|} a
htmlAttr attrname attrvalue = (Spaces (print attrname <+ " = " <+ Quotes attrvalue))


