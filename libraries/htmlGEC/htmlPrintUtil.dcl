definition module htmlPrintUtil

// a collection of handy print routines to write html to Std Output
// (c) MJP 2005

import StdGeneric
import StdFile

:: FoF :== (*File -> *File)

:: *NWorld							// io interface
	= 	{ worldC	:: !*World		// world for any io
		, inout		:: !*File		// to read from stdin and write to srdout
		}				

:: UpdValue 					// the updates that can take place	
	= UpdI Int					// new integer value
	| UpdR Real					// new real value
	| UpdB Bool					// new boolean value
	| UpdC String				// choose indicated constructor 
	| UpdS String				// new piece of text

instance FileSystem NWorld


// generic function for printing tags
// Constructors are converted to html tag strings
// prefix Name_ of Name_Attrname is removed, and Name is converted to lowercase string

generic gHpr a :: !*File !a -> *File		

derive gHpr UNIT, PAIR, EITHER, CONS, OBJECT
derive gHpr Int, Real, Bool, String, Char, []

// the main print routine

print_to_stdout 	:: a *NWorld -> *NWorld | gHpr{|*|} a

// handy utility print routines	

print 			:: !String 				-> FoF
(<+) infixl 	:: !*File !a 			-> *File | gHpr{|*|} a
(<+>) infixl 	:: !*File FoF 			-> *File
htmlAttrCmnd 	:: !hdr !tag !body  	-> FoF | gHpr{|*|} hdr & gHpr{|*|} tag & gHpr{|*|} body
openCmnd 		:: !a !b 				-> FoF | gHpr{|*|} a & gHpr{|*|} b
styleCmnd 		:: !a !b 				-> FoF | gHpr{|*|} a & gHpr{|*|} b
styleAttrCmnd 	:: !a !b 				-> FoF | gHpr{|*|} a & gHpr{|*|} b

