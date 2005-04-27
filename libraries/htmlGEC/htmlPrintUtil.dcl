definition module htmlPrintUtil

// a collection of handy print routines to write html to Std Output
// (c) MJP 2005

import StdGeneric

:: FoF :== (*File -> *File)

// generic function for printing tags
// Constructors are converted to html tag strings
// prefix Name_ of Name_Attrname is removed, and Name is converted to lowercase string

generic gHpr a :: !*File !a -> *File		

derive gHpr UNIT, PAIR, EITHER, CONS, OBJECT
derive gHpr Int, Real, Bool, String, Char, []

// the main print routine

print_to_stdout 	:: a *World -> *World | gHpr{|*|} a

// handy utility print routines	

print 			:: !String 				-> *FoF
(<+) infixl 	:: !*File !a 			-> *File | gHpr{|*|} a
(<+>) infixl 	:: !*File *FoF 			-> *File
htmlAttrCmnd 	:: !hdr !tag !body  	-> *FoF | gHpr{|*|} hdr & gHpr{|*|} tag & gHpr{|*|} body
//htmlAttr 		:: !String !a 			-> Spaces *File 	| gHpr{|*|} a
openCmnd 		:: !a !b 				-> *FoF | gHpr{|*|} a & gHpr{|*|} b

// special types for creating small effects

:: Quotes a 	= Quotes a 			// for putting " " around a term
:: Spaces a 	= Spaces a 			// for putting spaces around a term

derive gHpr Quotes, Spaces
