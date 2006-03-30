definition module htmlRefFormlib

import StdEnv, htmlHandler, htmlButtons

derive gForm 	Refto
derive gUpd 	Refto
derive gPrint 	Refto
derive gParse 	Refto

// The Refto structure is used to refer to a file with indicated name containing a value of indicated type
// This can be used to share information, the file name is used as key
// The file is openend read-only (Mode = Display) or it can be edited and the new value will be written to file

:: Refto a = Refto String

reftoEditForm 		:: 		!Mode !Init !(InIDataId  (Refto a,a))  !*HSt -> (Form (Refto a),Form a,!*HSt)   | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
reftoVertListForm 	:: 		!Mode !Init !(InIDataId [(Refto a,a)]) !*HSt -> (Form [Refto a],Form [a],!*HSt) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
reftoListFormButs 	:: !Int	!Mode !Init !(InIDataId [(Refto a,a)]) !*HSt -> (Form [Refto a],Form [a],!*HSt) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a

universalDB 		:: !Init !(String a -> Judgement) !String !a *HSt -> (a,!*HSt)   	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
universalRefEditor 	:: !Mode  !(a -> Judgement) !(Refto a) *HSt -> (Form a,!*HSt)   	| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a

invokeRefEditor 	:: (!(InIDataId b) !*HSt -> (Form d,!*HSt)) (InIDataId b) !*HSt -> (Form b,!*HSt)

// Usefull for exception handling

:: Judgement		:==	Maybe (String,String)	// id + message					
Ok 					::	Judgement

instance + Judgement

ExceptionStore :: (Judgement -> Judgement) *HSt -> (Judgement,!*HSt)
