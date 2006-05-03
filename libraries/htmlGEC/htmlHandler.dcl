definition module htmlHandler

// Main entrance for en user
// Generating HTML code, converting Clean types to GEC's, automatic dealing with form's ..
// (c) MJP 2005 *** under construction

import htmlDataDef, htmlFormData
import StdBool
import GenPrint, GenParse

TraceInput :== True		// set it to True if you want to see what kind of information is received from browser

derive bimap Form, FormId

//:: *HSt 					// unique state required for creating Html forms
:: *HSt 		= 	{ cntr 		:: Int // InputId		// counts position in expression
					, states 	:: *FormStates  // all form states are collected here ... 	
					, world		:: *NWorld		// to enable all other kinds of I/O
					}	

// doHtml main wrapper for generating & handling of a Html form

doHtml 			:: .(*HSt -> (Html,!*HSt)) *World -> *World  	// use this application with some external server and php
doHtmlServer 	:: (*HSt -> (Html,!*HSt))  *World -> *World 	// use this application with the build-in Clean server: http://localhost/clean

// mkViewForm is the swiss army nife function creating stateful interactive forms with a view v of data d
// make shure that all editors have a unique identifier !

mkViewForm 		:: !(InIDataId d) !(HBimap d v) !*HSt -> (Form d,!*HSt) | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC v

// gForm converts any Clean type to html code (form) to be used in a body
// gUpd updates a value of type t given any user input in the html form

generic gForm a :: !(InIDataId a) !*HSt -> *(Form a, !*HSt)		// user defined gForms: use "specialize"	
generic gUpd a 	:: UpdMode a -> (UpdMode,a)							// gUpd can simply be derived

derive gForm Int, Real, Bool, String, UNIT, PAIR, EITHER, OBJECT, CONS, FIELD//, (,) 
derive gUpd  Int, Real, Bool, String, UNIT, PAIR, EITHER, OBJECT, CONS, FIELD//, (,) 

// specialize has to be used if one wants to specialize gForm for a user-defined type

specialize :: !(!(InIDataId a) !*HSt -> (!Form a,!*HSt)) !(InIDataId a) !*HSt -> (!Form a,!*HSt) | gUpd {|*|} a
		
// utility functions

toHtml 			:: a -> BodyTag 				| gForm {|*|} a		// toHtml displays any type into a non-editable form
toHtmlForm 		:: (*HSt -> *(Form a,*HSt)) -> [BodyTag] 			// toHtmlForm displays any form one can make with a form function
												| gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
toBody 			:: (Form a) -> BodyTag								// just (BodyTag form.body)
createDefault 	:: a | gUpd{|*|} a 									// creates a default value of requested type

// definitions on HSt

instance FileSystem HSt												// enabling file IO on HSt

appWorldHSt		:: !.(*World -> *World)       !*HSt -> *HSt			// enabling World operations on HSt
accWorldHSt		:: !.(*World -> *(.a,*World)) !*HSt -> (.a,!*HSt)	// enabling World operations on HSt

// for testing

import iDataState

runUserApplication :: .(*HSt -> *(.a,*HSt)) *FormStates *NWorld -> *(.a,*FormStates,*NWorld)

// low level utility functions handy when specialize cannot be used, only to be used by experts !!

incrHSt 		:: Int *HSt -> *HSt											// Cntr := Cntr + 1
CntrHSt 		:: *HSt -> (Int,*HSt)										// Hst.Cntr
mkInput 		:: !Int !(InIDataId d) Value UpdValue *HSt -> (BodyTag,*HSt)	// Html Form Creation utility 
getChangedId	:: !*HSt -> (String,!*HSt)					// id of form that has been changed by user

:: UpdMode	= UpdSearch UpdValue Int		// search for indicated postion and update it
			| UpdCreate [ConsPos]			// create new values if necessary
			| UpdDone						// and just copy the remaining stuff
