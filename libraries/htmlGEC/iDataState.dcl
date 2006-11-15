definition module iDataState

// maintains the state of the iDate
// (c) 2005 - MJP

import StdMaybe
import GenParse, GenPrint
import htmlDataDef, EncodeDecode

// Maintaining the internal state of all forms

:: *FormStates 													// collection of all states of all forms

emptyFormStates		:: *FormStates								// creates emtpy states

findState 			:: !(FormId a) *FormStates *NWorld 			// find the state value given FormId and a correct type
					-> (Bool, Maybe a,*FormStates,*NWorld)		// true if form has not yet been previously inspected 	
												| iDataSerAndDeSerialize a		
replaceState 		:: !(FormId a) a *FormStates *NWorld 		// replace state given FormId
					-> (*FormStates,*NWorld)	| iDataSerialize a

getUpdateId 		:: *FormStates -> (String,*FormStates)		// id of previously changed form
getUpdate 			:: *FormStates -> (String,*FormStates)		// value typed in by user as string

// storage and retrieval of FormStates

retrieveFormStates 	:: ServerKind (Maybe String) *NWorld -> (*FormStates,*NWorld) 	// retrieves all form states hidden in the html page
storeFormStates 	:: !FormStates *NWorld -> (BodyTag,*NWorld)

// Triplet handling

:: Triplet			:== (String,Int,UpdValue)

:: UpdValue 													// the updates that can take place	
	= UpdI Int													// new integer value
	| UpdR Real													// new real value
	| UpdB Bool													// new boolean value
	| UpdC String												// choose indicated constructor 
	| UpdS String												// new piece of text

encodeTriplet		:: Triplet -> String						// encoding of triplets
decodeTriplet		:: String -> Maybe Triplet

getTriplet  		:: *FormStates -> (!Maybe Triplet,!Maybe b,*FormStates)  | gParse{|*|} b // inspect triplet

callClean 			:: Script									// script that takes care of sending the required input to this application

// fstate handling used for testing only

initTestFormStates 	::  *NWorld -> (*FormStates,*NWorld) 		// creates initial empty form states
setTestFormStates 	::  (Maybe Triplet) String String *FormStates *NWorld -> (*FormStates,*NWorld) 	// stores triplet updateid update in state 

