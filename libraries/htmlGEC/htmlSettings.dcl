definition module htmlSettings

import htmlHandler

// Global settings of iData applications

class iData a					// The collection of generic functions needed to make iData:	
		| gForm {|*|}			//		Creates an Html Form
		, gUpd  {|*|}			//		Makes it possible to edit the form and updates the corresponding value
		, gPrint{|*|}			//		To serialize a form to a String
		, gParse{|*|}			//		To de-serialize the string back to a value
		, gerda {|*|} 			//		To store and retrieve a value in a database
		, TC a					//		To be able to store values in a dynamic
								//		TC is a special class cannot be included here

TraceInput :== False			// set it to True if you want to see what kind of information is stored

MyDataBase :== "iDataDatabase"	// name of database being used by iData applications

iDataIdSeparator :== "."		// used as separator when combining iData form id's
radioButtonSeparator :== '.'	// used as extension for family of radiobuttons