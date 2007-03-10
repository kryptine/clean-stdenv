definition module htmlSettings

import htmlHandler

// Global settings of iData applications

class iData a							// The collection of generic functions needed to make iData:	
		| gForm {|*|}					//		Creates an Html Form
		, gUpd  {|*|}					//		Makes it possible to edit the form and updates the corresponding value
		, iDataSerAndDeSerialize a
		
class iDataSerialize a
		| gPrint{|*|}			//		To serialize a value to a String

// OPTION: Comment out the next line if you do not have access to an ODCB database on your machine !!!!
		, gerda {|*|} 			//		To store and retrieve a value in a database

		, TC a					//		To be able to store values in a dynamic
								
class iDataSerAndDeSerialize a
		| gParse{|*|}					// To de-serialize a string back to a value
		, iDataSerialize a		

TraceInput			:== False			// show what kind of information is received from Client
TraceOutput			:== False			// show what kind of information is stored

DEBUGSERVER			:== True			// show what kind of information is transmitted by the Clean HtmlServer

MyDataBase			:== "iDataDatabase"	// name of database being used by iData applications

iDataIdSeparator 	 :== "."			// used as separator when combining iData form id's
radioButtonSeparator :== '.'			// used as extension for family of radiobuttons

// OPTION: Comment out the next line if you do not have access to an ODCB database on your machine !!!!
IF_GERDA gerda no_gerda :== gerda		// If database option is used

// OPTION: Remove the comment from the next line if you do not have access to an ODCB database on your machine !!!!
//IF_GERDA gerda no_gerda :== no_gerda	// otherwise, BUT manually flag of ", gerda{|*|}" in the class definition above
