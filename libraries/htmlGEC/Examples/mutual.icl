module mutual

import StdEnv
import StdHtml

derive gForm  Pounds, Euros
derive gUpd   Pounds, Euros
derive gPrint Pounds, Euros
derive gParse Pounds, Euros

:: Pounds = {pounds :: Real}                        
:: Euros  = {euros  :: Real}                        

Start world  = doHtml mutual world


helloWorld hst
= (Html (Head [] []) (Body [] [mybody]),hst)
where
	mybody = Txt "Hello World"

/*
	mkHtml s tags hst 	= (Html (header s) (body tags),hst)
	header s 			= Head [`Hd_Std [Std_Title s]] [] 
	body tags 			= Body [] tags

*/


mutual hst
# ((_,body),hst) = startCircuit circuit initEuros hst
= mkHtml "Mutual Recursive Form"
	[ H1 [] "Example of a Mutual recursive form"
	, Br
	, BodyTag body
	, Br,Br,Br
	]  hst
where
	circuit :: GecCircuit Euros Euros
	circuit = feedback (edit "euros") (arr toPounds >>> edit "pounds" >>> arr toEuros)

	toPounds :: Euros -> Pounds                         
	toPounds {euros} = {pounds = euros / exchangerate}  
	                                                    
	toEuros :: Pounds -> Euros                          
	toEuros {pounds} = {euros = pounds * exchangerate} 
	
	initEuros  = {euros = 3.5}      
	
	exchangerate = 1.4                                  

	mkHtml s tags hst 	= (Html (header s) (body tags),hst)
	header s 			= Head [`Hd_Std [Std_Title s]] [] 
	body tags 			= Body [] tags

 