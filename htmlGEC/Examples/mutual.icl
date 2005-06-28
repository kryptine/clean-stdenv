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

mutual hst
# (mutual,hst) = startCircuit circuit initEuros hst
= mkHtml "Mutual Recursive Form"
	[ H1 [] "Example of a Mutual recursive form"
	, toBody mutual
	]  hst
where
	circuit :: GecCircuit Euros Euros
	circuit = feedback (edit (nFormId "euros")) (arr toPounds >>> edit (nFormId "pounds") >>> arr toEuros)

	toPounds :: Euros -> Pounds                         
	toPounds {euros} = {pounds = euros / exchangerate}  
	                                                    
	toEuros :: Pounds -> Euros                          
	toEuros {pounds} = {euros = pounds * exchangerate} 
	
	initEuros  = {euros = 3.5}      
	
	exchangerate = 1.4                                  

 