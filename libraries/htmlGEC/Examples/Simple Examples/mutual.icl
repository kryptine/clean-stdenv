module mutual

import StdEnv
import StdHtml

derive gForm  Pounds, Euros
derive gUpd   Pounds, Euros
derive gPrint Pounds, Euros
derive gParse Pounds, Euros

:: Pounds = {pounds :: Real}                        
:: Euros  = {euros  :: Real}                        

//Start world  = doHtml mutual world
Start world  = doHtmlServer test world

test hst
# (first,hst) = startCircuit circuit (0,0) hst
= mkHtml "first"
	[ H1 [] "test of first"
	, toBody first
	]  hst
where
	circuit = first (edit (nFormId "in" 0) >>> arr ((+) 1)) >>> display (nFormId "out" (0,0))



myEuroId :: (FormId Euros)
myEuroId	= nFormId "euros"  initEuros

myPoundsId :: (FormId Pounds)
myPoundsId	= nFormId "pounds" {pounds = 0.0}
initEuros	= {euros = 0.0}	

mutual hst
# (mutual,hst) = startCircuit circuit initEuros hst
= mkHtml "Mutual Recursive Form"
	[ H1 [] "Example of a Mutual recursive form"
	, toBody mutual
	]  hst
where
	circuit :: GecCircuit Euros Euros
	circuit = feedback (edit myEuroId) (arr toPounds >>> edit myPoundsId >>> arr toEuros)

	toPounds :: Euros -> Pounds                         
	toPounds {euros} = {pounds = euros / exchangerate}  
	                                                    
	toEuros :: Pounds -> Euros                          
	toEuros {pounds} = {euros = pounds * exchangerate} 
	
	exchangerate = 1.4                                  

 