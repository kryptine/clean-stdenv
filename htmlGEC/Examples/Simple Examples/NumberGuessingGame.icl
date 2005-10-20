module NumberGuessingGame

/**	This module implements the number guessing game.
	The program randomly selects a number between given bounds.
	The player tries to guess the selected number.
	The number of guesses are stored in a high-score file.
	These can be displayed on request.
*/
import StdEnv, StdHtml, Random

Start :: *World -> *World
Start world
	= doHtmlServer numberGuessingGame world

bounds = (1,10)

numberGuessingGame :: *HSt -> (Html, *HSt)
numberGuessingGame hst
	# (randomSeed,hst)	= accWorldHSt getNewRandomSeed hst
	# (r,randomSeed)	= random randomSeed
	# r					= low + (r mod (up-low))
	# (countF, hst)		= mkStoreForm countFormId 0 id hst
	# curCount			= countF.value
	# (guessF, hst)		= mkStoreForm guessFormId r id hst
	# (highF,  hst)		= mkStoreForm highFormId [("Peter",1),("Pieter",1),("Rinus",1)] id hst
	# (playerF,hst)		= mkEditForm playerFormId (low-1) hst
	# (funF,   hst)		= ListFuncBut False guessButtonFormId [(LButton defpixel "Guess", inc)] hst
	# (countF, hst)		= mkStoreForm countFormId curCount funF.value hst
	# newCount			= countF.value
//
	# (nameF,  hst)		= mkEditForm nameFormId "" hst
	# (okF,    hst)		= ListFuncBut False nameButtonFormId [(LButton defpixel "Ok", insert insertHigh (nameF.value,countF.value))] hst
	# (highF,  hst)		= mkStoreForm highFormId highF.value okF.value hst
	# (displF, hst)		= vertlistForm dispFormId highF.value hst
//
	| newCount > curCount && playerF.value == guessF.value || nameF.value<>""
		= hallOfFamePage countF guessF highF displF nameF okF hst
	| otherwise
		= guessPage (newCount > curCount) playerF guessF funF hst
where
	hallOfFamePage countF guessF highF displF nameF okF hst
		= mkHtml "Number Guessing Game" 
			[ Txt ("Congratulations. You won in "<$countF.value<$" turns.")
			, Txt "Please enter your name:", BodyTag nameF.form, BodyTag okF.form
			, BodyTag displF.form
			] hst
	
	guessPage nextNr playerF guessF funF hst
		= mkHtml "Number Guessing Game"
			[ if nextNr (Txt ("The number to guess is "<$if (playerF.value < guessF.value) "larger." "smaller.")) 
			            (Txt ("Guess a number between "<$low<$" and "<$up<$"."))
			, Br
			, BodyTag playerF.form
			, BodyTag funF.form
			] hst
	
	(low,up) = bounds

guessFormId			= nFormId "guess nr"
countFormId			= sFormId "count"
playerFormId		= nFormId "player"
guessButtonFormId	= nFormId "back"
nameFormId			= nFormId "name"
highFormId			= pFormId "high"
nameButtonFormId	= nFormId "nameOk"
dispFormId			= nFormId "display"

instance mod Int where mod a b = a - (a/b)*b

insertHigh (newPl,newHi) (elemPl,elemHi)
	= newHi < elemHi || newHi == elemHi && newPl <= elemPl

derive gForm []
derive gUpd  []
