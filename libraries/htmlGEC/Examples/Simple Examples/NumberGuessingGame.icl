module NumberGuessingGame

/**	This module implements the number guessing game.
	The program randomly selects a number between given bounds.
	The player tries to guess the selected number.
	The number of guesses are stored in a high-score file.
	These can be displayed on request.
*/
import StdEnv, StdHtml, Random

Start :: *World -> *World
Start world	= doHtmlServer numberGuessingGame world

bounds		= (low,up)
low			= 1
up			= 10

::	State	= { count	:: !Int
			  , guess	:: !Int
			  , seed	:: !RandomSeed
			  , high	:: ![(String,Int)]
			  }
derive gUpd   State
derive gForm  State
derive gParse State
derive gPrint State

mkState seed = {count=0,guess=low,seed=seed,high=[]}
incCount   st=:{count} = {st & count=count+1}
nextRandom st=:{seed}  = let (r,s) = random seed in {st & count=0,guess=low + (r mod (up-low)),seed=s}
addHigh pc st=:{high}  = nextRandom {st & high=insert insertHigh pc high}
where	insertHigh (newPl,newHi) (elemPl,elemHi) = newHi < elemHi || newHi == elemHi && newPl <= elemPl

derive gForm []
derive gUpd  []

numberGuessingGame :: *HSt -> (Html, *HSt)
numberGuessingGame hst
	# (nameF,     hst)	= nameForm "" hst
	# (playerF,   hst)	= playerForm  hst
	# (randomSeed,hst)	= accWorldHSt getNewRandomSeed hst
	# (stateF,    hst)	= stateForm randomSeed (\st -> if (low<=st.guess && st.guess<=up) st (nextRandom st)) hst
	# curCount			= stateF.value.count
	# (guessF,    hst)	= guessButtonForm hst
	# (newF,      hst)	= newButtonForm hst
	# (stateF,    hst)	= stateForm randomSeed (guessF.value o newF.value) hst
	# newCount			= stateF.value.count
	# guessNr			= stateF.value.guess
	# (addHighF,  hst)	= highButtonsForm (nameF.value,newCount) hst
	# (stateF,    hst)	= stateForm randomSeed addHighF.value hst
	# (displF,    hst)	= highForm stateF.value.high hst
	# pageTitle			= "Number Guessing Game"
	# header			= BodyTag [Txt "Your name is: ", BodyTag nameF.form, Br, Br]
	| playerF.value == guessNr && newCount > curCount
		= mkHtml pageTitle
			[ header
			, Txt ("Congratulations "<$nameF.value<$". You won in "<$newCount<$" turn"<$if (newCount>1) "s." ".")
			, Br, Br
			: map BodyTag [displF.form, addHighF.form, newF.form]
			] hst
	| otherwise
		= mkHtml pageTitle
			[ header
			, if (newCount > curCount)
				 (Txt ("The number to guess is "<$if (playerF.value < guessNr) "larger." "smaller.")) 
			     (Txt (if (nameF.value=="") "Please" (nameF.value<$", please")<$" guess a number between "<$low<$" and "<$up<$"."))
			, Br, Br
			: map BodyTag [playerF.form, guessF.form, [Br], newF.form]
			] hst
	
//	Form initializer functions:
playerForm			= mkEditForm   (nFormId "player")  (Init (low-1))		// The form in which the player enters guesses
stateForm r f		= mkStoreForm  (pFormId "state") (Init (mkState r)) f	// The store form that keeps track of the state of the application
nameForm name		= mkEditForm   (nFormId "name")     (Init name)		// The form in which the player can enter his/her name
highForm high		= vertlistForm (ndFormId "display") (Init high)		// The form that displays the high-score list
highButtonsForm pc	= ListFuncBut (nFormId "highButtons") (Init [(LButton (3*defpixel/2) "Add To High",addHigh pc)])	// Button to add result to high-score
guessButtonForm		= ListFuncBut (nFormId "guessbutton") (Init [(LButton defpixel "Guess",      incCount  )])		// Button to confirm number to guess
newButtonForm		= ListFuncBut (nFormId "newbutton")   (Init [(LButton defpixel "New Game",   nextRandom)])		// Button to start new game

//instance mod Int where mod a b = a - (a/b)*b
