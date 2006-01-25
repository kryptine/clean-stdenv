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

::	State	= { count	:: !Int
			  , guess	:: !Int
			  , high	:: ![(Int,String)]
			  }
derive gUpd   State
derive gForm  State
derive gParse State
derive gPrint State

derive gForm []
derive gUpd  []

//	Form initializer functions:
nameForm 		= mkEditForm   (Init, nFormId "name" "")      			// The form in which the player can enter his/her name
guessForm		= mkEditForm   (Init, nFormId "guess" (low-1))  		// The form in which the player enters guesses
stateForm f		= mkStoreForm  (Init, pFormId "state" (newGame nullRandomSeed Nothing [])) f	// The store form that keeps track of the state of the application
highForm high	= vertlistForm (Set,  ndFormId "display" high) 			// The form that displays the high-score list

low			= 1
up			= 10

newGame seed mbname high	= {count=1, guess=nextNumber seed, high = insert mbname high}
where
	insert Nothing list 			= list
	insert (Just namecount) list 	= [namecount:list]

	nextNumber seed = let (new,_) 	= random seed in low + (new mod (up-low))

adjustState newname newguess seed name guess state
| newname 					= newGame seed Nothing state.high
| newguess
	| guess == state.guess 	= newGame seed (Just (state.count,name)) state.high 
	| otherwise = {state & count = state.count + 1}
= state

numberGuessingGame :: *HSt -> (Html, *HSt)
numberGuessingGame hst
	# (ostate, hst)		= stateForm id hst							// get state
	# (name, hst)		= nameForm hst								// get name
	# (guess, hst)		= guessForm hst								// get new quess

	# (randomSeed,hst)	= accWorldHSt getNewRandomSeed hst			// create random number seed
	# (nstate, hst)		= stateForm 		 						// adjust state
							(\st -> adjustState name.changed guess.changed randomSeed name.value guess.value st) hst

	= mkHtml "Number Guessing Game"
	  (	[ Txt ("Type in your name and guess a number between "<$ low <$ " and " <$ up <$ ".")
	  	, Br, Br
		, name.form 
	  	<||> guess.form
	  	, Br 
	  	] ++ 
		(if guess.changed
			(if (guess.value == ostate.value.guess)
				[ Txt ("Congratulations " <$ name.value <$ ".")
				, Br 
				, Txt ("You have guessed the number in " <$ ostate.value.count <$ " turn" <$ if (ostate.value.count>1) "s." ".")
				, Br, Br 
				, Txt "Here follows the list of fame: "
				, Br, Br 
				, BodyTag (toHtmlForm (highForm (sort nstate.value.high)))
				, Br, Br 
				, Txt ("Just type in a new number if you want to guess again...")
				] 
				[ Txt ("Sorry, " <$ name.value <$ ", your guess number " <$ ostate.value.count <$ " was wrong.")
				, Br, Br
				, Txt ("The number to guess is "<$if (guess.value < ostate.value.guess) "larger." "smaller.") 
				]
			)
			[])
		) hst

instance mod Int where mod a b = a - (a/b)*b

/* old code

/**	This module implements the number guessing game.
	The program randomly selects a number between given bounds.
	The player tries to guess the selected number.
	The number of guesses are stored in a high-score file.
	These can be displayed on request.
*/

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
playerForm			= mkEditForm   (Init, nFormId "player" (low-1))  		// The form in which the player enters guesses
stateForm r f		= mkStoreForm  (Init, pFormId "state" (mkState r)) f	// The store form that keeps track of the state of the application
nameForm name		= mkEditForm   (Init, nFormId "name" name)      		// The form in which the player can enter his/her name
highForm high		= vertlistForm (Init, ndFormId "display" high) 			// The form that displays the high-score list
highButtonsForm pc	= ListFuncBut  (Init, nFormId "highButtons" [(LButton (3*defpixel/2) "Add To High",addHigh pc)]) 	// Button to add result to high-score
guessButtonForm		= ListFuncBut  (Init, nFormId "guessbutton" [(LButton defpixel "Guess",      incCount  )]) 		// Button to confirm number to guess
newButtonForm		= ListFuncBut  (Init, nFormId "newbutton" [(LButton defpixel "New Game",   nextRandom)])   		// Button to start new game

instance mod Int where mod a b = a - (a/b)*b
*/

