implementation module dictionary


import StdArray, StdBool, StdEnum, StdInt, StdString, StdList, StdMisc


::	Dictionary
	=	{	isEntry	:: !Bool			// True iff this record is an entry
		,	head	:: !Int				// If isEntry: bit 0='a' .. bit 25='z'; otherwise: 0
		,	end		:: !Int				// If isEntry: bit i=0: not a word; bit i=1: it's a word; otherwise: 0
		,	tail	:: ![Dictionary]	// If isEntry: tail.[i]=tail of bit i; NoEntry if no more tails at bit i; otherwise: []
		}

//	Constants:
a	=: toInt 'a'

//	Create a new dictionary.
newDictionary :: Dictionary
newDictionary
	= {isEntry=False,head=0,end=0,tail=[]}

// Sorted list to dictionary.
sortlistToDictionary :: ![String] -> Dictionary
sortlistToDictionary words
	= addwords words newDictionary
where
	addwords :: ![String] !Dictionary -> Dictionary
	addwords [w:ws] d = addwords ws (addToDictionary w d)
	addwords _ d = d

//	Return all words of the dictionary in lexicographical order.
allMembers :: !Dictionary -> [String]
allMembers dictionary
	= allMembers` "" dictionary

allMembers` :: !String !Dictionary -> [String]
allMembers` prefix {isEntry,head,end,tail}
	| isEntry
		= alltails prefix 0 head end tail
	| otherwise
		= []
where
	alltails :: !String !Int !Int !Int ![Dictionary] -> [String]
	alltails prefix bit head end [tail:tails]
		| not (bitset bit head)
			= othertails
		| not (bitset bit end)
			= newtails++othertails
		| otherwise
			= [prefix+++charString:newtails++othertails]
	where
		othertails	= alltails prefix (bit+1) head end tails
		charString	= toString (toChar (a+bit))
		newtails	= allMembers` (prefix+++charString) tail
	alltails _ _ _ _ []
		= []

// Return all words starting with the given character.
membersStartingWith	:: !Char !Dictionary -> [String]
membersStartingWith char {isEntry,head,end,tail}
	| okchar char && isEntry
		= allMembers` (toString char) (tail!!bit)
	| otherwise
		= []
where
	bit	= toInt char - a

//	Add a word to the dictionary.
addToDictionary :: !String !Dictionary -> Dictionary
addToDictionary word dictionary
	| word=="" || not (all okchar chars)
		= dictionary
	| otherwise
		= addToDictionary` chars dictionary
where
	chars	= [c \\ c<-:word]
	
	addToDictionary` :: ![Char] !Dictionary -> Dictionary
	addToDictionary` [char:chars] entry=:{isEntry,head,end,tail}
		| isEmpty chars
			= {entry	& isEntry	= True
						, head		= setbit bit head
						, end		= setbit bit end
						, tail		= newtail
			  }
		| otherwise
			#! d	= addToDictionary` chars (newtail!!bit)
			#! t	= updateAt` bit d newtail
			= {entry	& isEntry	= True
						, head		= setbit bit head
						, tail		= t
			  }
	where
		bit		= toInt char - a
		newtail	= if isEntry tail (repeatn 26 newDictionary)
	addToDictionary` _ dictionary
		= dictionary
	
	updateAt` :: !Int .a !u:[.a] -> u:[.a]
	updateAt` n x [y:ys]
		| n==0
			= [x:ys]
		| otherwise
			#! ys` = updateAt` (n-1) x ys
			= [y : ys`]
	updateAt` n x []
		= []

//	True iff word exists in dictionary.
isMemberDictionary :: !String !Dictionary -> Bool
isMemberDictionary word dictionary
	| word=="" || not (all okchar chars)
		= False
	| otherwise
		= isMemberDictionary` chars dictionary
where
	chars	= [c \\ c<-:word]
	
	isMemberDictionary` :: ![Char] !Dictionary -> Bool
	isMemberDictionary` [char:chars] entry=:{isEntry,head,end,tail}
		| not isEntry || not (bitset bit head)
			= False
		| isEmpty chars
			= bitset bit end
		| otherwise
			= isMemberDictionary` chars (tail!!bit)
	where
		bit	= toInt char - a
	isMemberDictionary` _ _
		= False


// Return the number of words.
sizeDictionary :: !Dictionary -> Int
sizeDictionary {isEntry,end,tail}
	| isEntry	= bitcount end + (sum (map sizeDictionary tail))
	| otherwise	= 0
where
	bitcount :: !Int -> Int	// count the nr of 1 bits in the argument
	bitcount x
		= bitcount x 0
	where
		bitcount :: !Int !Int -> Int
		bitcount x c
			| x==0		= c
			| otherwise	= bitcount (x>>1) (if (isOdd x) (c+1) c)


//	Auxiliary functions:
okchar :: !Char -> Bool
okchar c = 'a'<=c && c<='z'

setbit :: !Int !Int -> Int
setbit i bits = (1<<i) bitor bits

bitset :: !Int !Int -> Bool
bitset i bits = (1<<i) bitand bits > 0
