/*
	Examples of Debug behaviour

	Version 1.0.2
	Ronny Wichers Schreur
	ronny@cs.kun.nl
*/
module Examples

import StdEnv

/*
	these examples use Ronny's syntax (->>, <<- and <<->>)
	and options (see RWSDebug.icl)
*/
import RWSDebug

// choose your example here
Start
	=	example1

/*
	a <<- b (debugBefore)
		print b, then evaluate a
*/
example1
	=	abort "example1 value\n" <<- "example1 debug"

/*
	a ->> b (debugAfter)
		evaluate a, then print b
*/
example2
	=	abort "example2 value\n" ->> "example2 debug"

/*
	<<->> a (debugValue)
		print and evaluate a, value can be unique
*/
example3
	=	<<->> "example3"

/*
	debugging also works with infinity values (provided you
	limit the debug output with the DebugMax... options)
*/
example4
	=	"example4" <<- [1..]

/*
	debugging with algebraic values
*/
:: List a
	=	Nil
	|	Cons a (List a)

example5
	=	"example5" <<- Cons 1 (Cons 2 Nil)

/*
	debugging with a record value, note that the field names
	don't appear in the debug output (this information isn't
	available at run-time)
*/
:: R	= {f1 :: Int, f2 :: Int}

example6
	=	"example6" <<- {f1 = 1, f2 = 2}

/*
	debugging with arrays 
*/
example7
	=	"example7" <<- array
	where
		array :: {Int}
		array
			=	{1, 2, 3, 4, 5}

/*
	debugging with closures 
*/
example8
	=	"example8" <<- (take, take 5, take 5 ['Brubeck'])

/*
	debugging may evaluate values that wouldn't otherwise
	be evaluated
*/
example9
	=	hd (<<->> ["example9" : undef])

/*
	debugging may effect strictness, in this example f is not
	strict in its first argument because of the debug function
*/
example10
	=	f "example" "10"
	where
		f a b
			=	(a <<- "f") +++ b

/*
	debugging depends on the evalution order, you'll have to
	understand the evalution order to understand in which order
	the debug values will be printed
*/
example11
	=	fst (concatFirstTwo ["exam","ple11"])
	where
		concatFirstTwo
			=	(get ->> "get first") `bind` \first
			->	(get ->> "get second") `bind` \second
			->	return (first+++second) ->> "return"
	get [h:t]
		=	(h, t)

