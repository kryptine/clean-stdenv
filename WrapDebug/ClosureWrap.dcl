/*
	Wrap Clean closures.

	Version 1.0.4
	Arjen van Weelden
	arjenw@cs.kun.nl
	Ronny Wichers Schreur
	ronny@cs.kun.nl
*/
definition module ClosureWrap

from Wrap import ::WrappedNode, ::WrappedDescriptor,
					::UnwrappedArg, ::WrappedArg, class wrap

::  ClosureWrappedNode arg
	=	Closure !WrappedDescriptor !{!arg} !Int
	|	NotAClosure !(WrappedNode arg)
:: ClosureWrappedArg
	=	{closurearg :: ClosureDeeplyWrappedNode}

:: ClosureShallowlyWrappedNode
	:==	ClosureWrappedNode UnwrappedArg
:: ClosureDeeplyWrappedNode
	:==	ClosureWrappedNode ClosureWrappedArg

closureShallowWrapNode :: .a -> ClosureShallowlyWrappedNode
closureDeepWrapNode :: .a -> ClosureDeeplyWrappedNode

instance wrap (ClosureWrappedNode a) | wrap a
instance wrap ClosureWrappedArg

// check whether all functions have descriptors
closureHasDescriptor :: Bool