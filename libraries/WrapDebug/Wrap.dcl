/*
	Wrap Clean nodes (for debugging purposes).

	Version 1.0.4
	Ronny Wichers Schreur
	ronny@cs.kun.nl
*/
definition module Wrap

from StdOverloaded import class toString (..)

::	WrappedDescriptorId

instance toString WrappedDescriptorId

::	WrappedDescriptor
    =   WrappedDescriptorCons
    |   WrappedDescriptorNil
    |   WrappedDescriptorTuple
    |   WrappedDescriptorOther !WrappedDescriptorId

::  WrappedNode
	//	basic types
    =   WrappedInt !Int
    |   WrappedChar !Char
    |   WrappedBool !Bool
    |   WrappedReal !Real
    |   WrappedFile !File

	// unboxed arrays of basic types
    |   WrappedString !{#Char}
    |   WrappedIntArray !{#Int}
    |   WrappedBoolArray !{#Bool}
    |   WrappedRealArray !{#Real}
    |   WrappedFileArray !{#File}

	// other arrays
    |   WrappedArray !{WrappedNode}

	// records
    |   WrappedRecord !WrappedDescriptor !{WrappedNode}

	// unboxed lists
    |   WrappedUnboxedList !WrappedDescriptor !{WrappedNode}

	// unboxed lists of records
    |   WrappedUnboxedRecordList !WrappedDescriptor !{WrappedNode}

	// other nodes
    |   WrappedOther !WrappedDescriptor !{WrappedNode}

wrapNode :: !.a -> WrappedNode
shallowWrapNode :: !.a -> ShallowlyWrappedNode
:: ExtNode
	=	E.a: {node :: !a}

::  ShallowlyWrappedNode
	//	basic types
    =   ShallowlyWrappedInt !Int
    |   ShallowlyWrappedChar !Char
    |   ShallowlyWrappedBool !Bool
    |   ShallowlyWrappedReal !Real
    |   ShallowlyWrappedFile !File

	// unboxed arrays of basic types
    |   ShallowlyWrappedString !{#Char}
    |   ShallowlyWrappedIntArray !{#Int}
    |   ShallowlyWrappedBoolArray !{#Bool}
    |   ShallowlyWrappedRealArray !{#Real}
    |   ShallowlyWrappedFileArray !{#File}

	// other arrays
    |   ShallowlyWrappedArray !{ExtNode}

	// records
    |   ShallowlyWrappedRecord !WrappedDescriptor !{ExtNode}

	// unboxed lists
    |   ShallowlyWrappedUnboxedList !WrappedDescriptor !{ExtNode}

	// unboxed lists of records
    |   ShallowlyWrappedUnboxedRecordList !WrappedDescriptor !{ExtNode}

	// other nodes
    |   ShallowlyWrappedOther !WrappedDescriptor !{ExtNode}

