/*
	Show Wrapped Node

	Version 1.0.4
	Ronny Wichers Schreur
	ronny@cs.kun.nl
*/
definition module ClosureShowWrapped

from ClosureWrap import ::ClosureWrappedNode
from ShowWrapped import class showWrapped

instance showWrapped (ClosureWrappedNode arg) | showWrapped arg
