/*
	Show Wrapped Node

	Version 1.0.2
	Ronny Wichers Schreur
	ronny@cs.kun.nl
*/
definition module ShowWrapped

//1.3
from Wrap import WrappedNode
//3.1
/*2.0
from Wrap import :: WrappedNode
0.2*/

showWrapped :: WrappedNode -> [{#Char}]