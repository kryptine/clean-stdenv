implementation module RWSDebug

import ShowDebug

show :: DebugShowFunction .a
show
	=	debugShow [DebugMaxChars 79, DebugMaxDepth 5, DebugMaxBreadth 20]

class (<<-) infix 0 a  :: .a !b -> .a
class (->>) infix 0 a  :: !.a !b -> .a
class <<->> a :: !.a -> .a

instance <<- a where
	(<<-) value debugValue
		=	debugBefore debugValue show value

instance ->> a where
	(->>) value debugValue
		=	debugAfter debugValue show value

instance <<->> a where
	<<->> value
		=	debugValue show value

instance <<- (a -> b) | <<- b where
	(<<-) f debugValue
		=	\a -> (f a <<- debugValue)

instance ->> (a -> b) | ->> b where
	(->>) f debugValue
		=	\a -> (f a ->> debugValue)

instance <<->> (a -> b) | <<->> b where
	<<->> f
		=	\a -> <<->> (f a)
