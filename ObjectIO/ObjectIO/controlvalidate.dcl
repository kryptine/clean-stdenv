definition module controlvalidate


//	Clean Object I/O library, version 1.2

//	Control validation.

import wstate


validateControlTitle	:: !String		-> String
validateSliderState		:: !SliderState	-> SliderState

getWElementControlIds	::			![WElementHandle .ls .pst]	-> (![Id],![WElementHandle .ls .pst])
getWElementControlIds`	::			![WElementHandle`]			-> [Id]

//	Id occurrence checks on [WElementHandle .ls .pst] and [WElementHandle`].
noDuplicateControlIds	::			![WElementHandle .ls .pst]	-> (!Bool,![WElementHandle .ls .pst])
noDuplicateControlIds`	::			![WElementHandle`]			-> Bool
disjointControlIds		:: ![Id]	![WElementHandle .ls .pst]	-> (!Bool,![WElementHandle .ls .pst])
disjointControlIds`		:: ![Id]	![WElementHandle`]			-> Bool
