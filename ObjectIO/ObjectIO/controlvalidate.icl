implementation module controlvalidate


//	Clean Object I/O library, version 1.2


import	StdBool, StdInt, StdList
import	ospicture, oswindow
import	commondef, windowhandle, wstate


//	Validate the title of a control.

validateControlTitle :: !String -> String
validateControlTitle string
	= RemoveSpecialChars OSControlTitleSpecialChars string


//	Validate the settings of a slider.

validateSliderState :: !SliderState -> SliderState
validateSliderState {sliderMin=sMin, sliderMax=sMax, sliderThumb=thumb}
	= {	sliderMin	= min`
	  ,	sliderMax	= max`
	  ,	sliderThumb	= SetBetween thumb min` max`
	  }
where
	min` = min sMin sMax
	max` = max sMin sMax


//	Collect all Ids of the given [WElementHandle].

getWElementControlIds :: ![WElementHandle .ls .pst] -> (![Id],![WElementHandle .ls .pst])
getWElementControlIds [itemH:itemHs]
	# (ids1,itemH)	= getWElementIds itemH
	# (ids2,itemHs)	= getWElementControlIds itemHs
	= (ids1++ids2,[itemH:itemHs])
where
	getWElementIds :: !(WElementHandle .ls .pst) -> (![Id],!WElementHandle .ls .pst)
	getWElementIds (WItemHandle itemH)
		# (ids,itemH)	= getWElementIds` itemH
		= (ids,WItemHandle itemH)
	where
		getWElementIds` :: !(WItemHandle .ls .pst) -> (![Id],!WItemHandle .ls .pst)
		getWElementIds` itemH=:{wItemId,wItems}
			# (ids,itemHs)		= getWElementControlIds wItems
			# itemH				= {itemH & wItems=itemHs}
			| isJust wItemId	= ([fromJust wItemId:ids],itemH)
			| otherwise			= (ids,itemH)
	
	getWElementIds (WListLSHandle itemHs)
		# (ids,itemHs)	= getWElementControlIds itemHs
		= (ids,WListLSHandle itemHs)
	
	getWElementIds (WExtendLSHandle wExH=:{wExtendItems=itemHs})
		# (ids,itemHs)	= getWElementControlIds itemHs
		= (ids,WExtendLSHandle {wExH & wExtendItems=itemHs})
	
	getWElementIds (WChangeLSHandle wChH=:{wChangeItems=itemHs})
		# (ids,itemHs)	= getWElementControlIds itemHs
		= (ids,WChangeLSHandle {wChH & wChangeItems=itemHs})
getWElementControlIds _
	= ([],[])


//	Collect all Ids of the given [WElementHandle`].

getWElementControlIds` :: ![WElementHandle`] -> [Id]
getWElementControlIds` [itemH:itemHs]
	= getWElementIds itemH ++ getWElementControlIds` itemHs
where
	getWElementIds :: !WElementHandle` -> [Id]
	getWElementIds (WItemHandle` itemH)
		= getWElementIds` itemH
	where
		getWElementIds` :: !WItemHandle` -> [Id]
		getWElementIds` itemH=:{wItemId`,wItems`}
			# ids				= getWElementControlIds` wItems`
			| isJust wItemId`	= [fromJust wItemId`:ids]
			| otherwise			= ids
	
	getWElementIds (WRecursiveHandle` itemHs _)
		= getWElementControlIds` itemHs
getWElementControlIds` _
	= []


//	Id occurrence checks on [WElementHandle .ls .pst] and [WElementHandle`].

//	There are no duplicate (ControlId id) attributes:

noDuplicateControlIds :: ![WElementHandle .ls .pst] -> (!Bool,![WElementHandle .ls .pst])
noDuplicateControlIds itemHs
	# (ids,itemHs)	= getWElementControlIds itemHs
	= (noDuplicates ids, itemHs)

noDuplicateControlIds` :: ![WElementHandle`] -> Bool
noDuplicateControlIds` itemHs
	= noDuplicates (getWElementControlIds` itemHs)


//	The list of Ids does not occur in any (ControlId id) attribute:

disjointControlIds :: ![Id] ![WElementHandle .ls .pst] -> (!Bool,![WElementHandle .ls .pst])
disjointControlIds ids itemHs
	# (ids`,itemHs)	= getWElementControlIds itemHs
	= (disjointLists ids ids`,itemHs)

disjointControlIds` :: ![Id] ![WElementHandle`] -> Bool
disjointControlIds` ids itemHs
	= disjointLists ids (getWElementControlIds` itemHs)
