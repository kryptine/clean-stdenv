implementation module controlvalidate


//	Clean Object I/O library, version 1.2


import	StdBool, StdInt, StdList
import	ospicture, oswindow
import	commondef, windowhandle, wstate
from	windowaccess import getWItemReceiverInfo


controlvalidateFatalError :: String String -> .x
controlvalidateFatalError function error
	= FatalError function "controlvalidate" error


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


/*	Bind all free R(2)Ids that are contained in the WElementHandles.
	It assumes that it has already been checked that no R(2)Id is already bound in the ReceiverTable.
*/
bindReceiverControlIds :: !SystemId !Id ![WElementHandle .ls .pst] !*ReceiverTable -> (![WElementHandle .ls .pst],!*ReceiverTable)
bindReceiverControlIds ioId wId [itemH:itemHs] rt
	# (itemH, rt) = bindReceiverControlIds` ioId wId itemH  rt
	# (itemHs,rt) = bindReceiverControlIds  ioId wId itemHs rt
	= ([itemH:itemHs],rt)
where
	bindReceiverControlIds` :: !SystemId !Id !(WElementHandle .ls .pst) !*ReceiverTable
										  -> (!WElementHandle .ls .pst, !*ReceiverTable)
	bindReceiverControlIds` ioId wId (WItemHandle itemH=:{wItemKind,wItemInfo,wItems,wItemSelect}) rt
		| not (isReceiverControl wItemKind)
			# (itemHs,rt1)	= bindReceiverControlIds ioId wId wItems rt
			  itemH1		= {itemH & wItems=itemHs}
			= (WItemHandle itemH1,rt1)
		| otherwise
			# recLoc		= {rlIOId=ioId,rlDevice=WindowDevice,rlParentId=wId,rlReceiverId=id}
			# rte			= {rteLoc=recLoc,rteSelectState=if wItemSelect Able Unable,rteASMCount=0}
			# (_,rt)		= addReceiverToReceiverTable rte rt
			= (WItemHandle itemH,rt)
	where
		rH					= getWItemReceiverInfo wItemInfo
		id					= rH.rId
		
		isReceiverControl :: !ControlKind -> Bool
		isReceiverControl (IsOtherControl type)	= type=="Receiver" || type=="Receiver2"
		isReceiverControl _						= False
	
	bindReceiverControlIds` ioId wId (WListLSHandle itemHs) rt
		# (itemHs,rt)	= bindReceiverControlIds ioId wId itemHs rt
		= (WListLSHandle itemHs,rt)
	
	bindReceiverControlIds` ioId wId (WExtendLSHandle wExH=:{wExtendItems}) rt
		# (itemHs,rt)	= bindReceiverControlIds ioId wId wExtendItems rt
		= (WExtendLSHandle {wExH & wExtendItems=itemHs},rt)
	
	bindReceiverControlIds` ioId wId (WChangeLSHandle wChH=:{wChangeItems}) rt
		# (itemHs,rt)	= bindReceiverControlIds ioId wId wChangeItems rt
		= (WChangeLSHandle {wChH & wChangeItems=itemHs},rt)

bindReceiverControlIds _ _ [] rt
	= ([],rt)


/*	controlIdsAreConsistent checks whether the WElementHandles contain (R(2))Ids that have already been
	associated with open receivers or other I/O objects and if there are no duplicate Ids. 
	The ReceiverTable is not changed if there are duplicate (R(2))Ids; otherwise all (R(2))Ids have been bound.
*/
controlIdsAreConsistent :: !SystemId !Id ![WElementHandle .ls .pst] !*ReceiverTable !*IdTable
							   -> (!Bool,![WElementHandle .ls .pst],!*ReceiverTable,!*IdTable)
controlIdsAreConsistent ioId wId itemHs rt it
	# (ids,itemHs)	= getWElementControlIds itemHs
	| not (okMembersIdTable ids it)
		= (False,itemHs,rt,it)
	# idParent		= {idpIOId=ioId,idpDevice=WindowDevice,idpId=wId}
	  (ok,it)		= addIdsToIdTable [(id,idParent) \\ id<-ids] it
	  (itemHs,rt)	= bindReceiverControlIds ioId wId itemHs rt
	| not ok
		= controlvalidateFatalError "controlIdsAreConsistent" "could not add all Ids to IdTable"
	| otherwise
		= (True,itemHs,rt,it)
