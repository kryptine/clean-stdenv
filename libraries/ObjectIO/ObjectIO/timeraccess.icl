implementation module timeraccess


//	Clean Object I/O library, version 1.2


import	StdBool, StdTuple
import	devicesystemstate, timerhandle, timertable
from	commondef	import FatalError, StateMap2


timeraccessFatalError :: String String -> .x
timeraccessFatalError function error = FatalError function "timeraccess" error


/*	bindTimerElementIds binds all unbound R(2)Ids and Ids that can be located in the list of TimerElementStates.
	The Boolean result is True only if no bound identification was found, otherwise it is False.
*/
bindTimerElementIds :: !SystemId !Id ![TimerElementHandle .ls .ps] !ReceiverTable !IdTable
						   -> (!Bool,![TimerElementHandle .ls .ps],!ReceiverTable,!IdTable)
bindTimerElementIds pid timerid [itemH:itemHs] rt idtable
	# (ok,itemH,rt,idtable)		= bindTimerElementIds` pid timerid itemH rt idtable
	| not ok
		= (ok,[itemH:itemHs],rt,idtable)
	| otherwise
		# (ok,itemHs,rt,idtable)= bindTimerElementIds pid timerid itemHs rt idtable
		= (ok,[itemH:itemHs],rt,idtable)
where
	bindTimerElementIds` :: !SystemId !Id !(TimerElementHandle .ls .ps) !ReceiverTable !IdTable
								-> (!Bool, !TimerElementHandle .ls .ps, !ReceiverTable,!IdTable)
	bindTimerElementIds` pid timerid (TimerReceiverHandle itemH=:{tReceiverHandle=trH}) rt idtable
		| memberIdTable rid idtable
			= (False,TimerReceiverHandle itemH,rt,idtable)
		# opt_rte			= getReceiverTableEntry rid rt
		| isJust opt_rte	// This situation should not occur: the IdTable didn't contain the id while the ReceiverTable does.
			= timeraccessFatalError "bindTimerElementIds" "inconsistency detected between IdTable and ReceiverTable"
			//(False,TimerReceiverHandle itemH,rt,idtable)
		| otherwise
			# rl			= {rlIOId=pid,rlDevice=TimerDevice,rlParentId=timerid,rlReceiverId=rid}
			  rte			= {rteLoc=rl,rteSelectState=trH.rSelect,rteASMCount=0}
			  (_,rt)		= addReceiverToReceiverTable rte rt
			  (_,idtable)	= addIdToIdTable rid {idpIOId=pid,idpDevice=TimerDevice,idpId=timerid} idtable
			= (True,TimerReceiverHandle itemH,rt,idtable)
	where
		rid					= trH.rId
	bindTimerElementIds` pid timerid (TimerListLSHandle itemHs) rt idtable
		# (ok,itemHs,rt,idtable) = bindTimerElementIds pid timerid itemHs rt idtable
		= (ok,TimerListLSHandle itemHs,rt,idtable)
	bindTimerElementIds` pid timerid (TimerElimLSHandle itemHs) rt idtable
		# (ok,itemHs,rt,idtable) = bindTimerElementIds pid timerid itemHs rt idtable
		= (ok,TimerElimLSHandle itemHs,rt,idtable)
	bindTimerElementIds` pid timerid (TimerIntroLSHandle tInH=:{tIntroItems}) rt idtable
		# (ok,itemHs,rt,idtable) = bindTimerElementIds pid timerid tIntroItems rt idtable
		= (ok,TimerIntroLSHandle {tInH & tIntroItems=itemHs},rt,idtable)
	bindTimerElementIds` pid timerid (TimerExtendLSHandle tExH=:{tExtendItems}) rt idtable
		# (ok,itemHs,rt,idtable) = bindTimerElementIds pid timerid tExtendItems rt idtable
		= (ok,TimerExtendLSHandle {tExH & tExtendItems=itemHs},rt,idtable)
	bindTimerElementIds` pid timerid (TimerChangeLSHandle tChH=:{tChangeItems}) rt idtable
		# (ok,itemHs,rt,idtable) = bindTimerElementIds pid timerid tChangeItems rt idtable
		= (ok,TimerChangeLSHandle {tChH & tChangeItems=itemHs},rt,idtable)
bindTimerElementIds _ _ itemHs rt idtable
	= (True,itemHs,rt,idtable)


/*	unbindTimerElementIds unbinds all bound R(2)Ids and Ids that can be located in the list of TimerElementStates.
*/
unbindTimerElementIds :: !SystemId ![TimerElementHandle .ls .ps] !(!TimerTable,!ReceiverTable,!IdTable)
															   -> (!TimerTable,!ReceiverTable,!IdTable)
unbindTimerElementIds pid itemHs tables
	= StateMap2 unbindTimerElementIds` itemHs tables
where
	unbindTimerElementIds` :: !(TimerElementHandle .ls .ps) !(!TimerTable,!ReceiverTable,!IdTable)
														  -> (!TimerTable,!ReceiverTable,!IdTable)
	unbindTimerElementIds` (TimerReceiverHandle {tReceiverHandle={rId}}) (tt,rt,idtable)
		= (snd (removeTimerFromTimerTable teLoc tt),snd (removeReceiverFromReceiverTable rId rt),snd (removeIdFromIdTable rId idtable))
	where
		teLoc	= {tlIOId=pid,tlDevice=TimerDevice,tlParentId=rId,tlTimerId=rId}
	unbindTimerElementIds` (TimerListLSHandle tListItems) tables
		= StateMap2 unbindTimerElementIds` tListItems tables
	unbindTimerElementIds` (TimerElimLSHandle tElimItems) tables
		= StateMap2 unbindTimerElementIds` tElimItems tables
	unbindTimerElementIds` (TimerIntroLSHandle {tIntroItems}) tables
		= StateMap2 unbindTimerElementIds` tIntroItems tables
	unbindTimerElementIds` (TimerExtendLSHandle {tExtendItems}) tables
		= StateMap2 unbindTimerElementIds` tExtendItems tables
	unbindTimerElementIds` (TimerChangeLSHandle {tChangeItems}) tables
		= StateMap2 unbindTimerElementIds` tChangeItems tables

identifyTimerStateHandle :: !Id !(TimerStateHandle .ps) -> (!Bool,!TimerStateHandle .ps)
identifyTimerStateHandle id tlsH=:(TimerLSHandle {tHandle={tId}})
	= (id==tId,tlsH)
