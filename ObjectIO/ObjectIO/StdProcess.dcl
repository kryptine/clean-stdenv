definition module StdProcess


//	********************************************************************************
//	Clean Standard Object I/O library, version 1.2
//	
//	StdProcess contains the process creation and manipulation functions.
//	********************************************************************************


import	StdProcessDef


/*	General process topology creation functions:
*/

class Processes pdef where
	startProcesses			:: !pdef      !*World      -> *World
	openProcesses			:: !pdef      !(PSt .l .p) -> PSt .l .p

class shareProcesses pdef	:: !(pdef .p) !(PSt .l .p) -> PSt .l .p

/*	(start/open/share)Processes creates an interactive process topology specified by
		the pdef argument. 
		All interactive processes can communicate with each other by means of  the 
		file system or by message passing. 
		By default, processes obtain a private ProcessWindow. However, if a process 
		has the ProcessShareGUI attribute, then the process will share the 
		ProcessWindow of the current interactive process. Every interactive process 
		can create processes in this way. This results in a tree of processes (see 
		also the notes of termination at closeProcess).
	startProcesses aborts the application if the argument world does not contain a 
		file system. 
		startProcesses terminates as soon as all interactive processes that are 
		created by startProcesses and their child processes have terminated. It 
		returns the final world, consisting of the final file system and event 
		stream. 
	shareProcesses adds the interactive processes specified by the pdef argument to 
		the process group of the current interactive process. The new interactive 
		processes can communicate with all interactive processes of the current 
		process group by means of the public process state component. 
*/

instance Processes		(ProcessGroup pdef)		| shareProcesses pdef
instance Processes		[pdef]					| Processes		 pdef
instance Processes		(:^: pdef1 pdef2)		| Processes		 pdef1
												& Processes		 pdef2

instance shareProcesses	Process
instance shareProcesses	(ListCS    pdef )		| shareProcesses pdef
instance shareProcesses (:~: pdef1 pdef2)		| shareProcesses pdef1
												& shareProcesses pdef2


//	Convenience process creation functions:

startIO :: !DocumentInterface !.l !.p !(ProcessInit      (PSt .l .p))
									  ![ProcessAttribute (PSt .l .p)]
			!*World -> *World
/*	startIO creates one process group of one interactive process. 
*/


//	Process access operations:

closeProcess	:: !(PSt .l .p) -> PSt .l .p
/*	closeProcess removes all abstract devices that are held in the interactive 
	process.
	If the interactive process has processes that share its GUI then these will also
	be closed recursively. As a result evaluation of this interactive process 
	including GUI sharing processes will terminate.
*/


hideProcess		:: !(PSt .l .p) -> PSt .l .p
showProcess		:: !(PSt .l .p) -> PSt .l .p
/*	If the interactive process is active, hideProcess hides the interactive process, 
	and showProcess makes it visible. Note that hiding an interactive process does 
	NOT disable the process but simply makes it invisible.
*/

getProcessWindowPos	:: !(IOSt .l .p) -> (!Point2,!IOSt .l .p)
/*	getProcessWindowPos returns the current position of the ProcessWindow.
*/


getProcessWindowSize:: !(IOSt .l .p) -> (!Size,!IOSt .l .p)
/*	getProcessWindowSize returns the current size of the ProcessWindow.
*/
