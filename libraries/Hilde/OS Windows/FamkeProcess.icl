implementation module FamkeProcess

import FamkeKernel
import FamkeRpc, LoesListSeq, LoesKeyList, TcpIp
import StdBool, StdInt, StdMisc, StdString, StdList, StdArray, StdTuple, ArgEnv, Windows
from DynamicLinkerInterface import GetDynamicLinkerPath

TRACE msg x :== trace_n msg x; import StdDebug

:: ProtocolIn
	= ClientWorkRequest !ProcessId !Int
	| NewProcess Process
	| ReuseProcess !ProcessId Process
	| JoinProcess !ProcessId !ProcessId
	| KillProcess !ProcessId !ProcessId
	| Shutdown
	| ReservePort
	| FreePort !Int

:: ProtocolOut
	= ClientDoMoreWork Process
	| ClientNoMoreWork
	| ProcessCreated !ProcessId
	| ProcessReused
	| ProcessJoined
	| ProcessKilled
	| ShuttingDown
	| PortReserved !Int
	| PortFreed

newProcess :: !(*World -> *World) !*World -> (!ProcessId, !*World)
newProcess process famke
	# (ProcessCreated id, famke) = rpcProcessServer (NewProcess process) famke
	= (id, famke)

reuseProcess :: !ProcessId !(*World -> *World) !*World -> *World
reuseProcess id process famke
	# (reply, famke) = rpcProcessServer (ReuseProcess id process) famke
	= case reply of ProcessReused-> famke

joinProcess :: !ProcessId !*World -> *World
joinProcess id famke
	# (self, famke) = processId famke
	  (reply, famke) = rpcProcessServer (JoinProcess self id) famke
	= case reply of ProcessJoined -> famke

killProcess :: !ProcessId !*World -> *World
killProcess id famke
	# (self, famke) = processId famke
	  (reply, famke) = rpcProcessServer (KillProcess self id) famke
	= case reply of ProcessKilled -> famke

shutdown :: !*World -> *World
shutdown famke
	# (reply, famke) = rpcProcessServer Shutdown famke
	= eval famke
where
	eval :: !.a -> .a
	eval _ = abort "Shutdown failed"

reservePort :: !*World -> (!FamkePort .a .b, !*World)
reservePort famke
	# (_, ip, famke) = localhostIp famke
	  (reply, famke) = rpcProcessServer ReservePort famke
	= case reply of PortReserved p -> (FamkeServer {famkeIp = ip, famkePort = p}, famke)

freePort :: !(FamkePort .a .b) !*World -> *World
freePort (FamkeServer {famkePort}) famke
	# (reply, famke) = rpcProcessServer (FreePort famkePort) famke
	= case reply of PortFreed -> famke

rpcProcessServer :: !ProtocolIn !*World -> (!ProtocolOut, !*World)
rpcProcessServer request famke = rpc FamkeProcessServer request famke

:: Process :== *World -> *World

:: *State =
	{	working		:: !.KeyList ProcessId .Working
	,	waiting		:: !.[#.Waiting!]
	,	nextid		:: !Int
	,	executable	:: !String
	,	server		:: !.RpcServer ProtocolIn ProtocolOut
	,	famke		:: !.World
	,	ports		:: !.[#Int]
	}

:: WorkingState = NotConnected | Connected !Int | Disconnect

:: Working = 
	{	workingWork		:: !.[Process!]
	,	workingJoin		:: !.[#.Join!]
	,	workingId		:: !ProcessId
	,	workingState	:: !WorkingState
	}

:: Join =
	{	joinId		:: !ProcessId
	,	joinReply	:: !.(ProtocolOut -> *(*World -> *World))
	}

:: Waiting =
	{	waitingReply	:: !.(ProtocolOut -> *(*World -> *World))
	,	waitingWorking	:: !.Working
	}

startProcessServer :: !String !(*World -> *World) !*World -> *World
startProcessServer executable process famke
	# (_, server, famke) = rpcOpen FamkeProcessServer (TRACE "World Process Server" famke)
	  st = {working = Empty, waiting = Empty, nextid = 1, executable = executable, server = server, famke = famke, ports = [#0xFA01..0xFAFF]}
	  {server, famke} = newProcess` process (\_ famke -> famke) st
	= rpcClose server famke
where
	processServer st=:{working, waiting, server, famke}
	  	# (done, working) = uIsEmpty working
		| done  
			# famke = Fold (endWaitingClient 0) famke waiting
			= TRACE ("All clients idle, shutting down") {st & working = working, waiting = Empty, famke = famke}
		# (request, reply, server, famke) = rpcWait server famke
		  st = {st & working = working, server = server, famke = famke}
		= case request of
			ClientWorkRequest self osId -> clientWorkRequest self osId reply st
			NewProcess process -> newProcess` process reply st
			ReuseProcess id process -> renewProcess` id process reply st
			JoinProcess self id -> joinProcess` self id reply st
			KillProcess self id -> killProcess` self id reply st
			Shutdown -> shutdown` reply st
			ReservePort -> reservePort` reply st
			FreePort p -> freePort` p reply st

	clientWorkRequest self osId reply st=:{working, waiting, famke}
		# (Just w=:{workingWork, workingJoin, workingState}, working) = uExtractK self working
		  famke = Foldr (joinClient self) famke workingJoin
		= case workingState of
			Disconnect -> processServer {st & working = working, famke = endWaitingClient 0 {waitingReply = reply, waitingWorking=w} famke}
			_
				# (maybe, workingWork) = uDeCons workingWork
				  w = {w & workingWork = workingWork, workingJoin = Empty, workingState = Connected osId}
				-> case maybe of
					Just process
						# famke = reply (ClientDoMoreWork process) famke
						  working = uInsertK self w working
						-> TRACE ("Process client " +++ toString self +++ " put to work") (processServer {st & working = working, famke = famke})
					_
				  		# waiting = uSnoc waiting {waitingReply = reply, waitingWorking = w}
				  		-> TRACE ("Process client " +++ toString self +++ " idle") (processServer {st & working = working, waiting = waiting, famke = famke})
		
	newProcess` process reply st=:{working, waiting, nextid} 
		# (maybe, waiting) = uDeCons waiting
		  (id, st=:{famke}) = case maybe of
								Just client -> reuseClient client process {st & waiting = waiting}
								_ -> (nextid, launchClient nextid process {st & waiting = waiting, nextid = nextid + 1})
		  famke = reply (ProcessCreated id) famke
		= processServer {st & famke = famke}
	
	renewProcess` id process reply st=:{working, waiting} 
		# (maybe, working) = uExtractK id working
		  st=:{famke} = case maybe of
							Just w=:{workingWork}
								# working = uInsertK id {w & workingWork = uSnoc workingWork process} working
								-> {st & working = working}
							_
								# (maybe, waiting) = extractWaiting id waiting
								  st = {st & working = working, waiting = waiting}
								-> case maybe of
									Just client -> snd (reuseClient client process st)
									_ -> launchClient id process st
		  famke = reply ProcessReused famke
		= processServer {st & famke = famke}
	
	joinProcess` self id reply st=:{working, famke}
		# (maybe, working) = uExtractK id working
		= case maybe of
			Just w=:{workingJoin} | self <> id
				# working = uInsertK id {w & workingJoin = uCons {joinId = self, joinReply = reply} workingJoin} working
				-> TRACE ("Process client " +++ toString self +++ " waiting to join " +++ toString id) (processServer {st & working = working})
			_
				# famke = reply ProcessJoined famke
				-> TRACE ("Process client " +++ toString self +++ " joined " +++ toString id) (processServer {st & working = working, famke = famke})
	
	killProcess` self id reply st=:{working}
		# (maybe, working) = uExtractK id working
		  st=:{waiting, famke} = TRACE ("Process client " +++ toString self +++ " wants " +++ toString id +++ " killed") {st & working = working}
		= case maybe of
			Just client
				# st=:{famke} = endWorkingClient self client st
				  famke = if (id <> self) (reply ProcessKilled famke) famke
				-> processServer {st & famke = famke}
			_
				# (Just client, waiting) = extractWaiting id waiting
				  famke = endWaitingClient self client famke
				  famke = reply ProcessKilled famke
				-> processServer {st & waiting = waiting, famke = famke}
	
	shutdown` reply st=:{working, waiting, famke}
		# famke = Fold (endWaitingClient 0) famke waiting
		  st = Fold (endWorkingClient 0) {st & working = Empty, waiting = Empty, famke = famke} working
		= TRACE "Process server starts killing all clients" (processServer st)

	launchClient id process st=:{executable, working, famke}
		# (ok, famke) = launchExecutable executable [toString id] famke
		| not ok = abort "launchExecutable failed"
		# working = uInsertK id {workingWork = uCons process Empty, workingJoin = Empty, workingId = id, workingState = NotConnected} working
		= TRACE ("Process client " +++ toString id +++ " launched") {st & working = working, famke = famke}
	
	reuseClient {waitingReply, waitingWorking=waitingWorking=:{workingId}} process st=:{working, famke}
		# famke = waitingReply (ClientDoMoreWork process) famke
		  working = uInsertK workingId waitingWorking working
		= TRACE ("Process client " +++ toString workingId +++ " reused") (workingId, {st & working = working, famke = famke})
	
	joinClient self {joinId, joinReply} famke = joinReply ProcessJoined (TRACE ("Process client " +++ toString joinId +++ " joined " +++ toString self) famke)
	
	endWaitingClient self {waitingReply, waitingWorking={workingId}} famke
		# famke = waitingReply ClientNoMoreWork famke
		= TRACE ("Process client " +++ toString self +++ " ended (idle) " +++ toString workingId) famke

	endWorkingClient self w=:{workingJoin, workingId, workingState} st=:{working, famke}
		= case workingState of
			Connected osId
				# (ok, handle, famke) = OpenProcess PROCESS_TERMINATE False osId famke
				| not ok -> abort "OpenProcess failed"
				# (ok, famke) = TerminateProcess handle 0 famke
				| not ok -> abort "TerminateProcess failed"
				# famke = if (self <> 0) (Foldr (joinClient self) famke workingJoin) famke
				-> TRACE ("Process client " +++ toString self +++ " ended (killed) " +++ toString workingId) {st & famke = famke}
			_ -> {st & working = uInsertK workingId {w & workingState = Disconnect} working}

	extractWaiting id xs 
		# (maybe, xs) = uDeCons xs
		= case maybe of
			Just x=:{waitingWorking={workingId}}
				| workingId == id -> (Just x, xs)
				# (maybe, xs) = extractWaiting id xs
				-> (maybe, uCons x xs)
			_ -> (Nothing, xs)

	reservePort` reply st=:{ports=[|p:ports], famke}
		# famke = reply (PortReserved p) famke
		  st = {st & ports = ports, famke = famke}
		= TRACE ("Process reserved port " +++ toString p) (processServer st)

	freePort` p reply st=:{ports, famke}
		# famke = reply PortFreed famke
		  st = {st & ports = [|p:ports], famke = famke}
		= TRACE ("Process freed port " +++ toString p) (processServer st)

startProcessClient :: !*World -> *World
startProcessClient famke
	# (id, famke) = processId famke
	  (osId, famke) = GetCurrentProcessId famke
	= processClient id osId famke
where
	processClient id osId famke
		# (reply, famke) = rpcProcessServer (ClientWorkRequest id osId) famke
		= case reply of
			ClientDoMoreWork f 
				#!famke = TRACE ("World Process Client " +++ toString id +++ " working") famke
				  famke = f famke
			  	  famke = TRACE ("World Process Client " +++ toString id +++ " done") famke
			  	-> processClient id osId famke
			ClientNoMoreWork -> famke

StartProcess :: !(*World -> *World) !*World -> *World
StartProcess f world 
	# (_, executable, args, world) = commandLine world
	= case args of
		[id] -> StartKernel (toInt id) startProcessClient world
		[] -> StartKernel 0 (startProcessServer executable f) world

commandLine :: !*World -> (!String, !String, ![String], !*World)
commandLine world
	# [arg0:args] = [x \\ x <-: getCommandLine]
	  (executable, world) = if (arg0 <> GetDynamicLinkerPath +++ "\\utilities\\ConsoleClient.exe")
								 (arg0, world)
								let (_, batch, world`) = GetConsoleTitle world in (batch, world`)
	= (toString (reverse (getPath (reverse (fromString executable)))), executable, args, world)
where
	getPath ['\\':xs] = xs
	getPath [x:xs] = [x:getPath xs]

launchExecutable :: !String ![String] !*World -> (!Bool, !*World)
launchExecutable program args famke 
	# (ok, info, famke) = CreateProcess (foldl concat ("\"" +++ program +++ "\"") args) False 0 famke
	| not ok = (False, famke)
	# (ok1, famke) = CloseHandle info.hThread famke
	  (ok2, famke) = CloseHandle info.hProcess famke
	= (ok1 && ok2, famke)
where
	concat x y = x +++ " \"" +++ y +++ "\""
