implementation module ESMVizTool

import StdEnv, StdListExtensions
import StdiData
import StdiTasks //iTasks, iTaskDB
import ESMSpec
import iDataGraphvizForm
import GenPrint

derive gForm  KnownAutomaton, Maybe, []
derive gUpd   KnownAutomaton, Maybe, []
derive gPrint KnownAutomaton, Maybe
derive gParse KnownAutomaton, Maybe

//esmVizTool :: !(ESM s i o) *HSt -> (Bool,Html,*HSt) | iData, gEq{|*|}, render s & iData, gEq{|*|}, render, ggen{|*|} i & iData, gEq{|*|}, render o
esmVizTool :: !(ESM s i o) *World -> *World | iData, gEq{|*|}, render s & iData, gEq{|*|}, render, ggen{|*|} i & iData, gEq{|*|}, render o
esmVizTool esm world
	 = singleUserTask [] (iterateTask DiGraphFlow (newKA,[esm.s_0],[],1)) world
where
	DiGraphFlow	st=:(ka,as,trace,n)
	 =	orTaskL	[state
				,chooseTaskV [] (sortBy (\(a,_) (b,_).a<b) [(render i,step i) \\ i<-possibleInputs esm as])
				,[Br] !>> chooseTask [] [("Back" ,back),("Prune",prune),("Reset",return_V (newKA,[esm.s_0],[],1)),("Clear trace",return_V (ka,as,[],n))]
	    		,stepN <<! traceHtml trace <<! legend
	    		]
	where
		state 	=	issuesToHtml ka.issues
					!>>	editTask "OK" (mkDigraph ThisExe (ka,esm.s_0,as,allEdgesFound esm ka,map fst ka.issues,flatten trace))
					=>> \dig -> let (as`,trace`) = findSelectedStates dig ka as trace
							  	in return_V (ka,as`,trace`,n)
		step i 	= let next   = nextStates esm i as
					  ka`    = addTransitions 1 esm as [i] ka
					  trace` = addStep esm as i trace
				  in return_V (ka`,next,trace`,n)
		back | isEmpty trace
				= return_V (ka,as,[],n)
				= let next   = startStates (last trace)
					  trace` = init trace
				  in return_V (ka,next,trace`,n)
		stepN 	= editTask "Add All" n
				  =>> \n -> if (n>0)
				  		(return_V (addTransitions n esm as (possibleInputs esm as) ka,as,trace,n)) 
				  		(return_V (ka,as,trace,1))
		prune	= return_V ({trans=[t\\t<-ka.trans|gisMember t onTraces],issues=[i\\i=:(t,_)<-ka.issues|gisMember t onTraces]},as,trace,n) where onTraces = flatten trace

newKA = {trans=[],issues=[]}

iterateTask :: (a->Task a) a -> Task a | iData a
iterateTask task a = task a =>> iterateTask task

orTaskL :: [Task a] -> Task a | iData a
orTaskL l = foldl1 (-||-) l

foldl1 op [a]   = a
foldl1 op [a:x] = op a (foldl1 op x)

traceHtml trace
   = [H3 [] "Trace:"
     ,Table []
       [Tr [] [Td [] (map Txt (flatten [transToStrings t [] \\ t <- step]))]
           \\ step <- trace
       ]
     , Br
     ]

legend
 =	[ H3 [] "Legend:"
	, Txt "Double circled state: intial state."
	, Br
	, Txt "Red state: current state (change state by click + OK-button)."
	, Br
	, Txt "Blue state: all defined inputs have been applied."
	, Br
	, Txt "Blue transitions: part of the current trace."
	, Br
	, Txt "Red transitions: an issue was found on this trace. Arrow heads are slightly bigger on trace."
	]

issuesToHtml :: [(SeenTrans s i o,[String])] -> HtmlCode | render s & render i & render o
issuesToHtml [] = []
issuesToHtml l
	=	[H3 [] "Issues found:"
		:	[ Table []
				[ Tr [] [Td [] (map Txt (transToStrings t [": ":ss])) ]
				\\ (t,ss) <- l
				]
			]
		]

transToStrings :: (SeenTrans s i o) [String] -> [String] | render s & render i & render o
transToStrings (s,i,o,t) c = ["(",render s,",",render i,",[":showList "," o ["],",render t,")":c]]

showList :: !String ![a] [String] -> [String] | render a
showList delimit []    c = c
showList delimit [x]   c = [render x:c]
showList delimit [x:r] c = [render x,delimit:showList delimit r c]

findSelectedStates :: Digraph (KnownAutomaton s i o) [s] (Traces s i o) -> ([s],Traces s i o) | render, gEq{|*|} s
findSelectedStates (Digraph _ _ _ Nothing) _ as t = (as,t)
findSelectedStates (Digraph _ _ nodes (Just (Node nr))) ka as trace
	# as` = take 1
			[  s
			\\ NodeDef nr` _ atts _ <- nodes | nr`==nr
			,  (NAtt_label label) <- atts
			,  s <- nodesOf ka | render s == label
			]
	= case as` of
		[]	 = (as,trace)
		[ns] | gisMember ns as
				= (as`,narrowTraces trace as`)
			 # oneStep = [tr \\ tr=:(s,i,o,t)<-ka.trans | t===ns && gisMember s as]
			 | not (isEmpty oneStep)
			 = (as`,trace++[oneStep])
			 = (as`,partTraces trace ns [])
			 = (as`,[]) // ??

partTraces :: (Traces s i o) s (Traces s i o) -> (Traces s i o) | gEq{|*|} s
partTraces [] s seen = []
partTraces [trans:rest] s seen
	| gisMember s (targetStates trans)
		= narrowTraces (reverse [trans:seen]) [s]
		= partTraces rest s [trans:seen]

allEdgesFound :: (ESM s i o) (KnownAutomaton s i o) -> [s] | gEq{|*|} s & ggen{|*|} i
allEdgesFound esm automaton
	= [s \\ s <- nodesOf automaton
	      | length (edgesFrom s automaton) == length [t \\ i<-enumerate,t<-nextStates esm i [s]] 
	  ]

toHtmlString :: a -> String | gPrint{|*|} a
toHtmlString x
	# string = printToString x
	= {checkChar c \\ c <-: string}
where
	checkChar '"' = '\''
	checkChar  c  = c
