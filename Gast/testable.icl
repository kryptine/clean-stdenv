implementation module testable

/*
Pieter Koopman 2002
Nijmegen University, The Netherlands

GAST: A Generic Automatic Software Test-system

Known problems:
- evaluating is too strict, the system does not show the arguments before using them!

*/

import StdEnv, MersenneTwister, genLibTest, StdTime

:: Trace
	= Pair [(Trace,Trace)] [(Trace,Trace)] | Either Bool Trace Trace | Done | Empty
	| ListNil | ListCons Trace Trace | Unit
	| Int [Int] | Char [Char] | String [String] | Real Bool
emptyTrace :: Trace
emptyTrace
	=	Empty

derive gLess Result
instance == Result where (==) x y = x===y

newAdmin :: Admin
newAdmin = {res=Undef, labels=[], args=[], name=[]}

class TestArg a | genShow{|*|}, generate{|*|} a

class Testable a where evaluate :: a RandomStream Admin -> [Admin]

instance Testable Bool where evaluate b rs result = [{result & res = if b OK CE, args = reverse result.args}]

instance Testable Property
where evaluate (Prop p) rs result = p rs result

instance Testable (a->b) | Testable b & TestArg a  
where evaluate f rs result
		# (rs,rs2) = split rs
		= forAll f (generateAll rs) rs2 result

:: Property = Prop (RandomStream Admin -> [Admin])

prop :: a -> Property | Testable a
prop p = Prop (evaluate p)

forAll :: !(a->b) ![a] RandomStream Admin -> [Admin] | Testable b & TestArg a
forAll f list rs r = diagonal [apply f a (genRandInt seed) r \\ a<-list & seed<-rs ]

apply :: !(a->b) a RandomStream Admin -> [Admin] | Testable b & TestArg a
apply f a rs r = evaluate (f a) rs {r & args = [show1 a:r.args]}

diagonal :: [[a]] -> [a]
diagonal list = f 1 2 list []
where
	f n m [] [] = []
	f 0 m xs ys = f m (m+1) (rev ys xs) []
	f n m [] ys = f m (m+1) (rev ys []) []
	f n m [[x:r]:xs] ys = [x: f (n-1) m xs [r:ys]]
	f n m [[]:xs] ys = f (n-1) m xs ys
	
	rev []    accu = accu
	rev [x:r] accu = rev r [x:accu]

// showing arguments makes apply strict, if every from config is strict(!).

//instance + String where (+) s t = s +++ t

Left  :== True
Right :== False

instance toString Trace
where
	toString trace
	 = case trace of
		Pair [] n		 = "(Pair []"+toString (length n)+")"
		Pair [(x,y):r] n = "(Pair [("+toString x+" "+toString y+"):"+toString (length r)+"] "+toString (length n)+")"
		ListNil			 = "[]"
		ListCons h t	 = "["+toString h+":"+toString t+"]"
		Done			 = "Done"
		Empty			 = "Empty"
		Unit			 = "Unit"
		Int list		 = "Int "+toString list
		Char list		 = "Char "+toString list
		Either b t r
					| b	 = "(Either Left: "+toString t+" "+toString r+")"
						 = "(Either Right: "+toString t+" "+toString r+")"
		String l		 = "String"+show1 l
						 = "toString: Unknown Trace"

nextTrace :: Trace RandomStream -> (Trace, RandomStream)
nextTrace trace rnd
 = case trace of
	Done	= (Done, rnd)
	Unit	= (Done, rnd)
	Pair [(tx,ty):rest] next
		# (ty, rnd)	= nextTrace ty rnd
		  next		= case ty of
						Done = next
						ty   = [(tx,ty):next]
		  (tx, rnd)	= nextTrace tx rnd
		  next		= case rest of
						[]	= case tx of
								Done = next
								tx   = [(tx,Empty):next]
							= next
		= (case rest of
			[]	= case next of
					[]	= Done
						= Pair (reverse next) []
				= Pair rest next
		  ,rnd)
	Either _ tl tr
		# (b, rnd)			= genElem rnd
		| b # (tl`, rnd)	= nextTrace tl rnd
			= case tl` of
				Done	# (tr`, rnd) = nextTrace tr rnd
						= case tr` of
							Done = (Done, rnd)
							_	 = (Either Right tl tr`, rnd)
						= (Either Left tl` tr, rnd)
		| otherwise
			# (tr`, rnd)	= nextTrace tr rnd
			= case tr` of
				Done	# (tl`, rnd) = nextTrace tl rnd
						= case tl` of
							Done = (Done, rnd)
							_	 = (Either Left tl` tr, rnd)
						= (Either Right tl tr`, rnd)
	Int l	= case l of
				[]		= (Int l, rnd)
				[h:t]	= (Int t, rnd)
	Char l	= case l of
				[]		= (Char l, rnd)
				[h:t]	= (Char t, rnd)
	String l = case l of
				[]		= (String l, rnd)
				[h:t]	= (String t, rnd)
	Real s	 = (Real (not s), rnd)
	Empty	 = (Empty, rnd)

generic generate a :: Trace RandomStream -> (a, Trace, a->Int, RandomStream)

K x y = x

generate{|UNIT|} _ rnd = (UNIT, Unit, K 0, rnd)

generate{|Int|} t rnd = genInts t rnd
where 
	genInts t rnd
		= case t of
			Empty	= genInts (Int predInts) rnd
			Int l	= case l of
							[]		# (i,rnd) = genElem rnd
									= (i,t,id,rnd)
							[hd:tl]	= (hd,t,id,rnd)

generate{|Char|} t rnd = gen t rnd
where gen t rnd
		= case t of
			Empty	= gen (Char predChars) rnd
			Char l	= case l of
						[]		# (c,rnd) = genElem rnd
								= (c, t, \a = toInt a, rnd)
						[hd:tl]	= (hd,t,\a = toInt a,rnd)

generate{|Bool|} Empty rnd = (True,Either Right Done Empty,\a = if a 1 0,rnd)
generate{|Bool|} trace rnd = (False,Done,\a = if a 1 0,rnd)

generate{|Real|} t=:(Real small) rnd
	# (r,rnd) = genElem rnd
	= (if (small&&r<>0.0) (1.0/r) r, t, \a = toInt a, rnd)
generate{|Real|} t rnd
	= (0.0, Real True, \a = toInt a, rnd)

generate{|String|} t rnd = gen t rnd
where gen t rnd
		= case t of
			Empty	= gen (String predStrings) rnd
			String l= case l of
							[]		# (s,rnd) = genElem rnd
									= (s,t,\a = size a,rnd)
							[hd:tl]	= (hd,t,\a = size a,rnd)

generate{|PAIR|} f g Empty rnd
	# (x,tx,v1,rnd) = f Empty rnd
	  (y,ty,v2,rnd) = g Empty rnd
	= (PAIR x y, Pair [(tx,ty)] [], \(PAIR x y) = v1 x+v2 y, rnd)
generate{|PAIR|} f g (Pair [(tx,ty):rest] next) rnd
	# (x,tx,v1,rnd) = f tx rnd
	  (y,ty,v2,rnd) = g ty rnd
	= (PAIR x y, Pair [(tx,ty):rest] next, \(PAIR x y) = v1 x+v2 y, rnd)
generate{|PAIR|} f g (Pair [] done) rnd
	= abort "Invalid trace for generate{|PAIR|}: Pair [] ..."

generate{|EITHER|} fl fr t rnd
	# (rnd2,rnd)		= split rnd
	  (rnd3,rnd)		= split rnd
	= case t of
		Empty
			# (l,tl,gtil,_)		= fl Empty rnd2
			  (r,tr,gtir,_)		= fr Empty rnd3
			  (f,rnd)			= genElem rnd
			  gti				= \a = case a of
									LEFT  al = 2*gtil al
									RIGHT ar = 1+2*gtir ar
			| f	= (LEFT  l, Either Left  tl Empty, gti, rnd)
				= (RIGHT r, Either Right Empty tr, gti, rnd)
		Either left tl tr
			# (l,tl2,gtil,_)	= fl tl rnd2
			  (r,tr2,gtir,_)	= fr tr rnd3
			  gti				= \a = case a of
									LEFT  al = 2*gtil al
									RIGHT ar = 1+2*gtir ar
			| left
				= (LEFT  l, Either left tl2 tr, gti, rnd)
				= (RIGHT r, Either left tl tr2, gti, rnd)
			= abort ("Wrong trace in generate{|EITHER|}: "+toString t)

generate{|CONS|} f t rnd
	# (x, t, v, rnd) = f t rnd
	= (CONS x,  t,\(CONS a) = v a, rnd)

generate{|FIELD|} f t rnd
	# (x , t, v, rnd) = f t rnd
	= (FIELD x, t, \(FIELD a)= v a, rnd)

generate{|OBJECT|} f t rnd
	# (x , t, v, rnd) = f t rnd
	= (OBJECT x, t, \(OBJECT a)= v a, rnd)

generate{|(->)|} fa fr t rnd
	# (i,rnd)	= genElem rnd
	  (rnd2,rnd)= split rnd
	  (len,rnd)	= genElem rnd
	  len		= abs len rem 43 + 37
	  results	= take len (g Empty rnd2)
	  len		= length results
	= (\a = let (_,_,v,_) = fa t rnd in results !! (abs (i+v a) rem len),t,K len,rnd)
where
	g Done rnd = []
	g t rnd
		# (x, t,_, rnd) = fr t rnd
		= [x: (nextTrace t >>= g) rnd]

generic genToInt a :: a -> Int
genToInt{|UNIT|} a = 0
genToInt{|PAIR|} gti_l gti_r (PAIR l r) = gti_l l + gti_r r
genToInt{|EITHER|} gti_l gti_r (LEFT l) = 2*gti_l l
genToInt{|EITHER|} gti_l gti_r (RIGHT r) = 1+2*gti_r r
genToInt{|Int|} a = a
genToInt{|Bool|} a | a = 1; = 0
genToInt{|Real|} a = toInt a
genToInt{|String|} a = size a
genToInt{|c|} a = 0

generateAll :: RandomStream -> [a] | generate{|*|} a
generateAll random_list = g Empty random_list 
where
	g Done rnd = []
	g t rnd
		# (x, t, _, rnd) = generate{|*|} t rnd
		= [x: (nextTrace t >>= g) rnd]
/*
genT :: RandomStream -> [(a,Trace,Trace)] | generate{|*|} a
genT random_list = g Empty random_list 
where
	g Done rnd = []
	g t rnd
		# (x, t`, _, rnd) = generate{|*|} t rnd
		= [(x,t,t`): g (nextTrace t`) rnd]
*/
derive gEq Result
derive bimap [], (,), (,,), (,,,), (,,,,), (,,,,,)
derive generate (,), (,,), (,,,), (,,,,), (,,,,,), []

//--- Random ---//

randomStream :: *env -> (RandomStream,*env)  | TimeEnv env
randomStream world
	# (time,world) = getCurrentTime world
	# seed         = (time.hours*60 + time.minutes)*60 + time.seconds + 42
	= (genRandInt seed,world)

class genElem a where genElem :: RandomStream -> .(a,RandomStream)

instance genElem Int where genElem [r:rnd] = (r rem IntSize,rnd)
instance genElem Char where genElem [r:rnd] = (toChar (32+((abs r) rem 94)),rnd)
instance genElem Bool where genElem [r:rnd] = (isOdd r,rnd)
instance genElem Real where genElem [r,s,t:rnd] = ((toReal r/toReal s)*toReal t,rnd)
instance genElem String
where
	genElem [r:rnd]
		# (chars,rnd)	= seqList (repeatn ((abs r) rem StrLen) genElem) rnd
		  string		= {c \\ c<-chars}
		= (string,rnd)

split :: RandomStream -> (RandomStream,RandomStream)
split [r,s:rnd]
	# seed = r*s
	| seed==0
		= split rnd
		= (rnd, genRandInt seed)

(>>=) infix 0 :: (a -> (b,a)) (b a -> d) -> a -> d
(>>=) f g = \st = let (r,st1) = f st in g r st1

result :: b -> a -> (b,a)
result b = \a = (b,a)

//--- testing ---//

verbose  :: RandomStream p -> [String] | Testable p
verbose rs p = testConfig rs verboseConfig p

verbosen :: !Int RandomStream p -> [String] | Testable p
verbosen n rs p = testConfig rs { verboseConfig & maxTests = n, maxArgs = 100*n } p

concise :: RandomStream p -> [String] | Testable p
concise rs p = testConfig rs countConfig p

concisen   :: !Int RandomStream p -> [String] | Testable p
concisen n rs p = testConfig rs { countConfig & maxTests = n, maxArgs = 100*n } p

quiet :: RandomStream p -> [String] | Testable p
quiet rs p = testConfig rs quietConfig p

quietn   :: !Int RandomStream p -> [String] | Testable p
quietn n rs p = testConfig rs { quietConfig & maxTests = n, maxArgs = 100*n } p

:: Config
 =	{ maxTests	:: Int
	, maxArgs	:: Int
	, every		:: Int Admin [String] -> [String]
	}

verboseConfig
 =	{ maxTests	= 100
	, maxArgs	= 1000
	, every		= \n r c = [blank,toString n,":":showArgs r.args c]
	}

blank :: String
blank =: { createArray len ' ' & [0] = '\r', [len-1] = '\r' } where len = 81

countConfig
 =	{ maxTests	= 100
	, maxArgs	= 10000
	, every		= \n r c = [toString n,"\r": c]
	}

quietConfig
 =	{ maxTests	= 100
	, maxArgs	= 10000
	, every		= \n r c = c
	}

gather :: [Admin] -> [[String]]
gather list = [r.args \\ r<- list]

testConfig :: RandomStream Config p -> [String] | Testable p
testConfig rs {maxTests,maxArgs,every} p = analyse (evaluate p rs newAdmin) maxTests maxArgs 0 0 []
where
	analyse [] ntests nargs 0    0    labels = [blank,"Proof: success for all arguments": conclude ntests nargs 0 0 labels]
	analyse [] ntests nargs nrej 0    labels = [blank,"Proof: Success for all not rejected arguments, ": conclude ntests nargs nrej 0 labels]
	analyse [] ntests nargs nrej nund labels
				| nrej+nund==maxTests-ntests = [blank,"All arguments used, all tests rejected or undefined ": conclude ntests nargs nrej nund labels]
											 = [blank,"Success for arguments, ": conclude ntests nargs nrej nund labels]
	analyse _  0      nargs nrej nund labels = [blank,"Passed": conclude 0 nargs nrej nund labels]
	analyse _  ntests 0     nrej nund labels
		| ntests==maxTests				= [blank,"No tests performed, maximum number of arguments (",toString maxArgs,") generated": conclude ntests 0 nrej nund labels]
										= [blank,"Passed: maximum number of arguments (",toString maxArgs,") generated": conclude ntests 0 nrej nund labels]
	analyse [res:rest] ntests nargs nrej nund labels
		= every (maxTests-ntests+1) res
		  (	case res.res of
			 OK 	= analyse rest (ntests-1) (nargs-1) nrej nund (admin res.labels labels)
			 CE 	= ["\n":showName res.name ["Counter-example found after ",toString (maxTests-ntests+1)," tests:":showArgs res.args ["\n":showLabels (maxTests-ntests+1) (sort (admin res.labels labels))]]]
			 Rej	= analyse rest ntests (nargs-1) (nrej+1) nund     labels
			 Undef	= analyse rest ntests (nargs-1) nrej     (nund+1) labels
		  )

	conclude ntests nargs nrej nund labels
		# n    = maxTests-ntests
		  rest = showLabels n (sort labels)
		  rest = case nrej of
		  			0 = rest
		  			1 = [" one case rejected":rest]
		  			  = [", ",toString nrej," cases rejected":rest]
		  mu   = (maxArgs-nargs-nrej-n)
		  rest = case nund of
		  			0 = rest
		  			1 = [" one case undefined":rest]
		  			  = [", ",toString nund," cases undefined":rest]
		= [" after ",toString n," tests":rest]

	admin :: [String] [(String,Int)] -> [(String,Int)]
	admin [] accu = accu
	admin [label:rest] accu = admin rest (insert label accu)

	insert :: String [(String,Int)] -> [(String,Int)]
	insert label [] = [(label,1)]
	insert label [this=:(old,n):rest]
	 | label==old
		= [(old,n+1):rest]
		= [this:insert label rest]

	showLabels :: Int [(String,Int)] -> [String]
	showLabels ntests [] = ["\n"]
	showLabels ntests [(lab,n):rest] = ["\n",lab,": ",toString n," (",toString (toReal (n*100)/toReal ntests),"%)":showLabels ntests rest]

	showName l c = s (reverse l) c
	where
		s [] c = c
		s [l] c = [l," ":c]
		s [a:x] c = [a,".":s x c]

cr :== "\r"

showArgs :: [String] [String] -> [String]
showArgs []       c = c // ["\n":c] // c
showArgs [a:rest] c = [" ",a: showArgs rest c]
