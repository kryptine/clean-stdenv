definition module testable

/*
Pieter Koopman 2002
Nijmegen University, The Netherlands

GAST: A Generic Automatic Software Test-system
*/

import StdEnv, genLibTest, StdTime //, Property
from stdProperty import ::Property // for instance of testable

//--- basics --//

:: Admin = {res::Result, labels::[String], args::[String], name::[String]}
:: Result = Undef | Rej | Pass | OK | CE
:: Trace
:: RandomStream :== [Int]

derive gLess Result
instance == Result

:: Property = Prop (RandomStream Admin -> [Admin])

prop :: a -> Property | Testable a

randomStream :: *env -> (RandomStream,*env)  | TimeEnv env

generic generate a :: Trace RandomStream -> (a, Trace, a->Int, RandomStream)

class TestArg a | genShow{|*|}, generate{|*|} a
class Testable a where evaluate :: a RandomStream Admin -> [Admin]

instance Testable Bool
instance Testable Property
instance Testable (a->b) | Testable b & TestArg a  

derive bimap [], (,), (,,), (,,,), (,,,,), (,,,,,)
derive generate (,), (,,), (,,,), (,,,,), (,,,,,), [], Int, Char, Bool, Real, String, UNIT, PAIR, EITHER, CONS, FIELD, OBJECT, (->)

predInts	:== [0,1,-1]
predChars	:== ['aZ ~']
predReals	:== [0.0, 1.0, -1.0]
predStrings :== ["","\"\"","Hello world!"]
StrLen		:== 25
IntSize		:== 1000
MaxExists	:== 500 //500
MaxNoCE		:== 500

//--- for implementationof properties ---//

forAll :: !(a->b) ![a] RandomStream Admin -> [Admin] | Testable b & TestArg a
split :: RandomStream -> (RandomStream,RandomStream)
generateAll :: RandomStream -> [a] | generate{|*|} a

//--- testing --//

verbose  ::      RandomStream p -> [String] | Testable p
verbosen :: !Int RandomStream p -> [String] | Testable p
concise  ::      RandomStream p -> [String] | Testable p
concisen :: !Int RandomStream p -> [String] | Testable p
quiet    ::      RandomStream p -> [String] | Testable p
quietn   :: !Int RandomStream p -> [String] | Testable p

test :== verbosen 20
