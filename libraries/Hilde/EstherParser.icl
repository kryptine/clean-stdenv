implementation module EstherParser

import StdException, StdGeneric, StdMaybe
import StdParsComb, StdBool, StdList, StdEnum, StdFunc

parseStatement :: !String -> /*Src*/ NTexpression //NTstatement
parseStatement input 
	= case begin (sp parser{|*|} <& sp eof) (fromString input) of
		[([], syntax)] -> syntax
		[] -> raise SyntaxError
		_ -> raise AmbigiousParser

generic parser a :: CParser Char a b

parser{|UNIT|} = yield UNIT

parser{|PAIR|} gl gr = gl <&> \l -> sp gr <@ PAIR l

parser{|EITHER|} gl gr = gl <@ LEFT <!> gr <@ RIGHT

parser{|CONS|} gx = gx <@ CONS

parser{|FIELD|} gx = gx <@ FIELD

parser{|OBJECT|} gx = gx <@ OBJECT

parser{|Bool|} 
	= token ['True'] &> yield True 
	<!> token ['False'] &> yield False

parser{|Char|} = symbol '\'' &> character ['\''] <& symbol '\''

parser{|Int|} 
	= <?> (symbol '+') &> nat 
	<!> symbol '-' &> nat <@ ~
where
	nat 
		= token ['0x'] &> number_ 16
		<!> symbol '0' &> number_ 8
		<!> number_ 10
	
parser{|Real|} = (realWithExp <!> realWithoutExp) <@ toReal o toString
where
	realWithoutExp = int <?= ['0'] <++> symbol '.' <:&> numberList_ 10
	realWithExp = int <++> (symbol '.' <:&> numberList_ 10) <?= ['0'] <++> symbol 'E' <:&> int
	int = symbol '-' <:&> numberList_ 10 <!> <?> (symbol '+') &> numberList_ 10 

parser{|String|} = symbol '"' &> <*> (character ['"']) <& symbol '"' <@ toString
/*
parser{|Src|} ge = p
where
	p sc xc ac ss = ge sc` xc ac ss
	where
		sc` x xc` ac` ss` = sc {node = x, src = toString (take (length ss - length ss`) ss)} xc` ac` ss`
*/
parser{|NTexpression|} = (parser{|*|} <@ Sugar <!> parser{|*|} <@ Term) <&> p
where
	p a = (parser{|*|} <&> \UNIT -> parser{|*|} <&> \b -> p (Apply a b)) 
		<!> yield a

parser{|NTvariable|} 
	= lowercaseIdentifier <&> (\n -> if (isMember n keywords) fail (yield (NTvariable n)))
/*
parser{|NTnameOrValue|}
	= parser{|*|} <@ (\x -> NTvalue (dynamic x :: String) GenConsNoPrio)
	<!> <+> (character (spacechars ++ symbolchars)) <&> \cs -> case begin p cs of [([], parser)] -> parser
where
	p 
		= parser{|*|} <& eof <@ (\x -> yield (NTvalue (dynamic x :: Real) GenConsNoPrio))
		<!> parser{|*|} <& eof <@ (\x -> yield (NTvalue (dynamic x :: Int) GenConsNoPrio))
		<!> parser{|*|} <& eof <@ (\x -> yield (NTvalue (dynamic x :: Char) GenConsNoPrio))
		<!> parser{|*|} <& eof <@ (\x -> yield (NTvalue (dynamic x :: Bool) GenConsNoPrio))
		<!> (lowercaseIdentifier <!> uppercaseIdentifier <!> funnyIdentifier) <& eof <@ (\n -> if (isMember n keywords) fail (yield (NTname n)))
		<!> <+> (satisfy (const True)) <@ (\n -> yield (NTname (toString n)))
*/
parser{|NTnameOrValue|}
	= parser{|*|} <@ (\x -> NTvalue (dynamic x :: String) GenConsNoPrio)
	<!> parser{|*|} <@ (\x -> NTvalue (dynamic x :: Real) GenConsNoPrio)
	<!> parser{|*|} <@ (\x -> NTvalue (dynamic x :: Int) GenConsNoPrio)
	<!> parser{|*|} <@ (\x -> NTvalue (dynamic x :: Char) GenConsNoPrio)
	<!> parser{|*|} <@ (\x -> NTvalue (dynamic x :: Bool) GenConsNoPrio)
	<!> (lowercaseIdentifier <!> uppercaseIdentifier <!> funnyIdentifier) <&> (\n -> if (isMember n keywords) fail (yield (NTname n)))

parser{|(|-|)|} ga ge gb = ga &> sp ge <& sp gb <@ |-|

parser{|(+-)|} ge gs = parseSequence (sp ge) (sp gs) <@ \[x:xs] -> +- [x:xs]

parser{|Maybe|} ge 
	= ge <@ Just
	<!> yield Nothing

parser{|Topen|} = symbol '(' &> yield Topen
parser{|Tclose|} = symbol ')' &> yield Tclose
parser{|TopenBracket|} = symbol '[' &> yield TopenBracket
parser{|TcloseBracket|} = symbol ']' &> yield TcloseBracket
parser{|Tlambda|} = keyword "\\" &> yield Tlambda
parser{|Tarrow|} = keyword "->" &> yield Tarrow
parser{|Tlet|} = keyword "let" &> yield Tlet
parser{|Tin|} = keyword "in" &> yield Tin
parser{|Tcase|} = keyword "case" &> yield Tcase
parser{|Tof|} = keyword "of" &> yield Tof
parser{|Tsemicolon|} = symbol ';' &> yield Tsemicolon
parser{|Tcolon|} = keyword ":" &> yield Tcolon
parser{|Tcomma|} = symbol ',' &> yield Tcomma
parser{|Tunderscore|} = keyword "_" &> yield Tunderscore
parser{|Tis|} = keyword "=" &> yield Tis
parser{|Tdotdot|} = keyword ".." &> yield Tdotdot
parser{|Tzf|} = keyword "\\\\" &> yield Tzf
parser{|TbackArrow|} = keyword "<-" &> yield TbackArrow
parser{|Tguard|} = keyword "|" &> yield Tguard
parser{|Tand|} = keyword "&" &> yield Tand

derive parser NTstatement, NTterm, NTsugar, NTlist, NTlistComprehension, NTqualifier, NTgenerator
derive parser NTlambda, NTpattern, NTlet, NTletDef, NTcase, NTcaseAlt
derive parser Scope, (,)

character :: ![Char] -> CParser Char Char t
character delimiters
	= symbol '\\' &> escaped
	<!> satisfy (\c -> not (isMember c delimiters))
where
	escaped
		= symbol 'n' <@ const '\n'
		<!> symbol 'r' <@ const '\r'
		<!> symbol 'f' <@ const '\f'
		<!> symbol 'b' <@ const '\b'
		<!> symbol 't' <@ const '\t'
		<!> symbol 'v' <@ const '\v'
		<!> symbol 'x' &> number 2 16 <@ toChar
		<!> symbol 'X' &> number 2 16 <@ toChar
		<!> symbol 'd' &> number 3 10 <@ toChar
		<!> symbol 'D' &> number 3 10 <@ toChar
		<!> number 3 8 <@ toChar
		<!> satisfy (\_ -> True)

number_ :: Int -> CParser Char Int t
number_ base = number (1 << 31 - 1) base

number :: Int Int -> CParser Char Int t
number n base = numberList n base <@ convert 0
where
	convert x [] = x
	convert x [c:cs] = convert (x * base + (numberchars ?? c)) cs

numberList_ :: Int -> CParser Char [Char] t
numberList_ base = numberList (1 << 31 - 1) base

numberList :: Int Int -> CParser Char [Char] t
numberList n base 
	| n < 1 = fail
	| n == 1 = satisfy (\c -> isMember (toUpper c) (take base numberchars)) <@ \c -> [toUpper c]
	= numberList 1 base <&> \[c] -> numberList (n - 1) base <?= [] <@ \cs -> [c:cs]

keyword c = (lowercaseIdentifier <!> uppercaseIdentifier <!> funnyIdentifier) <&> \n -> if (n == c && isMember n keywords) (yield n) fail 

lowercaseIdentifier = satisfy (\c -> isMember c lowerchars) <:&> <*> (satisfy (\c -> isMember c alphachars)) <@ toString
uppercaseIdentifier = satisfy (\c -> isMember c upperchars) <:&> <*> (satisfy (\c -> isMember c alphachars)) <@ toString
funnyIdentifier = <+> (satisfy (\c -> isMember c funnychars)) <@ toString

keywords =: ["=", "->", "let", "in", "case", "of", "\\", "_", ":", "..", "\\\\", "<-", "|", "&"]
symbolchars =: ['\',();[]{}"']
spacechars =: ['\t\n\r\v ']
funnychars =: ['\\?.=:$!@#%^&*+-<>/|~']
lowerchars =: ['a'..'z'] ++ ['_', '`']
upperchars =: ['A'..'Z']
digitchars =: ['0'..'9']
numberchars =: digitchars ++ upperchars
alphachars =: numberchars ++ lowerchars

(??) infix 9
(??) xs y :== find xs 0
where
	find [] _ = raise "??: not found in list?!"
	find [x:xs] i
		| x == y = i
		= find xs (i + 1)

(<?=) infix 7
(<?=) p def :== <?> p <@ \l -> case l of [x] -> x; _ -> def
/*
generic pretty e :: !Bool e -> String
pretty{|UNIT|} _ _ = ""
pretty{|EITHER|} gl gr p (LEFT l) = gl p l
pretty{|EITHER|} gl gr p (RIGHT r) = gr p r
pretty{|CONS|} gx p (CONS x) = gx p x
pretty{|OBJECT|} gx p (OBJECT x) = gx p x
pretty{|FIELD|} gx p (FIELD x) = gx p x
pretty{|PAIR|} gl gr p (PAIR l r) = gl p l +++ " " +++ gr p r

pretty{|NTexpression|} p (Term x) = pretty{|*|} p x
pretty{|NTexpression|} False (Apply f _ x) = pretty{|*|} True f +++ " " +++ pretty{|*|} True x
pretty{|NTexpression|} True (Apply f _ x) = "(" +++ pretty{|*|} True f +++ " " +++ pretty{|*|} True x +++ ")"

pretty{|NTnameOrValue|} _ (NTvalue d p) = fst (prettyDynamic d)
pretty{|NTnameOrValue|} _ (NTname n) = n

pretty{|NTvariable|} _ (NTvariable n) = n

pretty{|Src|} gx p {node} = gx p node
pretty{|(+-)|} ga gb _ (+- [a]) = ga False a
pretty{|(+-)|} ga gb _ (+- [a:as]) = ga False a +++ gb False (raise "pretty") +++ pretty{|*->*->*|} ga gb False (+- as)

pretty{|Tsemicolon|} _ _ = ";"
pretty{|Tclose|} _ _ = ")"
pretty{|Topen|} _ _ = "("
pretty{|TcloseBracket|} _ _ = "]"
pretty{|TopenBracket|} _ _ = "["
pretty{|Tof|} _ _ = "of"
pretty{|Tcase|} _ _ = "case"
pretty{|Tcomma|} _ _ = ","
pretty{|Tarrow|} _ _ = "->"
pretty{|Tlambda|} _ _ = "\\"
pretty{|Tlet|} _ _ = "let"
pretty{|Tin|} _ _ = "in"
pretty{|Tis|} _ _ = "="
pretty{|Tcolon|} _ _ = ":"
pretty{|Tunderscore|} _ _ = "_"
pretty{|Tdotdot|} _ _ = ".."

derive pretty NTstatement, NTterm, NTsugar, NTlambda, NTlet, NTletDef, NTcase, NTcaseAlt, NTpattern, NTlist, NTlistComprehension
derive pretty Scope, Maybe, (,), |-|
*/