implementation module EstherPostParser

import EstherParser
import StdMisc, StdList, StdString, EstherBackend

generic resolveNames e :: !e ![String] !*env -> (!e, ![String], !*env) | resolveFilename env

resolveNames{|c|} e vs st = (e, vs, st)

resolveNames{|PAIR|} gl gr (PAIR l r) vs st = (PAIR l` r`, vs``, st``)
where
	  (l`, vs`, st`) = gl l vs st
	  (r`, vs``, st``) = gr r vs` st`

resolveNames{|EITHER|} gl gr (LEFT l) vs st = (LEFT l`, vs`, st`)
where
	(l`, vs`, st`) = gl l vs st
resolveNames{|EITHER|} gl gr (RIGHT r) vs st = (RIGHT r`, vs`, st`)
where
	(r`, vs`, st`) = gr r vs st

resolveNames{|CONS|} gx (CONS x) vs st = (CONS x`, vs`, st`)
where
	(x`, vs`, st`) = gx x vs st

resolveNames{|FIELD|} gx (FIELD x) vs st = (FIELD x`, vs`, st`)
where
	(x`, vs`, st`) = gx x vs st

resolveNames{|OBJECT|} gx (OBJECT x) vs st = (OBJECT x`, vs`, st`)
where
	(x`, vs`, st`) = gx x vs st
/*
resolveNames{|Src|} ge src=:{node} vs st = ({src & node = node`}, vs`, st`)
where
	(node`, vs`, st`) = ge node vs st
*/
resolveNames{|NTlet|} (NTlet t1 (+- p_ds) t2 e) vs st = (NTlet t1 p_ds` t2 e`, vs`, st```)
where
	ps = [p \\ NTletDef p _ _ <- p_ds]
	(_, vs`, st`) = resolveNames{|*|} ps vs st
	(p_ds`, _, st``) = resolveNames{|*|} (+- p_ds) vs` st`
	(e`, _, st```) = resolveNames{|*|} e vs` st``

resolveNames{|NTvariable|} e=:(NTvariable n) vs st = (e, [n:vs], st)

resolveNames{|NTnameOrValue|} (NTvalue (x :: a) prio) vs st 
	#!x = x
	= (NTvalue (dynamic x :: a) prio, vs, st)
resolveNames{|NTnameOrValue|} (NTname n) vs st 
	| isMember n vs = (NTname n, vs, st)
	= (NTvalue dyn prio, vs, st`)
where
	(dyn, prio, st`) = resolveFilename n st

resolveNames{|Scope|} ge (Scope e) vs st = (Scope e`, vs, st`)
where
	(e`, _, st`) = ge e vs st

resolveNames{|NTterm|} (Sugar e) vs st = (Plain (Nested (|-| e`)), vs`, st`)
where
	(e`, vs`, st`) = resolveNames{|*|} (desugar e) vs st
resolveNames{|NTterm|} (Plain e) vs st = (Plain e`, vs`, st`)
where
	(e`, vs`, st`) = resolveNames{|*|} e vs st

derive resolveNames NTstatement, NTexpression, NTsugar, NTplain, NTlist, NTlambda, NTpattern, NTletDef, NTcase, NTcaseAlt, NTlistComprehension
derive resolveNames +-, |-|, [], Maybe, (,)

desugar :: !NTsugar -> NTexpression
desugar (Tuple _ e _ (+- es) _) = foldl (\f x -> Apply f (Plain (Nested (|-| x)))) (Term (Plain (NameOrValue (NTvalue (dynamicTuple (length es`)) GenConsNoPrio)))) es`
where
	es` = [e:es]
desugar (List (|-| e)) = desugarList e
where
	desugarList (Cons hds Nothing) = desugarList (Cons hds (Just (Tcolon, Term (Sugar (List (|-| Nil))))))
	desugarList (Cons (+- [hd:hds]) (Just tl)) = Apply (Apply (Term (Plain (NameOrValue (NTvalue dynamicCons GenConsNoPrio)))) (Plain (Nested (|-| hd)))) (Plain (Nested (|-| (Term (Sugar (List (|-| (Cons (+- hds) (Just tl)))))))))
	desugarList (Cons (+- []) (Just (_, tl))) = tl
	desugarList Nil = Term (Plain (NameOrValue (NTvalue dynamicNil GenConsNoPrio)))
	desugarList (ListComprehension c) = desugarListComprehension c
	
	desugarListComprehension (DotDot f t _ e) = desugarDotDot f t e
	desugarListComprehension (ZF e _ qs) = raise "ZF not implemented"

	desugarDotDot f Nothing Nothing = Apply (Term (Plain (NameOrValue (NTvalue dynamicFrom GenConsNoPrio)))) (Plain (Nested (|-| f)))
	desugarDotDot f Nothing (Just e) = Apply (Apply (Term (Plain (NameOrValue (NTvalue dynamicFromTo GenConsNoPrio)))) (Plain (Nested (|-| f)))) (Plain (Nested (|-| e)))
	desugarDotDot f (Just (_, t)) Nothing = Apply (Apply (Term (Plain (NameOrValue (NTvalue dynamicFromThen GenConsNoPrio)))) (Plain (Nested (|-| f)))) (Plain (Nested (|-| t)))
	desugarDotDot f (Just (_, t)) (Just e) = Apply (Apply (Apply (Term (Plain (NameOrValue (NTvalue dynamicFromThenTo GenConsNoPrio)))) (Plain (Nested (|-| f)))) (Plain (Nested (|-| t)))) (Plain (Nested (|-| e)))

generic fixInfix e :: !e -> e

fixInfix{|c|} e = e

fixInfix{|PAIR|} gl gr (PAIR l r) = PAIR (gl l) (gr r)

fixInfix{|EITHER|} gl gr (LEFT l) = LEFT (gl l)
fixInfix{|EITHER|} gl gr (RIGHT r) = RIGHT (gr r)

fixInfix{|CONS|} gx (CONS x) = CONS (gx x)

fixInfix{|FIELD|} gx (FIELD x) = FIELD (gx x)

fixInfix{|OBJECT|} gx (OBJECT x) = OBJECT (gx x)

//fixInfix{|Src|} gx src=:{node} = {src & node = gx node}

fixInfix{|NTexpression|} e = fix e []
where
	fix (Term e) es = ap (Term (fixInfix{|*|} e)) es
	fix (Apply _ (Value _ (GenConsPrio _ _))) [] = raise InfixRightArgumentMissing
	fix (Apply l r) es
		# r = fixInfix{|*|} r
		= case l of
			(Apply ll lr=:(Value _ (GenConsPrio rightAssoc rightPrio)))
				# ll = fixInfix{|*|} ll
				  leftish = Apply (Apply (Term (fixInfix{|*|} lr)) (Plain (Nested (|-| ll)))) (Plain (Nested (|-| (ap (Term r) es))))
				-> case ll of
					Apply (Apply llll=:(Term (Value _ (GenConsPrio leftAssoc leftPrio))) lllr) llr
						# rightish = Apply (Apply llll lllr) (Plain (Nested (|-| (Apply (Apply (Term (fixInfix{|*|} lr)) llr) r))))
						| rightPrio < leftPrio -> leftish
						| leftPrio < rightPrio -> rightish
						-> case (rightAssoc, leftAssoc) of
							(GenConsAssocLeft, GenConsAssocLeft) -> leftish
							(GenConsAssocRight, GenConsAssocRight) -> rightish
							-> raise UnsolvableInfixOrder
					_ -> leftish
			(Term (Value _ (GenConsPrio _ _))) -> raise InfixLeftArgumentMissing
			_ -> fix l [r:es]

	ap f [] = f
	ap f [x:xs] = ap (Apply f x) xs

Value d p :== Plain (NameOrValue (NTvalue d p))

derive fixInfix NTstatement, NTterm, NTsugar, NTlist, NTlistComprehension, NTlambda, NTpattern, NTlet, NTletDef, NTcase, NTcaseAlt
derive fixInfix +-, |-|, [], Maybe, (,), Scope

derive bimap (,), (,,)

dynamicTuple :: !Int -> Dynamic
dynamicTuple 2 = dynamicTuple2
dynamicTuple 3 = dynamicTuple3
dynamicTuple 4 = dynamicTuple4
dynamicTuple 5 = dynamicTuple5
dynamicTuple 6 = dynamicTuple6
dynamicTuple 7 = dynamicTuple7
dynamicTuple 8 = dynamicTuple8
dynamicTuple 9 = dynamicTuple9
dynamicTuple 10 = dynamicTuple10
dynamicTuple 11 = dynamicTuple11
dynamicTuple 12 = dynamicTuple12
dynamicTuple n = raise "dynamicTuple<1||>12"

dynamicTuple2 =: dynamic \a b -> (a, b) :: A.a b: a b -> (a, b)
dynamicTuple3 =: dynamic \a b c -> (a, b, c) :: A.a b c: a b c -> (a, b, c)
dynamicTuple4 =: dynamic \a b c d -> (a, b, c, d) :: A.a b c d: a b c d -> (a, b, c, d)
dynamicTuple5 =: dynamic \a b c d e -> (a, b, c, d, e) :: A.a b c d e: a b c d e -> (a, b, c, d, e)
dynamicTuple6 =: dynamic \a b c d e f -> (a, b, c, d, e, f) :: A.a b c d e f: a b c d e f -> (a, b, c, d, e, f)
dynamicTuple7 =: dynamic \a b c d e f g -> (a, b, c, d, e, f, g) :: A.a b c d e f g: a b c d e f g -> (a, b, c, d, e, f, g)
dynamicTuple8 =: dynamic \a b c d e f g h -> (a, b, c, d, e, f, g, h) :: A.a b c d e f g h: a b c d e f g h -> (a, b, c, d, e, f, g, h)
dynamicTuple9 =: dynamic \a b c d e f g h i -> (a, b, c, d, e, f, g, h, i) :: A.a b c d e f g h i: a b c d e f g h i -> (a, b, c, d, e, f, g, h, i)
dynamicTuple10 =: dynamic \a b c d e f g h i j -> (a, b, c, d, e, f, g, h, i, j) :: A.a b c d e f g h i j: a b c d e f g h i j -> (a, b, c, d, e, f, g, h, i, j)
dynamicTuple11 =: dynamic \a b c d e f g h i j k -> (a, b, c, d, e, f, g, h, i, j, k) :: A.a b c d e f g h i j k: a b c d e f g h i j k -> (a, b, c, d, e, f, g, h, i, j, k)
dynamicTuple12 =: dynamic \a b c d e f g h i j k l -> (a, b, c, d, e, f, g, h, i, j, k, l) :: A.a b c d e f g h i j k l: a b c d e f g h i j k l -> (a, b, c, d, e, f, g, h, i, j, k, l)

dynamicCons =: dynamic \x xs -> [x:xs] :: A.a: a [a] -> [a]
dynamicNil =: dynamic [] :: A.a: [a]

dynamicFrom =: overloaded2 "+" "one" (dynamic (undef, undef, From) :: A.a: (a, a, (a a -> a) a a -> [a]))
where
	From add one n = frm n
	where
		frm n = [n : frm (add n one)]

dynamicFromTo =: overloaded3 "<" "+" "one" (dynamic (undef, undef, undef, FromTo) :: A.a: (a, a, a, (a a -> Bool) (a a -> a) a a a -> [a]))
where
	FromTo less add one n e = from_to n e
	where
		from_to n e
			| not (less e n) = [n : from_to (add n one) e]
			= []

dynamicFromThen =: overloaded2 "-" "+" (dynamic (undef, undef, From_then) :: A.a: (a, a, (a a -> a) (a a -> a) a a -> [a]))
where
	From_then sub add n1 n2 = [n1 : from_by n2 (sub n2 n1)]
	where
		from_by n s	= [n : from_by (add n s) s]

dynamicFromThenTo =: overloaded3 "<" "-" "+" (dynamic (undef, undef, undef, From_then_to) :: A.a: (a, a, a, (a a -> Bool) (a a -> a) (a a -> a) a a a -> [a]))
where
	From_then_to less sub add n1 n2 e
		| not (less n2 n1) = from_by_to n1 (sub n2 n1) e
		= from_by_down_to n1 (sub n2 n1) e
	where
		from_by_to n s e
			| not (less e n) = [n : from_by_to (add n s) s e]
			= []
		from_by_down_to n s e
			| not (less n e) = [n : from_by_down_to (add n s) s e]
			= []
