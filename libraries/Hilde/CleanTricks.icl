implementation module CleanTricks

import StdInt, StdArray, StdEnum
import StdTuple, StdList, StdString

unsafeTypeAttrCast :: !.a -> .b
unsafeTypeAttrCast _ = code {
		pop_a		0
	}

unsafeTypeCast :: !u:a -> u:b
unsafeTypeCast x = unsafeTypeAttrCast x

matchConstructor :: !a !a -> Bool
matchConstructor x y = code {
		pushD_a		1
		pushD_a 	0
		pop_a		2
		eqI
	}

descriptorArity :: !a -> Int
descriptorArity _ = code {
		get_desc_arity	0
		pop_a			1
	}

unsafeSelect1of1 :: !a -> a1
unsafeSelect1of1 n = code {
		repl_args	1 1
		jsr_eval	0
	}

unsafeSelect1of2 :: !a -> a1
unsafeSelect1of2 n = code {
		repl_args	2 2
		jsr_eval	0
		updatepop_a	0 1
	}

unsafeSelect2of2 :: !a -> a2
unsafeSelect2of2 n = code {
		repl_args	2 2
		jsr_eval	1
		pop_a		1
	}

unsafeSelect1of3 :: !a -> a1
unsafeSelect1of3 n = code {
		repl_args	3 3
		jsr_eval	0
		updatepop_a	0 2
	}

unsafeSelect2of3 :: !a -> a2
unsafeSelect2of3 n = code {
		repl_args	3 3
		jsr_eval	1
		updatepop_a 1 2
	}

unsafeSelect3of3 :: !a -> a3
unsafeSelect3of3 n = code {
		repl_args	3 3
		jsr_eval	2
		pop_a		2
	}

unsafeSelect1of4 :: !a -> a1
unsafeSelect1of4 n = code {
		repl_args	4 4
		jsr_eval	0
		updatepop_a	0 3
	}

unsafeSelect2of4 :: !a -> a2
unsafeSelect2of4 n = code {
		repl_args	4 4
		jsr_eval	1
		updatepop_a 1 3
	}

unsafeSelect3of4 :: !a -> a3
unsafeSelect3of4 n = code {
		repl_args	4 4
		jsr_eval	2
		updatepop_a 2 3
	}

unsafeSelect4of4 :: !a -> a4
unsafeSelect4of4 n = code {
		repl_args	4 4
		jsr_eval	3
		pop_a		3
	}

unsafeSelect1of5 :: !a -> a1
unsafeSelect1of5 n = code {
		repl_args	5 5
		jsr_eval	0
		updatepop_a	0 4
	}

unsafeSelect2of5 :: !a -> a2
unsafeSelect2of5 n = code {
		repl_args	5 5
		jsr_eval	1
		updatepop_a 1 4
	}

unsafeSelect3of5 :: !a -> a3
unsafeSelect3of5 n = code {
		repl_args	5 5
		jsr_eval	2
		updatepop_a 2 4
	}

unsafeSelect4of5 :: !a -> a4
unsafeSelect4of5 n = code {
		repl_args	5 5
		jsr_eval	3
		updatepop_a	3 4
	}

unsafeSelect5of5 :: !a -> a5
unsafeSelect5of5 n = code {
		repl_args	5 5
		jsr_eval	4
		pop_a		4
	}

unsafeSelect1of6 :: !a -> a1
unsafeSelect1of6 n = code {
		repl_args	6 6
		jsr_eval	0
		updatepop_a	0 5
	}

unsafeSelect2of6 :: !a -> a2
unsafeSelect2of6 n = code {
		repl_args	6 6
		jsr_eval	1
		updatepop_a 1 5
	}

unsafeSelect3of6 :: !a -> a3
unsafeSelect3of6 n = code {
		repl_args	6 6
		jsr_eval	2
		updatepop_a 2 5
	}

unsafeSelect4of6 :: !a -> a4
unsafeSelect4of6 n = code {
		repl_args	6 6
		jsr_eval	3
		updatepop_a	3 5
	}

unsafeSelect5of6 :: !a -> a5
unsafeSelect5of6 n = code {
		repl_args	6 6
		jsr_eval	4
		updatepop_a	4 5
	}

unsafeSelect6of6 :: !a -> a6
unsafeSelect6of6 n = code {
		repl_args	6 6
		jsr_eval	5
		pop_a		5
	}

unsafeSelect1of7 :: !a -> a1
unsafeSelect1of7 n = code {
		repl_args	7 7
		jsr_eval	0
		updatepop_a	0 6
	}

unsafeSelect2of7 :: !a -> a2
unsafeSelect2of7 n = code {
		repl_args	7 7
		jsr_eval	1
		updatepop_a 1 6
	}

unsafeSelect3of7 :: !a -> a3
unsafeSelect3of7 n = code {
		repl_args	7 7
		jsr_eval	2
		updatepop_a 2 6
	}

unsafeSelect4of7 :: !a -> a4
unsafeSelect4of7 n = code {
		repl_args	7 7
		jsr_eval	3
		updatepop_a	3 6
	}

unsafeSelect5of7 :: !a -> a5
unsafeSelect5of7 n = code {
		repl_args	7 7
		jsr_eval	4
		updatepop_a	4 6
	}

unsafeSelect6of7 :: !a -> a6
unsafeSelect6of7 n = code {
		repl_args	7 7
		jsr_eval	5
		updatepop_a	5 6
	}

unsafeSelect7of7 :: !a -> a7
unsafeSelect7of7 n = code {
		repl_args	7 7
		jsr_eval	6
		pop_a		6
	}

unsafeSelect1of8 :: !a -> a1
unsafeSelect1of8 n = code {
		repl_args	8 8
		jsr_eval	0
		updatepop_a	0 7
	}

unsafeSelect2of8 :: !a -> a2
unsafeSelect2of8 n = code {
		repl_args	8 8
		jsr_eval	1
		updatepop_a 1 7
	}

unsafeSelect3of8 :: !a -> a3
unsafeSelect3of8 n = code {
		repl_args	8 8
		jsr_eval	2
		updatepop_a 2 7
	}

unsafeSelect4of8 :: !a -> a4
unsafeSelect4of8 n = code {
		repl_args	8 8
		jsr_eval	3
		updatepop_a	3 7
	}

unsafeSelect5of8 :: !a -> a5
unsafeSelect5of8 n = code {
		repl_args	8 8
		jsr_eval	4
		updatepop_a	4 7
	}

unsafeSelect6of8 :: !a -> a6
unsafeSelect6of8 n = code {
		repl_args	8 8
		jsr_eval	5
		updatepop_a	5 7
	}

unsafeSelect7of8 :: !a -> a7
unsafeSelect7of8 n = code {
		repl_args	8 8
		jsr_eval	6
		updatepop_a	6 7
	}

unsafeSelect8of8 :: !a -> a8
unsafeSelect8of8 n = code {
		repl_args	8 8
		jsr_eval	7
		pop_a		7
	}
