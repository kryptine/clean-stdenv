implementation module StdArrayExtensions

import StdEnv

// extensions for StdArray

createStrictArr :: !Int !a -> .{!a}
createStrictArr size el
	= createArray size el

createLazyArr :: !Int !a -> .{a}
createLazyArr size el
	= createArray size el

createString :: !Int !Char -> .String
createString size el
	= createArray size el

createUnboxedIntArr :: !Int !Int -> .{#Int}
createUnboxedIntArr size el
	= createArray size el

createUnboxedRealArr :: !Int !Real -> .{#Real}
createUnboxedRealArr size el
	= createArray size el

class updateArrElt a :: !(.e -> .e) !Int !*(a .e) -> *(a .e)

instance updateArrElt {}
  where
	updateArrElt f index array = updateArrElt_Lazy f index array

instance updateArrElt {!}
  where
	updateArrElt f index array = updateArrElt_Strict f index array

updateArrElt_Lazy :: !(.e -> .e) !Int !*{.e} -> *{.e}
updateArrElt_Lazy f i a
	= code
		{
			push_b 0
			push_a 1
			update_a 1 2
			updatepop_a 0 1
			push_a 0
			select _ 1 0
			push_a 0
			push_a 3
			build e_system_dAP 2 e_system_nAP
			push_b 0
			push_a 2
			update_a 1 4
			updatepop_a 0 3
			updatepop_b 0 1
			update _ 1 0
		}

updateArrElt_Strict :: !(.e -> .e) !Int !*{!.e} -> *{!.e}
updateArrElt_Strict f i a
	= code
		{
			push_b 0
			push_a 1
			update_a 1 2
			updatepop_a 0 1
			push_a 0
			select _ 1 0
			push_a 0
			push_a 3
			update_a 3 4
			update_a 1 3
			updatepop_a 0 2
		.d 2 0
			jsr e_system_sAP
		.o 1 0
			push_b 0
			push_a 1
			update_a 1 2
			updatepop_a 0 1
			updatepop_b 0 1
			update _ 1 0
		.d 1 0
			rtn
		}

class accArrElt a :: !(.e -> (!.x, !.e)) !Int !*(a .e) -> (!.x, !*(a .e))

instance accArrElt {}
  where
	accArrElt f i a
		= accArrElt_Lazy f i a

instance accArrElt {!}
  where
	accArrElt f i a
		= accArrElt_Strict f i a

accArrElt_Lazy :: !(.e -> (!.x, !.e)) !Int !*{.e} -> (!.x, !*{.e})
accArrElt_Lazy f i a
	= code
		{
			push_b 0
			push_a 1
			update_a 1 2
			updatepop_a 0 1
			push_a 0
			select _ 1 0
			push_a 0
			push_a 3
			update_a 3 4
			update_a 1 3
			updatepop_a 0 2
		.d 2 0
			jsr e_system_sAP
		.o 1 0
			push_arg 0 2 2
			push_b 0
			push_a 2
			updatepop_b 0 1
			update_a 2 3
			update_a 1 2
			updatepop_a 0 1
			update _ 1 0
			push_arg 1 2 1
			jsr_eval 0
			update_a 1 2
			updatepop_a 0 1
		.d 2 0
			rtn
		}		

accArrElt_Strict :: !(.e -> (!.x, !.e)) !Int !*{!.e} -> (!.x, !*{!.e})
accArrElt_Strict f i a
	= code
		{
			push_b 0
			push_a 1
			update_a 1 2
			updatepop_a 0 1
			push_a 0
			select _ 1 0
			push_a 0
			push_a 3
			update_a 3 4
			update_a 1 3
			updatepop_a 0 2
		.d 2 0
			jsr e_system_sAP
		.o 1 0
			push_arg 0 2 2
			jsr_eval 0
			push_b 0
			push_a 2
			updatepop_b 0 1
			update_a 2 3
			update_a 1 2
			updatepop_a 0 1
			update _ 1 0
			push_arg 1 2 1
			jsr_eval 0
			update_a 1 2
			updatepop_a 0 1
		.d 2 0
			rtn
		}
/*
	# (ai, a) = a![i]
	  (x, fai) = f ai
	= (x, { a & [i] = fai })
*/


findlArrElt pred array i
	:== findl array i
  where
	findl array i
		| i>=size array || pred array.[i]
			= i
		= findl array (i+1)

findrArrElt pred array i
	:== findr array i
  where
	findr array i
		| i<0 || pred array.[i]
			= i
		= findr array (i-1)

//createStrictArrIncFoldSt :: !Int !(Int .st -> ([a], .st)) !.st -> (!.{![a]}, !.st)
createStrictArrIncFoldSt :: !Int !(Int .st -> (.a, .st)) !.st -> (!.{!.a}, !.st)
createStrictArrIncFoldSt size create_element st
/*
	| size<0
		= abort "createStrictArrIncFoldSt: called with negative size parameter\n"
	# new_array = createArray size []
	= createStrictArrIncFoldSt_loop 0 size 1 create_element new_array st
*/
	= code {
		.export createStrictArrIncFoldSt_loop
			pushI 0
			push_b 1
			ltI
			jmp_false ok1
			buildAC "createStrictArrIncFoldSt: called with negative size parameter\n"
			updatepop_a 0 2
			pop_b 1
		.d 1 0
			jsr e_StdMisc_sabort
		.o 1 0
			repl_args 2 2
			jsr_eval 1
			jsr_eval 0
			push_array 0
			update_a 0 1
			pop_a 1
		.d 2 0
			rtn
		:ok1
			buildh _Nil 0
			push_b 0
			create_array _ 1 0
			push_a 2
			push_a 2
			pushI 1
			push_b 1
			pushI 0
			push_a 2
			update_a 2 3
			update_a 0 2
			pop_a 1
			update_a 2 4
			update_a 1 3
			updatepop_a 0 2
			update_b 2 3
			update_b 1 2
			updatepop_b 0 1
		.d 3 3 iii
			jmp createStrictArrIncFoldSt_loop
		.o 3 3 iii
		:createStrictArrIncFoldSt_loop
			push_b 1
			push_b 1
			eqI
			jmp_false further
			push_a 2
			push_a 2
			update_a 1 4
			updatepop_a 0 3
			pop_b 3
		.d 2 0
			rtn
		:further
			buildI_b 0
			push_a 1
		.d 2 0
			jsr e_system_sAP
		.o 1 0
			push_a 3
			push_a 1
			update_a 1 2
			update_a 0 1
			pop_a 1
			buildh _Nil 0
			update_a 0 5
			pop_a 1
		.d 2 0
			jsr e_system_sAP
		.o 1 0
			push_arg 0 2 2
			jsr_eval 0
			push_arg 1 2 1
			jsr_eval 0
			push_b 0
			push_a 4
			buildh _Nil 0
			update_a 0 6
			update_a 0 4
			pop_a 1
			update _ 1 0
			push_b 2
			push_b 1
			addI
			push_a 3
			push_b 3
			push_b 3
			push_b 2
			update_b 2 3
			update_b 1 2
			update_b 0 1
			pop_b 1
			update_a 2 6
			update_a 1 5
			updatepop_a 0 4
			update_b 2 5
			update_b 1 4
			updatepop_b 0 3
		.d 3 3 iii
			jmp createStrictArrIncFoldSt_loop
	}
		
//createStrictArrDecFoldSt :: !Int !(Int .st -> ([a], .st)) !.st -> (!.{![a]}, !.st)
createStrictArrDecFoldSt :: !Int !(Int .st -> (.a, .st)) !.st -> (!.{!.a}, !.st)
createStrictArrDecFoldSt size create_element st
/*
	| size<0
		= abort "createStrictArrDecFoldSt: called with negative size parameter\n"
	# new_array = createArray size []
	= createStrictArrIncFoldSt_loop (size-1) (-1) (-1) create_element new_array st
*/
	= code {
			pushI 0
			push_b 1
			ltI
			jmp_false ok2
			buildAC "createStrictArrDecFoldSt: called with negative size parameter\n"
			updatepop_a 0 2
			pop_b 1
		.d 1 0
			jsr e_StdMisc_sabort
		.o 1 0
			repl_args 2 2
			jsr_eval 1
			jsr_eval 0
			push_array 0
			update_a 0 1
			pop_a 1
		.d 2 0
			rtn
		:ok2
			buildh _Nil 0
			push_b 0
			create_array _ 1 0
			pushI 1
			push_b 1
			subI
			push_a 2
			push_a 2
			pushI -1
			pushI -1
			push_a 2
			update_a 2 3
			update_a 0 2
			pop_a 1
			push_b 2
			update_b 2 3
			update_b 1 2
			update_b 0 1
			pop_b 1
			update_a 2 4
			update_a 1 3
			updatepop_a 0 2
			update_b 2 3
			update_b 1 2
			updatepop_b 0 1
		.d 3 3 iii
			jmp createStrictArrIncFoldSt_loop
	}
		
/*
createStrictArrIncFoldSt_loop :: !Int !Int !Int !(Int .st -> (.a, .st)) !*{!.a} !.st -> (!.{!.a.}, !.st)
createStrictArrIncFoldSt_loop frm to step create_element new_array st
	| frm==to
		= (new_array, st)
	# (new_element, st) = create_element frm st
	= loop (frm+step) to step create_element { new_array & [frm] = new_element } st
*/
