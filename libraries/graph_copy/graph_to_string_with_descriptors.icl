implementation module graph_to_string_with_descriptors;

import StdEnv;

import code from "copy_graph_to_string_interface.obj";
import code from "copy_graph_to_string.obj";

eval_all_nodes :: !.a -> .a;
eval_all_nodes g = code {
	push_a 0
	.d 1 0
	jsr	_eval_to_nf
	.o 0 0
}

copy_to_string :: !a -> *{#Char};
copy_to_string g = code {
	.d 1 0
		jsr _copy_graph_to_string
	.o 1 0
}

get_D_from_string :: !{#Char} !Int -> Int;
get_D_from_string s i = code inline {
	push_a_b 0
	pop_a 1
	addI
	load_i 8
}

get_D_arity :: !Int -> Int;
get_D_arity a = code {
	push_b 0
	load_si16 0
	addI
	load_si16 4
}

get_D_node_arity :: !Int -> Int;
get_D_node_arity d = code inline {
	load_si16 -2
}

get_D_record_a_arity :: !Int -> Int;
get_D_record_a_arity d = code inline {
	load_si16 0
}

is_Int_D :: !Int -> Bool;
is_Int_D d = code inline {
	eq_desc_b INT 0
}

is_Char_D :: !Int -> Bool;
is_Char_D d = code inline {
	eq_desc_b CHAR 0
}

is_Real_D :: !Int -> Bool;
is_Real_D d = code inline {
	eq_desc_b REAL 0
}

is_Bool_D :: !Int -> Bool;
is_Bool_D d = code inline {
	eq_desc_b BOOL 0
}

is_Array_D :: !Int -> Bool;
is_Array_D d = code inline {
	eq_desc_b ARRAY 1
}

is__Array__D :: !Int -> Bool;
is__Array__D d = code inline {
	eq_desc_b _ARRAY_ 0
}

is__String__D :: !Int -> Bool;
is__String__D d = code inline {
	eq_desc_b _STRING_ 0
}

is__Nil_D :: !Int -> Bool;
is__Nil_D d = code inline {
	eq_desc_b _Nil 0
}

is__Cons_D :: !Int -> Bool;
is__Cons_D d = code inline {
	eq_desc_b _Cons 2
}

is__Tuple_D :: !Int -> Bool;
is__Tuple_D d = code inline {
	eq_desc_b _Tuple 0
}

get_D_name :: !Int -> {#Char};
get_D_name d = code {
	.d 0 1 i
	jsr DtoAC
	.o 1 0
}

get_D_cons_module :: !Int -> Int;
get_D_cons_module d = code {
	push_b 0
	load_si16 0
	addI
	load_i 6
}

get_D_record_module :: !Int -> Int;
get_D_record_module d = code {
	load_i -10
}

get_module_name_size :: !Int -> Int;
get_module_name_size a = code {
	load_i 0
}

get_module_name_char :: !Int !Int -> Char;
get_module_name_char a i = code {
	addI
	load_ui8 4
}

get_record_type_char :: !Int !Int -> Char;
get_record_type_char a i = code {
	addI
	load_ui8 2
}

get_array_elem_D :: !a !Int -> Int;
get_array_elem_D a offset = code {
	push_a_b 0
	addI
	pop_a 1
	load_i 8
}

get_array_size :: !a !Int -> Int;
get_array_size a offset = code {
	push_a_b 0
	addI
	pop_a 1
	load_i 4
}

get_record_type :: !Int -> {#Char};
get_record_type d
	= {get_record_type_char d i\\i<-[0..get_record_type_size 0 d-1]};
{
	get_record_type_size i d
		# c=get_record_type_char d i;
		| c=='\0'
			= i;
			= get_record_type_size (i+1) d;
}
	
get_module_name :: !Int -> {#Char};
get_module_name m
	= {get_module_name_char m i\\i<-[0..get_module_name_size m-1]};

get_n_non_pointers_and_array_elem_desc :: !Int !a !Int -> (!Int,!Int);
get_n_non_pointers_and_array_elem_desc d v offset
	# arity = get_D_node_arity d;
	| arity==0
		| is_Int_D d || is_Char_D d || is_Bool_D d
			= (1,0);
		| is_Real_D d
			= (2,0);
		| is__Array__D d
			# ed=get_array_elem_D v offset;
			| ed==0
				= (2,ed);
			| is_Int_D ed
				= (2+get_array_size v offset,ed);
			| is_Real_D ed
				= (2+(get_array_size v offset<<1),ed);
			| is_Bool_D ed
				= (2+((get_array_size v offset+3)>>2),ed);
				# arity = get_D_node_arity ed;
				= (2 + get_array_size v offset * (arity-256-get_D_record_a_arity ed),ed);
		| is__String__D d
			= (1+((get_array_size v offset+3)>>2),0);
			= (0,0);
	| arity < 256
		= (0,0);
		= (arity-256-get_D_record_a_arity d,0);

get_module :: !Int -> Int;
get_module d
	# arity = get_D_node_arity d;
	| arity==0
		| is_Int_D d || is_Char_D d || is_Real_D d || is_Bool_D d || is__Array__D d || is__String__D d
			= 0;
		| is__Nil_D d
			= 0;
			= get_D_cons_module d;
	| arity < 256
		| is__Cons_D d
			= 0;
		| is__Tuple_D (d-arity*8)
			= 0;
			= get_D_cons_module d;
		= get_D_record_module d;

uarray :: u:{#.a} -> u:{#.a} | Array {#} a;
uarray a = a;

cast_string_to_a :: !{#Char} -> a;
cast_string_to_a s = code {
	pop_a 0
}

:: DescOrModTree
	= DescOrModTreeNode /*descriptor or module*/!Int /*descriptor_n or module_n*/!Int !DescOrModTree !DescOrModTree
	| EmptyDescOrModTree;

search_desc_or_mod_n_in_tree :: !Int !DescOrModTree -> Int;
search_desc_or_mod_n_in_tree desc_or_mod (DescOrModTreeNode tree_desc_or_mod tree_desc_or_mod_n left_desc_tree right_desc_tree)
	| desc_or_mod==tree_desc_or_mod
		= tree_desc_or_mod_n;
	| desc_or_mod<tree_desc_or_mod
		= search_desc_or_mod_n_in_tree desc_or_mod left_desc_tree;
		= search_desc_or_mod_n_in_tree desc_or_mod right_desc_tree;
search_desc_or_mod_n_in_tree desc_or_mod EmptyDescOrModTree
	= -1;

add_desc_or_mod_to_tree :: !Int !Int !u:DescOrModTree -> u:DescOrModTree;
add_desc_or_mod_to_tree desc desc_n (DescOrModTreeNode descriptor descriptor_n left_desc_tree right_desc_tree)
	| desc==descriptor
		= abort "add_desc_or_mod_to_tree: desc already in tree";
	| desc<descriptor
		= DescOrModTreeNode descriptor descriptor_n (add_desc_or_mod_to_tree desc desc_n left_desc_tree) right_desc_tree;
		= DescOrModTreeNode descriptor descriptor_n left_desc_tree (add_desc_or_mod_to_tree desc desc_n right_desc_tree);
add_desc_or_mod_to_tree desc desc_n EmptyDescOrModTree
	= DescOrModTreeNode desc desc_n EmptyDescOrModTree EmptyDescOrModTree;

store_int_in_string :: !*{#Char} !Int !Int -> *{#Char};
store_int_in_string s i n
	= {s & [i]=toChar n,[i+1]=toChar (n>>8),[i+2]=toChar (n>>16),[i+3]=toChar (n>>24)};

replace_descs_by_desc_numbers_and_build_desc_tree :: !Int !*{#Char} !Int !DescOrModTree -> (!*{#Char},!Int,!DescOrModTree);
replace_descs_by_desc_numbers_and_build_desc_tree i s n_descs desc_tree
	| i>=size s
		= (s,n_descs,desc_tree);
	#! desc=get_D_from_string s i;
	| desc bitand 1<>0
		= replace_descs_by_desc_numbers_and_build_desc_tree (i+4) s n_descs desc_tree;
	| desc bitand 2==0
		= abort ("unevaluated node in replace_descs_by_desc_numbers_and_build_desc_tree "+++toString desc);
	#! a=cast_string_to_a s;
	# (d,array_elem_desc) = get_n_non_pointers_and_array_elem_desc desc a (i+8);
	# (s,n_descs,desc_tree) = store_desc_n_and_add_desc desc i s n_descs desc_tree;
	| array_elem_desc==0
		= replace_descs_by_desc_numbers_and_build_desc_tree (i+4+(d<<2)) s n_descs desc_tree;
		# (s,n_descs,desc_tree) = store_desc_n_and_add_desc array_elem_desc (i+8) s n_descs desc_tree;
		= replace_descs_by_desc_numbers_and_build_desc_tree (i+4+(d<<2)) s n_descs desc_tree;
{}{
	store_desc_n_and_add_desc :: Int Int !*{#Char} !Int !DescOrModTree -> (!*{#Char},!Int,!DescOrModTree);
	store_desc_n_and_add_desc desc i s n_descs desc_tree
		# desc_n=search_desc_or_mod_n_in_tree desc desc_tree;
		| desc_n>=0
			# s=store_int_in_string s i (desc_n+1);  // add 1 because 0 is used as element descriptor for lazy/boxed arrays
			= (s,n_descs,desc_tree);
			# desc_tree = add_desc_or_mod_to_tree desc n_descs desc_tree;
			# s=store_int_in_string s i (n_descs+1); // add 1 because 0 is used as element descriptor for lazy/boxed arrays
			= (s,n_descs+1,desc_tree);			
}

:: Desc_ModuleN = {desc::!Int,desc_mod_n::!Int};

make_desc_array :: !Int !DescOrModTree -> *{#Desc_ModuleN};
make_desc_array n_descs desc_tree
	= fill_desc_array desc_tree (createArray n_descs {desc=0,desc_mod_n=0});
{
	fill_desc_array :: !DescOrModTree !*{#Desc_ModuleN} -> *{#Desc_ModuleN};
	fill_desc_array (DescOrModTreeNode descriptor descriptor_n left_desc_tree right_desc_tree) a
		= fill_desc_array right_desc_tree (fill_desc_array left_desc_tree {a & [descriptor_n].desc=descriptor});
	fill_desc_array EmptyDescOrModTree a
		= a;
}

make_mod_array :: !Int !DescOrModTree -> *{#Int};
make_mod_array n_mods mod_tree
	= fill_desc_array mod_tree (createArray n_mods 0);
{
	fill_desc_array :: !DescOrModTree !*{#Int} -> *{#Int};
	fill_desc_array (DescOrModTreeNode descriptor descriptor_n left_mod_tree right_mod_tree) a
		= fill_desc_array right_mod_tree (fill_desc_array left_mod_tree {a & [descriptor_n]=descriptor});
	fill_desc_array EmptyDescOrModTree a
		= a;
}

make_module_tree :: !*{#Desc_ModuleN} -> (!*{#Desc_ModuleN},!Int,!DescOrModTree);
make_module_tree a
	= add_modules 0 a 0 EmptyDescOrModTree;
{
	add_modules i a n_mods mod_tree
		| i==size a
			= (a,n_mods,mod_tree);
			# (desc,a)=a![i].desc;
			# mod=get_module desc;
			| mod==0
				= add_modules (i+1) a n_mods mod_tree;
			# mod_n=search_desc_or_mod_n_in_tree mod mod_tree;
			| mod_n>=0
				# a = {a & [i].desc_mod_n=mod_n+1};
				= add_modules (i+1) a n_mods mod_tree;
				# mod_tree = add_desc_or_mod_to_tree mod n_mods mod_tree;
				# a = {a & [i].desc_mod_n=n_mods+1};
				= add_modules (i+1) a (n_mods+1) mod_tree;
}

arity_to_char :: !Int -> Char;
arity_to_char a
	| a<0
		= abort "arity_to_charL: incorrect arity";
	| a<10
		= toChar (48+a); // 0..
	| a<36
		= toChar (55+a); // A..
		= abort "arity_to_charL: incorrect arity";

info_of_desc_and_mod :: !Desc_ModuleN -> {#Char};
info_of_desc_and_mod {desc,desc_mod_n}
	# arity = get_D_node_arity desc;
	| arity==0
		| is_Int_D desc
			= "i";
		| is_Char_D desc
			= "c";
		| is_Real_D desc
			= "r";
		| is_Bool_D desc
			= "b";
		| is__Array__D desc
			= "a";
		| is__String__D desc
			= "s";
		| is__Nil_D desc
			= "n";
			= {'C',arity_to_char arity,arity_to_char (get_D_arity desc),toChar (desc_mod_n),toChar (desc_mod_n>>8)}
				+++get_D_name desc+++"\0";
	| arity < 256
		| is__Cons_D desc
			= ":";
		| is__Tuple_D (desc-arity*8)
			= {'t',arity_to_char arity};
		= {'C',arity_to_char arity,arity_to_char (get_D_arity desc),toChar (desc_mod_n),toChar (desc_mod_n>>8)}
			+++get_D_name desc+++"\0";
		= {'R',arity_to_char (arity-256),arity_to_char (get_D_record_a_arity desc),toChar (desc_mod_n),toChar (desc_mod_n>>8)}
			+++get_record_type desc+++"\0"
			+++get_D_name desc+++"\0";

array_of_strings_to_string :: !{#{#Char}} -> {#Char};
array_of_strings_to_string a
	#! n=size a;
	# n_s={toChar n,toChar (n>>8),toChar (n>>16),toChar (n>>24)};
 	= concat_strings [n_s : [s+++"\0"\\s<-:a]];
 {
 	concat_strings [s] = s;
 	concat_strings a = concat_strings (append_pairs a);
 	
 	append_pairs [s1,s2:l] = [s1+++s2:append_pairs l];
 	append_pairs l = l;
 }

graph_to_string_with_descriptor_and_module_table :: !a -> (!{#Char},!{#{#Char}},!{#{#Char}});
graph_to_string_with_descriptor_and_module_table g
	# g = eval_all_nodes g;
	# s = copy_to_string g;
	# (s,n_descs,desc_tree) = replace_descs_by_desc_numbers_and_build_desc_tree 0 s 0 EmptyDescOrModTree;
	# desc_a = make_desc_array n_descs desc_tree;
	# (desc_a,n_mods,mod_tree) = make_module_tree desc_a;
	# mod_a = make_mod_array n_mods mod_tree;
	# mod_s_a = uarray {get_module_name mod \\ mod<-:mod_a};
	# desc_s_a = uarray {info_of_desc_and_mod desc_and_mod \\ desc_and_mod <-:desc_a};
	= (s,desc_s_a,mod_s_a);

graph_to_string_with_descriptors :: !a -> {#Char};
graph_to_string_with_descriptors g
	# (s,desc_s_a,mod_s_a) = graph_to_string_with_descriptor_and_module_table g;
	= array_of_strings_to_string mod_s_a+++array_of_strings_to_string desc_s_a+++s;