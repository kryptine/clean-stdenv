definition module StdGeneric

// embedding-projection
:: Bimap a b = { map_to :: .(a -> b), map_from :: .(b -> a) }
bimapId :: Bimap .a .a

// generic representation
:: UNIT = UNIT
:: EITHER a b = LEFT a | RIGHT b
:: PAIR a b = PAIR a b

// recursion (not yet used)
:: REC a = REC a 			// recursion mark

// for constructor information
:: OBJECT a = OBJECT a		// object marking
:: CONS a = CONS a 			// constructor marking
:: FIELD a = FIELD a 		// record field marking

:: GenericInfo 	= NoGenericInfo	
				| GenericConsInfo GenericConsDescriptor
				| GenericFieldInfo GenericFieldDescriptor
				| GenericTypeDefInfo GenericTypeDefDescriptor

:: GenericConsDescriptor = 
	{ gcd_name 		:: String
	, gcd_arity  	:: Int	
	, gcd_prio 		:: GenConsPrio					// priority and associativity
	, gcd_type_def	:: GenericTypeDefDescriptor  // type def of the constructor
	, gcd_type 		:: GenType						// type of the constructor
	, gcd_fields 	:: [GenericFieldDescriptor] 	// non-empty for records
	, gcd_index 	:: Int							// index of the contructor in the type def
	}
:: GenConsPrio = GenConsNoPrio | GenConsPrio GenConsAssoc Int
:: GenConsAssoc = GenConsAssocNone | GenConsAssocLeft | GenConsAssocRight   	
:: GenericFieldDescriptor = 
	{ gfd_name 	:: String
	, gfd_index :: Int							// index of the field in the record
	, gfd_cons 	:: GenericConsDescriptor 		// the record constructor
	}

:: GenericTypeDefDescriptor =
	{ gtd_name  	:: String 
	, gtd_arity 	:: Int 
	, gtd_num_conses :: Int
	, gtd_conses 	:: [GenericConsDescriptor]
	}

:: GenType 	= GenTypeCons String
			| GenTypeVar String
			| GenTypeApp GenType GenType
			| GenTypeArrow GenType GenType
	
// determine the path in the generic binary-sum-tree of a constructor
:: ConsPos = ConsLeft | ConsRight
getConsPath :: GenericConsDescriptor -> [ConsPos]
	
// generic bidirectional mapping
generic bimap a b :: Bimap .a .b
derive bimap c
derive bimap PAIR
derive bimap EITHER
derive bimap OBJECT
derive bimap CONS
derive bimap FIELD
derive bimap (->)
derive bimap Bimap

// HACK: dictionary for all generics.
// It works since all generic classes have only one method and do not inherit 
// from other classes
:: GenericDict a = { generic_dict :: !a } 			  