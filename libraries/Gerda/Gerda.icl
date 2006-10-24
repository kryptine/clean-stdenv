implementation module Gerda

import StdMisc, StdMaybe, StdGeneric
import StdEnv, odbc, odbccp, StdDebug

TRACE_SQL statement :== statement
//TRACE_SQL statement :== trace_n ("<<<" +++ statement +++ ">>>") statement

:: Gerda = {
		index :: !Int,
		buffer :: !.{#Int},
		layout :: ![Table],
		malloc8 :: !Int,
		connection :: !SQLHDBC,
		environment :: !SQLHENV,
		state :: !.SqlState}
	
:: GerdaFunctions a = GerdaFunctions GerdaType GerdaLayout (GerdaWrite a) (GerdaRead a)

:: GerdaType :== Path History Int -> (GenType, Int)
:: GerdaLayout :== [SqlAttr] String String [Table] Int -> ([Column], [Table], Int)
:: GerdaWrite a :== .(Maybe a) *Gerda -> *Gerda
:: GerdaRead a :== *Gerda -> *(.Maybe a, *Gerda)

:: History = History !Int !String !String !History | NoHistory
:: Path = P !Path !Path | T !Int !String !String

:: Table = {
	name :: !String,
	header :: ![Column],
	key :: !Int,
	bufferSize :: !Int,
	bufferPointer :: !Int,
	insertStmt :: !SQLHSTMT,
	updateStmt :: !SQLHSTMT,
	selectStmt :: !SQLHSTMT}

:: Column = {
	name :: !String,
	sqlType :: !SqlType,
	sqlAttr :: ![SqlAttr]}

:: SqlAttr 
	= SqlReference !String
	| SqlUnique
	| SqlPrimary
	| SqlNull

:: SqlType
	= SqlInteger
	| SqlBit
	| SqlChar1
	| SqlVarChar252
	| SqlDouble

openGerda :: !String !*World -> (!*Gerda, !*World)
openGerda dbname world
	# (state, world) = openSqlState world
	  (r, env, state) = SQLAllocHandle SQL_HANDLE_ENV SQL_NULL_HANDLE state
	| r <> SQL_SUCCESS = abort "SQLAllocHandle SQL_HANDLE_ENV failed"
	# (r, state) = SQLSetEnvAttr env SQL_ATTR_ODBC_VERSION SQL_OV_ODBC2 0 state
	| r <> SQL_SUCCESS = abort "SQLSetEnvAttr failed"
	# (r, dbc, state) = SQLAllocHandle SQL_HANDLE_DBC env state
	| r <> SQL_SUCCESS = abort "SQLAllocHandle SQL_HANDLE_DBC failed"
	# path = "."
	  what = "DSN=MS Access Database;DBQ=" +++ path +++ "\\" +++ dbname +++ ".mdb;DefaultDir=" +++ path +++ ";FIL=MS Access;"
	  (r, _, _, state) = SQLDriverConnect dbc 0 what (size what) 0 SQL_DRIVER_NOPROMPT state
	  state = case r of
	  	SQL_SUCCESS -> state
	  	_
			# (r, state) = SQLConfigDataSource 0 ODBC_ADD_DSN "Microsoft Access Driver (*.mdb)\0" ("CREATE_DB=\"" +++ path +++ "\\" +++ dbname +++ ".mdb\" General\0\0") state
		 	| r <> 1 -> abort "SQLConfigDataSource failed"
	  		# (r, _, _, state) = SQLDriverConnect dbc 0 what (size what) 0 SQL_DRIVER_NOPROMPT state
		 	| r <> SQL_SUCCESS -> abort "SQLDriverConnect failed"
	  		-> state
	# (p, state) = LocalAlloc 0 8 state
	| p == 0 = abort "LocalAlloc failed"
	= ({index = 0, buffer = {}, layout = [], malloc8 = p, 
		connection = dbc, environment = env, state = state}, world)

closeGerda :: !*Gerda !*World -> *World
closeGerda g=:{layout} world 
	# g = foldr closeTable g layout
	  {layout, malloc8, connection, environment, state} = g
	  (r, state) = LocalFree malloc8 state
	| r <> 0 = abort "LocalFree failed"
	# (r, state) = SQLDisconnect connection state;
	| r <> SQL_SUCCESS = abort "SQLDisconnect failed"
	# (r, state) = SQLFreeHandle SQL_HANDLE_DBC connection state
	| r <> SQL_SUCCESS = abort "SQLFreeHandle SQL_HANDLE_DBC failed"
	# (r, state) = SQLFreeHandle SQL_HANDLE_ENV environment state
	| r <> SQL_SUCCESS = abort "SQLFreeHandle SQL_HANDLE_DBC failed"
	= closeSqlState state world

writeGerda :: !String !a !*Gerda -> *Gerda | gerda{|*|} a
writeGerda name x g
//	| trace_tn (fst (typeA (T 0 "" "") NoHistory 0)) = abort "THIS IS ONLY THE TYPE"
	# g = removeTable name g
	  (layout, g) = g!layout
	  tables = layoutTable tableName layoutA layout
	  (key, g) = writeToTable tableName tables writeA x g
	= g
where
	(GerdaFunctions typeA layoutA writeA _) = gerda{|*|}
	tableName = "*" +++ name

readGerda :: !String !*Gerda -> (!Maybe a, !*Gerda) | gerda{|*|} a
readGerda name g=:{layout} = readFromTable tableName tables readA 0 g
where
	(GerdaFunctions _ layoutA _ readA) = gerda{|*|}
	tables = layoutTable tableName layoutA layout
	tableName = "*" +++ name

generic gerda a :: GerdaFunctions a

gerda{|OBJECT of {gtd_name, gtd_arity, gtd_num_conses}|} gerdaA
	= GerdaFunctions typeO layoutO writeO readO
where
	(GerdaFunctions typeA layoutA writeA readA) = gerdaA

	(GerdaFunctions _ layoutR writeR readR) = gerdaInt [SqlReference tableName]

	typeO path=:(T arity con typ) history i
		| gtd_arity == 0 = (GenTypeCons gtd_name, i)
		| lookup history > arity = (t, j)
		= (realtype, k)
	where
		realtype = unify (repeatn gtd_num_conses t) ts t
	
		ts = splitEithers gtd_num_conses ta
		where
			splitEithers 1 t = [t]
			splitEithers n (GenTypeApp (GenTypeApp (GenTypeCons "EITHER") a) b) 
				= splitEithers (n >> 1) a ++ splitEithers ((n + 1) >> 1) b
	
		(ta, k) = typeA path history j
	
		(t, j) = makeType gtd_arity (GenTypeCons gtd_name) i
		where
			makeType 0 t i = (t, i)
			makeType n t i = makeType (n - 1) (GenTypeApp t (GenTypeVar (toString i))) (i + 1)
	
		lookup (History n c t hs)
			| c == con && t == typ = n
			| otherwise = lookup hs
		lookup _ = 0
	
	tableName = type2tableName type
	where
		(type, _) = typeO (T 0 "" "") NoHistory 0

	layoutO attr constr field tables i = (columns, tables``, j)
	where
		(columns, tables`, j) = layoutR attr constr field tables i
		tables`` = layoutTable tableName layoutA tables`
	
	writeO (Just (OBJECT x)) g=:{layout}
		# (key, g) = writeToTable tableName [] writeA x g
		= writeR (Just key) g
	writeO _ g = writeR Nothing g

	readO g = case readR g of
					(Just ref, g) -> unsafeRead readA ref g 
					(_, g) -> (Nothing, g)
	where
		unsafeRead :: !(GerdaRead a) !Int !*Gerda -> (Maybe (OBJECT a), !*Gerda)
		unsafeRead readA ref g=:{layout, malloc8, connection, environment}
			# (state, _) = openSqlState (cast 0x9E5DA)
			  g` = {index = 0, buffer = {}, layout = layout, malloc8 = malloc8, 
			  		connection = connection, environment = environment, state = state}
			  (m, _) = readFromTable tableName [] readA ref g`
			= (mapMaybe OBJECT m, g)

gerda{|OBJECT|} gerdaA = gerdaBimap (GenTypeApp (GenTypeCons "OBJECT")) [] (\(OBJECT x) -> x) OBJECT gerdaA

gerda{|EITHER|} gerdaA gerdaB = GerdaFunctions typeE layoutP writeE readE
where
	(GerdaFunctions typeP layoutP writeP readP) = gerda{|*->*->*|} (gerda{|*->*|} gerdaA) (gerda{|*->*|} gerdaB)

	typeE path history i = (GenTypeApp (GenTypeApp (GenTypeCons "EITHER") ta) tb, j)
	where
		(GenTypeApp (GenTypeApp (GenTypeCons _) (GenTypeApp _ ta)) (GenTypeApp _ tb), j) = typeP (P path path) history i
			
	writeE = mapWrite writeP either2pair
	where
		either2pair (LEFT x) = PAIR (Just x) Nothing
		either2pair (RIGHT y) = PAIR Nothing (Just y)
	
	readE = mapRead readP pair2either
	where
		pair2either (PAIR (Just x) _) = LEFT x
		pair2either (PAIR _ (Just y)) = RIGHT y

gerda{|CONS of {gcd_name, gcd_arity, gcd_type, gcd_type_def}|} gerdaA
	= GerdaFunctions typeC layoutC writeC readC
where
	(GerdaFunctions typeA layoutA writeA readA) = gerdaA
	
	{gtd_name, gtd_arity} = gcd_type_def

	typeC path history i 
		| gcd_arity == 0 = (t, j)
		= (unify formals actuals result, k)
	where
		actuals = splitPairs gcd_arity type
		where
			splitPairs 1 t = [t]
			splitPairs n (GenTypeApp (GenTypeApp (GenTypeCons "PAIR") a) b) 
				= splitPairs (n >> 1) a ++ splitPairs ((n + 1) >> 1) b
	
		(type, k) = typeA cons history` j
	
		(cons, _) = makePairs gcd_arity formals
		where
			makePairs 1 [f:fs] 
				| varApp = (T gtd_arity gcd_name gtd_name, fs)
				= (T gtd_arity gcd_name typecons, fs)
			where
				(varApp, typecons) = typeCons f False
			makePairs n fs 
				# (a, fs) = makePairs (n >> 1) fs
				  (b, fs) = makePairs ((n + 1) >> 1) fs
				= (P a b, fs) 
	
			typeCons (GenTypeApp x y) _ = typeCons x True
			typeCons (GenTypeCons x) _ = (False, x)
			typeCons (GenTypeVar x) varApp = (varApp, "")
	
		history` = foldl increment history (typeConses gcd_type [])
		where
			typeConses (GenTypeApp x y) acc = typeConses y (typeConses x acc)
			typeConses (GenTypeArrow x y) acc = typeConses y (typeConses x acc)
			typeConses (GenTypeCons x) acc | not (isMember y acc) = [y:acc]
				where y = (gcd_name, x)
			typeConses _ acc = acc
	
			increment (History n c t hs) (con, typ)
				| c == con && t == typ = History (n + 1) c t hs
				| otherwise = History n c t (increment hs (con, typ))
			increment _ (con, typ) = History 1 con typ NoHistory
			
		(formals, result) = splitType gcd_arity t
		where
			splitType 0 t = ([], t)
			splitType n (GenTypeArrow x y) = ([x:xs], r)
				where (xs, r) = splitType (n - 1) y
				
		(t, j) = freshCopy gcd_type i

	layoutC attr _ field tables i = layoutA attr gcd_name field tables i
	
	writeC = mapWrite writeA \(CONS x) -> x

	readC = mapRead readA CONS

gerda{|CONS|} gerdaA = gerdaBimap (GenTypeApp (GenTypeCons "CONS")) [] (\(CONS x) -> x) CONS gerdaA

gerda{|FIELD of {gfd_name}|} gerdaA = GerdaFunctions typeA layoutF writeF readF
where
	(GerdaFunctions typeA layoutA writeA readA) = gerdaA
	
	layoutF attr _ _ tables i = layoutA attr "" gfd_name tables i
	
	writeF = mapWrite writeA \(FIELD x) -> x

	readF = mapRead readA FIELD

gerda{|FIELD|} gerdaA = gerdaBimap (GenTypeApp (GenTypeCons "FIELD")) [] (\(FIELD x) -> x) FIELD gerdaA

gerda{|PAIR|} gerdaA gerdaB = GerdaFunctions typeP layoutP writeP readP
where
	(GerdaFunctions typeA layoutA writeA readA) = gerdaA
	(GerdaFunctions typeB layoutB writeB readB) = gerdaB
	
	typeP path history i = (GenTypeApp (GenTypeApp (GenTypeCons "PAIR") a) b, k)
	where
		(a, j) = typeA pa history i
		(b, k) = typeB pb history j
		(pa, pb) = case path of 
						P l r -> (l, r)
						_ -> (path, path)

	layoutP attr constr field tables i = (columns ++ columns`, tables``, k)
	where
		(columns, tables`, j) = layoutA attr constr field tables i
		(columns`, tables``, k) = layoutB attr constr "" tables` j

	writeP (Just (PAIR x y)) g
		# g = writeA (Just x) g
		= writeB (Just y) g
	writeP _ g
		# g = writeA Nothing g
		= writeB Nothing g

	readP g
		# (ma, g) = readA g
		  (mb, g) = readB g
		= (case (ma, mb) of (Just x, Just y) -> Just (PAIR x y); _ -> Nothing, g)

gerda{|UNIT|} =: GerdaFunctions typeU layoutU writeU readU
where
	(GerdaFunctions _ layoutB writeB readB) = gerda{|*|}
	
	typeU _ _ i = (GenTypeCons "UNIT", i)
	
	layoutU attr constr field tables i = layoutB (removeMember SqlNull attr) constr field tables i

	writeU m g = writeB (Just (isJust m)) g

	readU g 
		# (Just b, g) = readB g 
		= if b (Just UNIT, g) (Nothing, g)
		
gerda{|Int|} =: gerdaInt []

gerda{|Bool|} =: gerdaInline (GenTypeCons "Bool") SqlBit [] 2 store load
where
	store x index buffer = {buffer & [index] = 1, [index + 1] = if x -1 0}
	load 1 index buffer 
		# (x, buffer) = buffer![index + 1]
		= (x bitand 1 <> 0, buffer)
	
gerda{|Char|} =: gerdaInline (GenTypeCons "Char") SqlChar1 [] 2 store load
where
	store x index buffer = {buffer & [index] = 1, [index + 1] = toInt x}
	load 1 index buffer 
		# (x, buffer) = buffer![index + 1]
		= (toChar x, buffer)

gerda{|Real|} =: gerdaInline (GenTypeCons "Real") SqlDouble [] 3 store load
where
	store x index buffer 
		# (i1, i2) = real2ints x
		= {buffer & [index] = 8, [index + 1] = i1, [index + 2] = i2}
	where
		real2ints :: !Real -> (!Int, !Int)
		real2ints _ = code {
				pop_a	0
			}

	load 8 index buffer 
		# (i1, buffer) = buffer![index + 1]
		  (i2, buffer) = buffer![index + 2]
		= (ints2real i1 i2, buffer)
	where	
		ints2real :: !Int !Int -> Real
		ints2real _ _ = code {
				pop_a 0
			}

gerda{|Binary252|} =: gerdaInline (GenTypeCons "Binary252") SqlVarChar252 [] 64 store load
where
	store {binary252} index buffer = storeString 0 binary252 (index + 1) {buffer & [index] = size binary252}
	where
		storeString :: !Int !String !Int !*{#Int} -> *{#Int}
		storeString i s j d = case size s - i of
			1 -> {d & [j] = toInt s.[i]}
			2 -> {d & [j] = toInt s.[i] + toInt s.[i + 1] << 8}
			3 -> {d & [j] = toInt s.[i] + toInt s.[i + 1] << 8 + toInt s.[i + 2] << 16}
			x | x <= 0 -> d
			_ -> storeString (i + 4) s (j + 1) {d & [j] = toInt s.[i] + toInt s.[i + 1] << 8 + toInt s.[i + 2] << 16 + toInt s.[i + 3] << 24}
	
	load avail index buffer | 0 <= avail && avail <= 252
		# (s, buffer) = loadString 0 (createArray avail '\0') (index + 1) buffer
		= ({binary252 = s}, buffer)
	where
		loadString :: !Int !*String !Int !u:{#Int} -> (!*String, !u:{#Int})
		loadString i d j s 
			# (e, s) = s![j]
			= case size d - i of
				1 -> ({d & [i] = toChar e}, s)
				2 -> ({d & [i] = toChar e, [i + 1] = toChar (e >> 8)}, s)
				3 -> ({d & [i] = toChar e, [i + 1] = toChar (e >> 8), [i + 2] = toChar (e >> 16)}, s)
				x | x <= 0 -> (d, s)
				_ ->  loadString (i + 4) {d & [i] = toChar e, [i + 1] = toChar (e >> 8), [i + 2] = toChar (e >> 16), [i + 3] = toChar (e >> 24)} (j + 1) s

gerda{|Maybe|} gerdaA = GerdaFunctions typeM layoutM writeM readM
where
	(GerdaFunctions typeA layoutA writeA readA) = gerdaA
	
	typeM path history i = (GenTypeApp (GenTypeCons "Maybe") a, j)
	where
		(a, j) = typeA path history i

	layoutM attr constr field tables i = layoutA (if (isMember SqlNull attr) attr [SqlNull:attr]) constr field tables i

	writeM (Just x=:(Just _)) g = writeA x g
	writeM _ g = writeA Nothing g
	
	readM g 
		# (m, g) = readA g
		= (Just m, g)

gerda{|String|} =: gerdaBimap typeS [] fromS toS (gerda{|*->*|} gerda{|*|})
where
	typeS _ = GenTypeCons "_String"
	
	fromS s = CompactList {binary252 = s % (0, 251)} (f (s % (252, 1 << 30)))
	where
		f s
			| size s <= 0 = Nothing
			= Just (CompactList {binary252 = s % (0, 251)} (f (s % (252, 1 << 30))))

	toS (CompactList {binary252} tail) = binary252 +++ f tail
	where
		f (Just (CompactList {binary252} tail)) = binary252 +++ f tail
		f _ = ""

gerda{|[]|} gerdaA = gerdaBimap typeL [] toMCL fromMCL (gerda{|*->*|} (gerda{|*->*|} gerdaA))
where
	typeL (GenTypeApp _ (GenTypeApp _ a)) = GenTypeApp (GenTypeCons "_List") a

	toMCL [x:xs] = Just (CompactList x (toMCL xs))
	toMCL _ = Nothing
	
	fromMCL (Just (CompactList x xs)) = [x:fromMCL xs]
	fromMCL _ = []

gerda{|{}|} gerdaA = gerdaArray (GenTypeCons "{}") gerdaA

gerda{|{!}|} gerdaA = gerdaArray (GenTypeCons "{!}") gerdaA

gerda{|GerdaObject|} gerdaA = GerdaFunctions typeG layoutG writeG readG
where
	(GerdaFunctions typeA layoutA writeA readA) = gerdaA

	(GerdaFunctions _ layoutR writeR readR) = gerdaInt [SqlReference tableName]

	typeG path history i = (GenTypeApp (GenTypeCons "GerdaObject") a, j)
	where
		(a, j) = typeA path history i
	
	tableName = type2tableName type
	where
		(type, _) = typeG (T 0 "" "") NoHistory 0

	layoutG attr constr field tables i = (columnsR, tables``, j)
	where
		(columnsR, tables`, j) = layoutR attr constr field tables i
		tables`` = layoutTable tableName layoutA tables`

	writeG (Just {gerdaValue}) g=:{layout}
		# (key, g) = writeToTable tableName [] writeA gerdaValue g
		= writeR (Just key) g
	writeG _ g = writeR Nothing g

	readG g = case readR g of
				(Just ref, g) -> case readFromTable tableName [] readA ref g of
									(Just x, g) -> (Just {gerdaValue = x, 
														gerdaWrite = updateInTable tableName [] writeA ref, 
														gerdaRead = \g -> case readFromTable tableName [] readA ref g of (Just x, g) -> (x, g)}, g)
									(_, g) -> (Nothing, g)
				(_, g) -> (Nothing, g)
			
gerdaInt :: ![SqlAttr] -> GerdaFunctions Int
gerdaInt sqlAttr = gerdaInline (GenTypeCons "Int") SqlInteger sqlAttr 2 store load
where
	store x index buffer = {buffer & [index] = 4, [index + 1] = x}
	load 4 index buffer = buffer![index + 1]

gerdaBimap :: !(GenType -> GenType) ![SqlAttr] !(b -> a) !(a -> b) !(GerdaFunctions a) -> GerdaFunctions b
gerdaBimap typeF sqlAttr toF fromF gerda = GerdaFunctions typeB layoutB writeB readB
where
	(GerdaFunctions typeA layoutA writeA readA) = gerda
	
	typeB path history i = (typeF type, j)
	where
		(type, j) = typeA path history i

	layoutB attr constr field tables i = layoutA (removeDup (sqlAttr ++ attr)) constr field tables i

	writeB = mapWrite writeA toF

	readB = mapRead readA fromF

gerdaInline genType sqlType sqlAttr dataSize store load :== GerdaFunctions typeI layoutI writeI readI
where
	typeI _ _ i = (genType, i)

	layoutI attr constr field tables i = ([{name = n, sqlType = sqlType, sqlAttr = removeDup (sqlAttr ++ attr)}], tables, i + 1)
	where
		n = if (field == "") (toString i +++ constr) field

	writeI m g=:{index, buffer} = case m of
		Just x -> {g & index = index + dataSize, buffer = store x index buffer}
		_ -> {g & index = index + dataSize, buffer = {buffer & [index] = SQL_NULL_DATA}}

	readI g=:{index, buffer}
		# (avail, buffer) = buffer![index]
		| avail == SQL_NULL_DATA = (Nothing, {g & index = index + dataSize, buffer = buffer})
		# (x, buffer) = load avail index buffer 
		#!x = x
		= (Just x, {g & index = index + dataSize, buffer = buffer})
	
gerdaArray type gerdaA :== GerdaFunctions typeY layoutY writeY readY
where
	(GerdaFunctions typeA layoutA writeA readA) = gerdaA

	(GerdaFunctions _ layoutR writeR readR) = gerdaInt [SqlReference tableName]

	(GerdaFunctions _ layoutS writeS readS) = gerdaInt []

	typeY path history i = (GenTypeApp type a, j)
	where
		(a, j) = typeA path history i
	
	tableName = type2tableName type
	where
		(type, _) = typeY (T 0 "" "") NoHistory 0

	layoutY attr constr field tables i = (columnsR ++ columnsS, tables```, k)
	where
		(columnsR, tables`, j) = layoutR attr constr field tables i
		(columnsS, tables``, k) = layoutS attr constr "" tables j
		tables``` = layoutTable tableName layoutA tables``

	writeY (Just array) g=:{layout}
		# (lastkey, g) = writeArray 0 -1 g
		  g = writeR (Just (lastkey - len + 1)) g
		= writeS (Just len) g
	where
		len = size array
		writeArray i key g
			| i >= len = (key, g)
			# (k, g) = writeToTable tableName [] writeA array.[i] g
			= writeArray (i + 1) k g
	writeY _ g = writeR Nothing g

	readY g = case readR g of
				(Just ref, g) -> case readS g of
									(Just len, g) -> readArray ref 0 (createArray len (cast [])) g
									(_, g) -> (Nothing, g)
				(_, g) -> (Nothing, g)
	where
		readArray ref i array g
			| i >= size array = (Just array, g)
			= case readFromTable tableName [] readA (ref + i) g of
				(Just x, g) -> readArray ref (i + 1) {array & [i] = x} g
				(_, g) -> (Nothing, g)
			
mapWrite write f m g :== case m of 
	Just x -> write (Just (f x)) g
	_ -> write Nothing g

mapRead read f g :== case read g of (m, g) -> (mapMaybe f m, g)

layoutTable :: !String !GerdaLayout ![Table] -> [Table]
layoutTable tableName layoutA layout
	| isJust (fst (extractTable tableName layout)) = layout
	= [table:tables]
where
    table = {name = tableName, header = key ++ header, key = 0, 
    		bufferSize = 0, bufferPointer = 0, insertStmt = 0, updateStmt = 0, selectStmt = 0}
	(header, tables, _) = layoutA [] "" "" tables` k
	(key, tables`, k) = layoutK [] "" "K" [table:layout] 0

	(GerdaFunctions _ layoutK _ _) = gerdaInt [SqlPrimary]

removeTable :: !String !*Gerda -> *Gerda
removeTable name g=:{layout, connection, state}
	# (r, h, state) = SQLAllocHandle SQL_HANDLE_STMT connection state
	| r <> SQL_SUCCESS = abort "SQLAllocHandle SQL_HANDLE_STMT failed"
	# drop = "DROP TABLE " +++ sqlEscape tableName
	  (r, state) = SQLExecDirect h (TRACE_SQL drop) (size drop) state
//	| r <> SQL_SUCCESS = abort ("SQLExecDirect failed " +++ drop)
	# (r, state) = SQLFreeHandle SQL_HANDLE_STMT h state
	| r <> SQL_SUCCESS = abort "SQLFreeHandle SQL_HANDLE_STMT failed"
	# (m, layout) = extractTable tableName layout
	  g = {g & layout = layout, state = state}
	= case m of
		Just table -> closeTable table g
		_ -> g
where
	tableName = "*" +++ name

updateInTable :: !String ![Table] !(GerdaWrite a) !Int !a !*Gerda -> *Gerda
updateInTable tableName tables write key x g
	# g=:{layout, index=previousIndex, buffer=previousBuffer} = openTables tables g
	  (m, layout) = extractTable tableName layout
	| isNothing m = abort ("updateInTable cannot find " +++ tableName +++ " (internal error)")
	# (Just t=:{header, bufferSize, bufferPointer, updateStmt}) = m
	  g=:{buffer, connection, state} = write (Just x) {g & index = 2, buffer = {createArray bufferSize 0 & [0] = 4, [1] = key}}
	  state = copy 0 bufferPointer buffer state
	  update = "UPDATE " +++ sqlEscape tableName +++ " SET " +++ separatorList "," [name +++ "=?"\\ {name, sqlType} <- tl header] +++ " WHERE K=" +++ toString key
	  (r, state) = SQLPrepare updateStmt (TRACE_SQL update) (size update) state
	| r <> SQL_SUCCESS = abort ("SQLPrepare failed " +++ update)
	# state = bindParameters (tl header) 1 (bufferPointer + 8) updateStmt state
	  (r, state) = SQLExecute updateStmt state
	| r <> SQL_SUCCESS = abort ("SQLExecDirect failed " +++ toString r)
	= {g & index = previousIndex, buffer = previousBuffer, state = state}
where
	copy :: !Int !Int !{#Int} !*st -> *st
	copy i ptr buffer state
		| i >= size buffer = state
		# state = poke ptr buffer.[i] state
		= copy (i + 1) (ptr + 4) buffer state

	bindParameters [{sqlType}:cs] i p h state
		# (r, state) = SQLBindParameter h i SQL_PARAM_INPUT c_type sql_type (len * 4 - 4) 0 (p + 4) 0 p state
		| r <> SQL_SUCCESS = abort "SQLBindParameter failed"
		= bindParameters cs (i + 1) (p + len * 4) h state
	where
		(len, c_type, sql_type, _) = sqlTypeInfo sqlType
	bindParameters _ _ _ _ state = state

writeToTable :: !String ![Table] !(GerdaWrite a) !a !*Gerda -> (!Int, !*Gerda)
writeToTable tableName tables write x g
	# g=:{layout, index=previousIndex, buffer=previousBuffer} = openTables tables g
	  (m, layout) = extractTable tableName layout
	| isNothing m = abort ("writeToTable cannot find " +++ tableName +++ " (internal error)")
	# (Just t=:{header, key, bufferSize, bufferPointer, insertStmt}) = m
	  g=:{buffer, connection, state} = write (Just x) {g & index = 2, buffer = {createArray bufferSize 0 & [0] = 4, [1] = key}, layout = [{t & key = key + 1}:layout]}
	  state = copy 0 bufferPointer buffer state
	  (r, state) = SQLExecute insertStmt state
	| r <> SQL_SUCCESS = abort ("SQLExecute failed " +++ toString r)
	= (key, {g & index = previousIndex, buffer = previousBuffer, state = state})
where
	copy :: !Int !Int !{#Int} !*st -> *st
	copy i ptr buffer state
		| i >= size buffer = state
		# state = poke ptr buffer.[i] state
		= copy (i + 1) (ptr + 4) buffer state

readFromTable :: !String ![Table] !(GerdaRead a) !Int !*Gerda -> (!Maybe a, !*Gerda)
readFromTable tableName tables read key g
	# g=:{index=previousIndex, buffer=previousBuffer, layout, connection, state} = openTables tables g
	  (m, _) = extractTable tableName layout
	| isNothing m = abort ("readFromTable cannot find " +++ tableName +++ " (internal error)")
	# (Just {header, bufferSize, bufferPointer, selectStmt}) = m
	# select = "SELECT * FROM " +++ sqlEscape tableName +++ " WHERE K=" +++ toString key
	  (r, state) = SQLExecDirect selectStmt (TRACE_SQL select) (size select) state
	| r <> SQL_SUCCESS 
		# (r, state) = SQLFreeStmt selectStmt SQL_CLOSE state
		| r <> SQL_SUCCESS = abort "SQLFreeStmt failed"
		= (Nothing, {g & state = state})
	# (r, state) = SQLFetch selectStmt state
	| r <> SQL_SUCCESS
		# (r, state) = SQLFreeStmt selectStmt SQL_CLOSE state
		| r <> SQL_SUCCESS = abort "SQLFreeStmt failed"
		= (Nothing, {g & state = state})
	# (buffer, state) = copy 0 bufferPointer (createArray bufferSize 0) state
	  (r, state) = SQLFreeStmt selectStmt SQL_CLOSE state
	| r <> SQL_SUCCESS = abort "SQLFreeStmt failed"
	# (m, g) = read {g & index = 2, buffer = buffer, state = state}
	= (m, {g & index = previousIndex, buffer = previousBuffer})
where
	copy :: !Int !Int !*{#Int} !*st -> (!*{#Int}, !*st)
	copy i ptr buffer state
		| i >= size buffer = (buffer, state)
		# (x, state) = peek ptr state
		= copy (i + 1) (ptr + 4) {buffer & [i] = x} state

extractTable :: !String ![Table] -> (!Maybe Table, ![Table])
extractTable tableName [t=:{name, header}:ts]
	| name == tableName = (Just t, ts)
	# (maybe, ts) = extractTable tableName ts
	= (maybe, [t:ts])
extractTable _ _ = (Nothing, [])

closeTable :: Table !*Gerda -> *Gerda
closeTable {bufferPointer, insertStmt, selectStmt} g=:{state}
	# (r, state) = SQLFreeHandle SQL_HANDLE_STMT insertStmt state
	| r <> SQL_SUCCESS = abort "SQLFreeHandle SQL_HANDLE_STMT failed"
	# (r, state) = SQLFreeHandle SQL_HANDLE_STMT selectStmt state
	| r <> SQL_SUCCESS = abort "SQLFreeHandle SQL_HANDLE_STMT failed"
	# (r, state) = LocalFree bufferPointer state
	| r <> 0 = abort "LocalFree failed"
	= {g & state = state}

openTables :: ![Table] !*Gerda -> *Gerda
openTables [t=:{name, header}:ts] g=:{layout, malloc8, connection, state} 
	| isJust (fst (extractTable name layout)) = openTables ts g
	# (key, bufferSize, state) = createTableIfNotExists name header malloc8 state
	  (bufferPointer, state) = LocalAlloc 0 (bufferSize * 4) state
	| bufferPointer == 0 = abort "LocalAlloc failed"

	# (r, insertStmt, state) = SQLAllocHandle SQL_HANDLE_STMT connection state
	| r <> SQL_SUCCESS = abort "SQLAllocHandle SQL_HANDLE_STMT failed"
	# insert = "INSERT INTO " +++ sqlEscape name +++ " VALUES (" +++ separatorList "," (map (const "?") header) +++ ")"
	  (r, state) = SQLPrepare insertStmt (TRACE_SQL insert) (size insert) state
	| r <> SQL_SUCCESS = abort ("SQLPrepare failed " +++ insert)
	# state = bindParameters header 1 bufferPointer insertStmt state

	# (r, updateStmt, state) = SQLAllocHandle SQL_HANDLE_STMT connection state
	| r <> SQL_SUCCESS = abort "SQLAllocHandle SQL_HANDLE_STMT failed"

	# (r, selectStmt, state) = SQLAllocHandle SQL_HANDLE_STMT connection state
	| r <> SQL_SUCCESS = abort "SQLAllocHandle SQL_HANDLE_STMT failed"
	# state = bindCols header 1 bufferPointer selectStmt state

	# t = {t & key = key, bufferSize = bufferSize, bufferPointer = bufferPointer, 
	  		insertStmt = insertStmt, updateStmt = updateStmt, selectStmt = selectStmt}
	= openTables ts {g & layout = [t:layout], state = state}
where
	bindParameters [{sqlType}:cs] i p h state
		# (r, state) = SQLBindParameter h i SQL_PARAM_INPUT c_type sql_type (len * 4 - 4) 0 (p + 4) 0 p state
		| r <> SQL_SUCCESS = abort "SQLBindParameter failed"
		= bindParameters cs (i + 1) (p + len * 4) h state
	where
		(len, c_type, sql_type, _) = sqlTypeInfo sqlType
	bindParameters _ _ _ _ state = state

	bindCols [{sqlType}:cs] i p h state
		# (r, state) = SQLBindCol h i c_type (p + 4) (len * 4 - 4) p state
		| r <> SQL_SUCCESS = abort "SQLBindCol failed"
		= bindCols cs (i + 1) (p + len * 4) h state
	where
		(len, c_type, _, _) = sqlTypeInfo sqlType
	bindCols _ _ _ _ state = state

	createTableIfNotExists name header p state
		# (r, h, state) = SQLAllocHandle SQL_HANDLE_STMT connection state
		| r <> SQL_SUCCESS = abort "SQLAllocHandle SQL_HANDLE_STMT failed"
		# select = "SELECT MAX(K) FROM " +++ sqlEscape name
		  (r, state) = SQLExecDirect h (TRACE_SQL select) (size select) state
		| r == SQL_SUCCESS
			# (r, state) = SQLBindCol h 1 SQL_C_SLONG (p + 4) 4 p state
			| r <> SQL_SUCCESS = abort "SQLBindCol failed"
			# (r, state) = SQLFetch h state
			| r <> SQL_SUCCESS = abort "SQLFetch failed"
			# (r, state) = SQLFreeStmt h SQL_DROP state
			| r <> SQL_SUCCESS = abort "SQLFreeStmt failed"
			# (avail, state) = peek p state
			| avail == SQL_NULL_DATA = (0, len, state)
			| avail <> 4 = abort "SQLFetch returned invalid data"
			# (key, state) = peek (p + 4) state
			| key < 0 = abort "Table has invalid MAX(K)"
			= (key + 1, len, state)
		# create = "CREATE TABLE " +++ sqlEscape name +++ " (" +++ separatorList "," (cols ++ attrs) +++ ")"
		  (r, state) = SQLExecDirect h (TRACE_SQL create) (size create) state
		| r <> SQL_SUCCESS = abort ("SQLExecDirect failed: " +++ create)
		# (r, state) = SQLFreeStmt h SQL_DROP state
		| r <> SQL_SUCCESS = abort "SQLFreeStmt failed"
		= (0, len, state)
	where
		(cols, attrs, len) = f header
		where
			f [column=:{name, sqlType, sqlAttr}:columns] 
				# (cols, attrs, lens) = f columns
				= ([foldl (\x y -> x +++ " " +++ y) (sqlEscape name +++ " " +++ sql_descr) attr1:cols], attr2 ++ attrs, lens + len)
			where
				(attr1, attr2) = sql2attr sqlAttr
				(len, _, _, sql_descr) = sqlTypeInfo sqlType
				sql2attr [SqlUnique:as]
					# (attr1, attr2) = sql2attr as
					= (attr1 ++ ["UNIQUE"], attr2)
				sql2attr [SqlPrimary:as]
					# (attr1, attr2) = sql2attr as
					= (attr1 ++ ["PRIMARY KEY"], attr2)
				sql2attr [SqlReference t:as]
					# (attr1, attr2) = sql2attr as
					= (attr1/* ++ ["REFERENCES " +++ sqlEscape t]*/, attr2)
				sql2attr [SqlNull:as]
					# (attr1, attr2) = sql2attr as
					= (["NULL":filter ((<>) "NOT NULL") attr1], attr2)
				sql2attr _ = (["NOT NULL"], [])
			f _ = ([], [], 0)
openTables _ g = g

freshCopy :: !GenType !Int -> (!GenType, !Int)
freshCopy t fresh = (s t, fresh`)
where
	(s, fresh`) = makeSubst t fresh
	
	makeSubst (GenTypeVar x) fresh = (subst x (GenTypeVar (toString fresh)), fresh + 1)
	makeSubst (GenTypeApp x y) fresh = (s2 o s1, fresh``)
	where 
		(s1, fresh`) = makeSubst x fresh
		(s2, fresh``) = makeSubst y fresh`
	makeSubst (GenTypeArrow x y) fresh = (s2 o s1, fresh``)
	where 
		(s1, fresh`) = makeSubst x fresh
		(s2, fresh``) = makeSubst y fresh`
	makeSubst _ fresh = (id, fresh)

unify :: ![GenType] ![GenType] -> GenType -> GenType
unify [GenTypeVar x:xs] [GenTypeVar y:ys] | x == y = unify xs ys
unify [GenTypeVar x:xs] [y:ys] 
/*	| not (occurs x y)*/ = unify (map s xs) (map s ys) o s where s = subst x y
unify [x:xs] [GenTypeVar y:ys] 
/*	| not (occurs y x)*/ = unify (map s xs) (map s ys) o s where s = subst y x
unify [GenTypeApp x1 x2:xs] [GenTypeApp y1 y2:ys] 
	= unify [x1, x2:xs] [y1, y2:ys]
unify [GenTypeArrow x1 x2:xs] [GenTypeArrow y1 y2:ys] 
	= unify [x1, x2:xs] [y1, y2:ys]
unify [GenTypeCons x:xs] [GenTypeCons y:ys] | x == y = unify xs ys
unify [] [] = id
unify xs ys = abort ("Cannot unify " +++ separatorList "," xs +++ " and " +++ separatorList "," ys)
/*
occurs :: !String !GenType -> Bool
occurs v (GenTypeVar x) = v == x
occurs v (GenTypeCons x) = v == x
occurs v (GenTypeApp x y) = occurs v x || occurs v y
occurs v (GenTypeArrow x y) = occurs v x || occurs v y
*/
subst :: !String !GenType !GenType -> GenType
subst x y (GenTypeApp a b) = GenTypeApp (subst x y a) (subst x y b)
subst x y (GenTypeArrow a b) = GenTypeArrow (subst x y a) (subst x y b)
subst x y (GenTypeVar t) | t == x = y
subst _ _ t = t

type2tableName t = f False t
where
	f _ (GenTypeVar x) = x
	f _ (GenTypeCons x) = case x of
		"_List" -> "l"
		"Char" -> "c"
		"Int" -> "i"
		"Real" -> "r"
		"Bool" -> "b"
		"OBJECT" -> "o"
		"EITHER" -> "e"
		"CONS" -> "k"
		"PAIR" -> "p"
		"UNIT" -> "n"
		"Maybe" -> "m"
		"Binary252" -> "t"
		"CompactList" -> "x"
		"{}" -> "a"
		"{!}" -> "s"
		"_String" -> "u"
		"GerdaObject" -> "g"
		s | s % (0, 5) == "_Tuple" -> s % (6, size s - 1)
		els -> els
	f False (GenTypeArrow x y) = f True x +++ "_" +++ f False y
	f False (GenTypeApp x y) = f False x +++ " " +++ f True y
	f True x = "(" +++ f False x +++ ")"

sqlTypeInfo SqlBit = (2, SQL_C_BIT, SQL_BIT, "BIT")
sqlTypeInfo SqlInteger = (2, SQL_C_SLONG, SQL_INTEGER, "INTEGER")
sqlTypeInfo SqlDouble = (3, SQL_C_DOUBLE, SQL_DOUBLE, "DOUBLE")
sqlTypeInfo SqlChar1 = (2, SQL_C_BINARY, SQL_BINARY, "CHAR(1)")
sqlTypeInfo SqlVarChar252 = (64, SQL_C_BINARY, SQL_VARCHAR, "VARCHAR(252)")

separatorList s xs = f xs
where
	f [x] = toString x
	f [x:xs] = toString x +++ s +++ f xs
	f _ = ""

instance toString GenType where
	toString x = f x False
	where
		f (GenTypeVar x) _ = x
		f (GenTypeCons x) _ = x
		f (GenTypeArrow x y) _ = f x True +++ " -> " +++ f y False
		f (GenTypeApp x y) False = f x False +++ " " +++ f y True
		f t True = "(" +++ f t False +++ ")"

instance == SqlAttr where
	(==) SqlNull SqlNull = True
	(==) SqlPrimary SqlPrimary = True
	(==) SqlUnique SqlUnique = True
	(==) (SqlReference t1) (SqlReference t2) = t1 == t2
	(==) _ _ = False

LocalAlloc :: !Int !Int !*st -> (!Int, !*st)
LocalAlloc flags size st = code {
		ccall LocalAlloc@8 "PII:I:A"
	}

LocalFree ::  !Int !*st -> (!Int, !*st)
LocalFree p st = code {
		ccall LocalFree@4 "PI:I:A"
	}

sqlEscape :: !String -> String
sqlEscape s = toString (['`':escape (fromString s)])
where
	escape [c:cs] 
		| toInt c < 32 = abort ("Illegal SQL string, cannot escape symbol < 32: " +++ s)
		| toInt c > 127 = abort ("Illegal SQL string, cannot escape symbol > 127: " +++ s)
//		| c == '`' = abort ("Illegal SQL string, contains a `: " +++ s)
		| c == '`' = ['\'':escape cs]
		= [c:escape cs]
	escape _ = ['`']

poke :: !Int !Int !*st -> *st
poke p v st = code {
    pushI -4
    addI
    push_b_a 0
    pop_b 1
    fill1_r _ 0 1 0 01
.keep 0 1
    pop_a 1
}

peek :: !Int !*st -> (!Int, !*st)
peek p st = code {
    push_b_a 0
    pop_b 1
    pushD_a 0
    pop_a 1
}

cast :: !u:a -> v:b
cast _ = code {
		pop_a	0
	}

derive bimap GerdaFunctions, (,), (,,), (,,,), [], Maybe
derive gerda CompactList
