implementation module id


//	Clean Object I/O library, version 1.2


import StdBool, StdFunc, StdInt, StdList, StdOrdList, StdOverloaded, StdString
import commondef, device, systemid


idFatalError :: String String -> .x
idFatalError rule error
	= FatalError rule "id" error

 
::	Id										// The Id data type:
	=	CustomId	!Int						// Range of Ids generated by programs
	|	CustomRId	!Int						// Range of Ids generated by programs for uni-receivers
	|	CustomR2Id	!Int						// Range of Ids generated by programs for bi-receivers
	|	SysId		!Int						// Range of Ids generated for device instances
	|	SpecialId	!Int						// Range of Ids generated for special components
::	RId mess								// The identification of uni-directional receivers:
	=	RId !Int
::	R2Id mess resp							// The identification of bi-directional receivers:
	=	R2Id !Int
::	IdTable									// The table of all Id entries
	=	{	customIds	:: ![(Int,IdParent)]	// all CustomId   entries
		,	customRIds	:: ![(Int,IdParent)]	// all CustomRId  entries
		,	customR2Ids	:: ![(Int,IdParent)]	// all CustomR2Id entries
		,	sysIds		:: ![(Int,IdParent)]	// all SysId      entries
	//	,	specialIds	:: ![(Int,IdParent)]	// all SpecialId  entries
		}
::	IdParent
	=	{	idpIOId		:: !SystemId		// Id of parent process
		,	idpDevice	:: !Device			// Device kind of parent GUI object
		,	idpId		:: !Id				// Id of parent GUI object
		}

//	Special Ids:

SpecialWindowMenuId			:==	1			// The special id of the WindowMenu
SpecialWindowMenuRadioId	:==	2			// The id of the RadioMenu displaying the open windows
SpecialWindowMenuCascadeId	:==	3			// The id of the Cascade WindowMenu item
SpecialWindowMenuTileHId	:==	4			// The id of the Tile Horizontally WindowMenu item
SpecialWindowMenuTileVId	:==	5			// The id of the Tile Vertically WindowMenu item
SpecialWindowMenuSeparatorId:==	6			// The id of the MenuSeparator item

specialIdName :: !Int -> {#Char}
specialIdName SpecialWindowMenuId			= "WindowMenuId"
specialIdName SpecialWindowMenuRadioId		= "WindowMenuRadioId"
specialIdName SpecialWindowMenuCascadeId	= "SpecialWindowMenuCascadeId"
specialIdName SpecialWindowMenuTileHId		= "SpecialWindowMenuTileHId"
specialIdName SpecialWindowMenuTileVId		= "SpecialWindowMenuTileVId"
specialIdName SpecialWindowMenuSeparatorId	= "WindowMenuSeparatorId"
specialIdName _								= idFatalError "toString (Id)" "undefined special Id."

WindowMenuId			:: Id;		WindowMenuId			= SpecialId SpecialWindowMenuId;
WindowMenuRadioId		:: Id;		WindowMenuRadioId		= SpecialId SpecialWindowMenuRadioId;
WindowMenuCascadeId		:: Id;		WindowMenuCascadeId		= SpecialId SpecialWindowMenuCascadeId;
WindowMenuTileHId		:: Id;		WindowMenuTileHId		= SpecialId SpecialWindowMenuTileHId;
WindowMenuTileVId		:: Id;		WindowMenuTileVId		= SpecialId SpecialWindowMenuTileVId;
WindowMenuSeparatorId	:: Id;		WindowMenuSeparatorId	= SpecialId SpecialWindowMenuSeparatorId;


toId :: !Int -> Id
toId i = CustomId i

toRId :: !Int -> RId mess
toRId i = RId i

toR2Id :: !Int -> R2Id mess resp
toR2Id i = R2Id i

sysId :: !Int -> Id
sysId i = SysId i

fromId :: !Id -> Int
fromId (CustomId   id)		= id
fromId (CustomRId  id)		= id
fromId (CustomR2Id id)		= id
fromId (SysId	   id)		= id
fromId (SpecialId  id)		= id

isSysId :: !Id -> Bool
isSysId (SysId _)			= True
isSysId _					= False

isCustomId :: !Id -> Bool
isCustomId (CustomId _)		= True
isCustomId _				= False

isCustomRId :: !Id -> Bool
isCustomRId (CustomRId _)	= True
isCustomRId _				= False

isCustomR2Id :: !Id -> Bool
isCustomR2Id (CustomR2Id _)	= True
isCustomR2Id _				= False

isSpecialId :: !Id -> Bool
isSpecialId (SpecialId _)	= True
isSpecialId _				= False

instance == Id where
	(==) :: !Id !Id -> Bool
	(==) (CustomId	 id1)	id	= case id of
									(CustomId	id2)	-> id1==id2
									(CustomRId	id2)	-> id1==id2 // MW11++
									(CustomR2Id	id2)	-> id1==id2 // MW11++
									_					-> False
	(==) (CustomRId	 id1)	id	= case id of
									(CustomId	id2)	-> id1==id2 // MW11++
									(CustomRId	id2)	-> id1==id2
									(CustomR2Id	id2)	-> id1==id2 // MW11++
									_					-> False
	(==) (CustomR2Id id1)	id	= case id of
									(CustomId	id2)	-> id1==id2 // MW11++
									(CustomRId	id2)	-> id1==id2 // MW11++
									(CustomR2Id	id2)	-> id1==id2
									_					-> False
	(==) (SysId		 id1)	id	= case id of
									(SysId		id2)	-> id1==id2
									_					-> False
	(==) (SpecialId  id1)	id	= case id of
									(SpecialId	id2)	-> id1==id2
									_					-> False
	(==) _					_	= False

instance == (RId mess) where
	(==) :: !(RId mess) !(RId mess) -> Bool
	(==) (RId i) (RId j) = i==j

instance == (R2Id mess resp) where
	(==) :: !(R2Id mess resp) !(R2Id mess resp) -> Bool
	(==) (R2Id i) (R2Id j) = i==j

RIdtoId :: (RId mess) -> Id
RIdtoId (RId i) = CustomRId i

R2IdtoId :: (R2Id mess resp) -> Id
R2IdtoId (R2Id i) = CustomR2Id i

instance toString Id where
	toString :: !Id -> {#Char}
	toString (CustomId id)	= "toId "+++toString id
	toString (CustomRId _)	= "RId"
	toString (CustomR2Id _)	= "R2Id"
	toString (SysId _)		= "Id"
	toString (SpecialId id)	= specialIdName id


//	IdTable operations:

initialIdTable :: IdTable
initialIdTable
	= {	customIds	= []
	  ,	customRIds	= []
	  ,	customR2Ids	= []
	  ,	sysIds		= []
/*	  ,	specialIds	= sort [SpecialWindowMenuId
						   ,SpecialWindowMenuRadioId
						   ,SpecialWindowMenuCascadeId
						   ,SpecialWindowMenuTileHId
						   ,SpecialWindowMenuTileVId
						   ,SpecialWindowMenuSeparatorId
						   ]
*/	  }

memberIdTable :: !Id !IdTable -> Bool
memberIdTable (CustomId   nr) {customIds}	= membersortlist nr customIds
memberIdTable (CustomRId  nr) {customRIds}	= membersortlist nr customRIds
memberIdTable (CustomR2Id nr) {customR2Ids}	= membersortlist nr customR2Ids
memberIdTable (SysId      nr) {sysIds}		= membersortlist nr sysIds
memberIdTable (SpecialId _)    _			= False

// membersortlist checks for membership in a < sorted list
membersortlist :: !Int ![(Int,x)] -> Bool
membersortlist x [(y,_):ys]
	| x==y			= True
	| x>y			= False
	| otherwise		= membersortlist x ys
membersortlist _ _	= False

okMembersIdTable :: ![Id] !IdTable -> Bool
okMembersIdTable ids idTable
	= noDuplicates ids && all (\id->not (memberIdTable id idTable)) ids

getIdParent :: !Id !IdTable -> Maybe IdParent
getIdParent (CustomId   nr) {customIds}		= getparentsortlist nr customIds
getIdParent (CustomRId  nr) {customRIds}	= getparentsortlist nr customRIds
getIdParent (CustomR2Id nr) {customR2Ids}	= getparentsortlist nr customR2Ids
getIdParent (SysId      nr) {sysIds}		= getparentsortlist nr sysIds
getIdParent (SpecialId  _)  _				= Nothing

// getparentsortlist retrieves (Just parent) in a < sorted list. If not found, Nothing is returned
getparentsortlist :: !Int ![(Int,x)] -> Maybe x
getparentsortlist x [(y,py):ys]
	| x==y		= Just py
	| x>y		= Nothing
	| otherwise	= getparentsortlist x ys
getparentsortlist _ _
	= Nothing

getIdParents :: ![Id] !IdTable -> [Maybe IdParent]
getIdParents ids idTable
	= [getIdParent id idTable \\ id<-ids]

addIdToIdTable :: !Id !IdParent !IdTable -> *(!Bool,!IdTable)
addIdToIdTable (CustomId nr) idParent idTable=:{customIds}		= (not found,{idTable & customIds=ids})
where	(found,ids)	= addtosortlist nr idParent customIds
addIdToIdTable (CustomRId nr) idParent idTable=:{customRIds}	= (not found,{idTable & customRIds=ids})
where	(found,ids)	= addtosortlist nr idParent customRIds
addIdToIdTable (CustomR2Id nr) idParent idTable=:{customR2Ids}	= (not found,{idTable & customR2Ids=ids})
where	(found,ids)	= addtosortlist nr idParent customR2Ids
addIdToIdTable (SysId nr) idParent idTable=:{sysIds}			= (not found,{idTable & sysIds=ids})
where	(found,ids)	= addtosortlist nr idParent sysIds
//addIdToIdTable (SpecialId nr) idParent idTable=:{specialIds}	= (not found,{idTable & specialIds=ids})
//where	(found,ids)	= addtosortlist nr idParent specialIds
addIdToIdTable (SpecialId nr) idParent idTable
	= (False,idTable)

// addtosortlist adds an element in a < sorted list. True iff element is already member.
addtosortlist :: !Int !IdParent ![(Int,IdParent)] -> (!Bool,![(Int,IdParent)])
addtosortlist x px [(y,py):ys]
	| x==y			= (True, [(y,py):ys])
	| x>y			= (False,[(x,px),(y,py):ys])
	| otherwise		= (found,[(y,py):ys`])
	with
		(found,ys`)	= addtosortlist x px ys
addtosortlist x px _
	= (False,[(x,px)])

addIdsToIdTable :: ![(Id,IdParent)] !IdTable -> *(!Bool,!IdTable)
addIdsToIdTable idparents idTable
//	# (oks,idTable)	= seqList (map (\(id,parent)->addIdToIdTable id parent) idparents) idTable
	# (oks,idTable)	= StrictSeqList (map add idparents) idTable
	= (and oks,idTable)
where
	add :: !(!Id,!IdParent) !IdTable -> *(!Bool,!IdTable)
	add (id,parent) idTable = addIdToIdTable id parent idTable

removeIdFromIdTable :: !Id !IdTable -> (!Bool,!IdTable)
removeIdFromIdTable (CustomId nr) idTable=:{customIds}		= (not found,{idTable & customIds=ids})
where	(found,ids)	= removefromsortlist nr customIds
removeIdFromIdTable (CustomRId nr) idTable=:{customRIds}	= (not found,{idTable & customRIds=ids})
where	(found,ids)	= removefromsortlist nr customRIds
removeIdFromIdTable (CustomR2Id nr) idTable=:{customR2Ids}	= (not found,{idTable & customR2Ids=ids})
where	(found,ids)	= removefromsortlist nr customR2Ids
removeIdFromIdTable (SysId nr) idTable=:{sysIds}			= (not found,{idTable & sysIds=ids})
where	(found,ids)	= removefromsortlist nr sysIds
removeIdFromIdTable (SpecialId _) idTable					= (False,idTable)

// removefromsortlist removes an element from a < sorted list. True iff element was member.
removefromsortlist :: !Int !w:[v:(Int,u:x)] -> (!Bool,!w:[v:(Int,u:x)]), [v<=u,w<=v]
removefromsortlist x [(y,py):ys]
	| x==y			= (True, ys)
	| x>y			= (False,[(y,py):ys])
	| otherwise		= (found,[(y,py):ys`])
	with
		(found,ys`)	= removefromsortlist x ys
removefromsortlist _ _
	= (False,[])

removeIdsFromIdTable :: ![Id] !IdTable -> (!Bool,!IdTable)
removeIdsFromIdTable [id:ids] idTable
	# (removed1,idTable)	= removeIdFromIdTable  id  idTable
	# (removed2,idTable)	= removeIdsFromIdTable ids idTable
	= (removed1 && removed2,idTable)
removeIdsFromIdTable _ idTable
	= (True,idTable)