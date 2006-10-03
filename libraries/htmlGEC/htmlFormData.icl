implementation module htmlFormData

import htmlDataDef
import StdMaybe, StdBool, StdString, StdInt

// utility for creating FormId's

pFormId :: !String !d -> (FormId d)		// persistent formid
pFormId s d = {id = s, lifespan = Persistent, mode = Edit, storage = PlainString, ival = d}

rFormId :: !String !d -> (FormId d)		// persistent formid
rFormId s d = {id = s, lifespan = PersistentRO, mode = Edit, storage = PlainString, ival = d}

sFormId :: !String !d -> (FormId d)		// session formid
sFormId s d = {id = s, lifespan = Session, mode = Edit, storage = PlainString, ival = d}

nFormId :: !String !d -> (FormId d)		// page formid
nFormId s d = {id = s, lifespan = Page, mode = Edit, storage = PlainString, ival = d}

xtFormId :: !String !d -> (FormId d)	// persistent formid
xtFormId s d = {id = s, lifespan = Temp, mode = NoForm, storage = PlainString, ival = d}

tFormId :: !String !d -> (FormId d)	// persistent formid
tFormId s d = {id = s, lifespan = Temp, mode = Edit, storage = PlainString, ival = d}

tdFormId :: !String !d -> (FormId d)	// persistent formid
tdFormId s d = {id = s, lifespan = Temp, mode = Display, storage = PlainString, ival = d}

xpFormId :: !String !d -> (FormId d)		// persistent formid
xpFormId s d = {id = s, lifespan = Persistent, mode = NoForm, storage = PlainString, ival = d}

xrFormId :: !String !d -> (FormId d)		// persistent formid
xrFormId s d = {id = s, lifespan = PersistentRO, mode = NoForm, storage = PlainString, ival = d}

xsFormId :: !String !d -> (FormId d)		// session formid
xsFormId s d = {id = s, lifespan = Session, mode = NoForm, storage = PlainString, ival = d}

xnFormId :: !String !d -> (FormId d)		// page formid
xnFormId s d = {id = s, lifespan = Page, mode = NoForm, storage = PlainString, ival = d}

pdFormId :: !String !d -> (FormId d)	// persistent formid
pdFormId s d = {id = s, lifespan = Persistent, mode = Display, storage = PlainString, ival = d}

rdFormId :: !String !d -> (FormId d)	// persistent formid
rdFormId s d = {id = s, lifespan = PersistentRO, mode = Display, storage = PlainString, ival = d}

sdFormId :: !String !d -> (FormId d)	// session formid
sdFormId s d = {id = s, lifespan = Session, mode = Display, storage = PlainString, ival = d}

ndFormId :: !String !d -> (FormId d)	// page formid
ndFormId s d = {id = s, lifespan = Page, mode = Display, storage = PlainString, ival = d}

// store info as a dynamic

pDFormId :: !String !d -> (FormId d)	// persistent formid
pDFormId s d = {id = s, lifespan = Persistent, mode = Edit, storage = StaticDynamic, ival = d}

rDFormId :: !String !d -> (FormId d)	// persistent formid
rDFormId s d = {id = s, lifespan = PersistentRO, mode = Edit, storage = StaticDynamic, ival = d}

sDFormId :: !String !d -> (FormId d)	// session formid
sDFormId s d = {id = s, lifespan = Session, mode = Edit, storage = StaticDynamic, ival = d}

nDFormId :: !String !d -> (FormId d)	// page formid
nDFormId s d = {id = s, lifespan = Page, mode = Edit, storage = StaticDynamic, ival = d}

pdDFormId :: !String !d -> (FormId d)	// persistent formid
pdDFormId s d = {id = s, lifespan = Persistent, mode = Display, storage = StaticDynamic, ival = d}

rdDFormId :: !String !d -> (FormId d)	// persistent formid
rdDFormId s d = {id = s, lifespan = PersistentRO, mode = Display, storage = StaticDynamic, ival = d}

sdDFormId :: !String !d -> (FormId d)	// session formid
sdDFormId s d = {id = s, lifespan = Session, mode = Display, storage = StaticDynamic, ival = d}

ndDFormId :: !String !d -> (FormId d)	// page formid
ndDFormId s d = {id = s, lifespan = Page, mode = Display, storage = StaticDynamic, ival = d}

// create id's

(++/) infixr 5 
(++/) s1 s2 = s1 +++ "/" +++ s2

extidFormId :: !(FormId d) !String -> (FormId d)
extidFormId formid s = {formid & id = formid.id ++/ s}

subFormId :: !(FormId a) !String !d 	-> (FormId d)	// make new formid of new type copying other old settinf
subFormId formid s d = {formid & id = formid.id ++/ s, ival = d}

subnFormId :: !(FormId a) !String !d 	-> (FormId d)	// make new formid of new type copying other old settinf
subnFormId formid s d = {formid & id = formid.id ++/ s, ival = d, lifespan = Page}

subsFormId :: !(FormId a) !String !d 	-> (FormId d)	// make new formid of new type copying other old settinf
subsFormId formid s d = {formid & id = formid.id ++/ s, ival = d, lifespan = Session}

subpFormId :: !(FormId a) !String !d 	-> (FormId d)	// make new formid of new type copying other old settinf
subpFormId formid s d = {formid & id = formid.id ++/ s, ival = d, lifespan = Persistent}

subtFormId :: !(FormId a) !String !d 	-> (FormId d)	// make new formid of new type copying other old settinf
subtFormId formid s d = {formid & id = formid.id ++/ s, ival = d, lifespan = Temp}

setFormId :: !(FormId d) !d -> (FormId d)			// set new initial value in formid
setFormId formid d = {formid & ival = d}

reuseFormId :: !(FormId d) !v -> (FormId v)
reuseFormId formid v = {formid & ival = v}

initID		:: !(FormId d) 		-> InIDataId d	// (Init,FormId a)
initID formid = (Init,formid)

setID		:: !(FormId d) !d 	-> InIDataId d	// (Set,FormId a)
setID formid na = (Set,setFormId formid na)

onMode :: !Mode a a a -> a
onMode Edit 	e1 e2 e3 = e1
onMode Display  e1 e2 e3 = e2
onMode NoForm   e1 e2 e3 = e3

toViewId :: !Init !d !(Maybe d) -> d
toViewId Init d Nothing 	= d
toViewId Init d (Just v) 	= v
toViewId _  d _ 			= d

toViewMap :: !(d -> v) !Init !d !(Maybe v) -> v
toViewMap f init d mv = toViewId init (f d) mv

instance toBool Init
where
	toBool Set = True
	toBool _ = False

instance == Mode
where
	(==) Display Display 	= True
	(==) Edit Edit 			= True
	(==) NoForm NoForm		= True
	(==) _ _ 				= False

instance == Init
where
	(==) Init Init 			= True
	(==) Set Set 			= True
	(==) Const Const 		= True
	(==) _ _ 				= False

instance == Lifespan
where
	(==) Persistent 	Persistent		= True
	(==) PersistentRO 	PersistentRO	= True
	(==) Session 		Session			= True
	(==) Page 			Page			= True
	(==) Temp 			Temp			= True
	(==) _ _ 							= False

instance < Lifespan
where
	(<) l1 l2 = toInt l1 < toInt l2

instance toInt 	Lifespan
where
	toInt 	Temp			= 0
	toInt 	Page			= 1
	toInt 	Session			= 2
	toInt 	PersistentRO	= 3
	toInt 	Persistent		= 4
