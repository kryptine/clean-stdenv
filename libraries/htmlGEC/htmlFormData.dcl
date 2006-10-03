definition module htmlFormData

// common data type definition used for forms
// (c) 2005 - MJP

import htmlDataDef
import StdMaybe, StdBool

:: FormId d										// properties one has to assign to any form 
	=	{ id 		:: !String					// id *uniquely* identifying the form
		, lifespan	:: !Lifespan				// lifespan of form
		, mode		:: !Mode					// editable or not
		, storage	:: !StorageFormat			// serialization method
		, ival		:: !d						// initial value
		}

:: Init											// Usage of the value stored in FormId
	=	Const									//	The value is a constant
	|	Init 									// 	The value is taken as initial value
	|	Set  									// 	The value will be used as new iData value

:: Lifespan										// 	defines how long a form will be maintained		
	= 	Persistent								// 	form will live "forever" (in a file)
	|	PersistentRO							//	form will live "forever" (in a file), is used read-only
	| 	Session									// 	form will live as long as one browses between the pages offered by the application
	| 	Page									// 	form will be automatically garbage collected when no reference is made to it			
	|	Temp									//	form setting is not stored at all, only lives in application	

:: Mode											// one can choose:
	=	Edit									// 	an editable form
	| 	Display									// 	a non-editable form
	|	NoForm									//	do not generate a form, only a value

:: HBimap d v 									// swiss army nife allowing to make a distinction between data and view domain
	=	{ toForm   	:: Init d (Maybe v) -> v	// 	converts data to view domain, given current view
		, updForm 	:: Changed v -> v			// 	update function, True when the form is edited 
		, fromForm 	:: Changed v -> d			// 	converts view back to data domain, True when form is edited
		, resetForm :: Maybe (v -> v)			// 	can be used to reset view (eg for buttons)
		}
:: Changed
	=	{ isChanged	:: Bool						// is this form changed
		, changedId	:: String					// id of changed form
		}
:: StorageFormat								// Serialization method:
	=	StaticDynamic							// + higher order types, fast, NO dynamic linker needed; - works only for a specific application !
	| 	PlainString								// - first order types only, slow (requires generic parser); + can be used by anyone who knows the type

:: Form a 										// result of any form
	=	{ changed 	:: Bool						// the user has edited the form
		, value		:: a						// current value in data domain (feel)
		, form		:: [BodyTag]				// html code to create the form, representing view domain (look)
		}

:: InIDataId d									// Often used combination
	:==	(Init,FormId d)						

// **** easy creation of FormId's ****

nFormId		:: !String !d -> (FormId d)		// page 	  	, editable, string format
sFormId		:: !String !d -> (FormId d)		// session 	  	, editable, string format
pFormId		:: !String !d -> (FormId d)		// persistent 	, editable, string format
rFormId		:: !String !d -> (FormId d)		// persistentRO	, editable, string format

ndFormId	:: !String !d -> (FormId d)		// page 	  	, displayed non-editable, string format
sdFormId	:: !String !d -> (FormId d)		// session 	  	, displayed non-editable, string format
pdFormId	:: !String !d -> (FormId d)		// persistent 	, displayed non-editable, string format 
rdFormId	:: !String !d -> (FormId d)		// persistentRO	, displayed non-editable, string format 

xtFormId 	:: !String !d -> (FormId d)		// temp			, noform
xnFormId	:: !String !d -> (FormId d)		// page 	  	, noform, string format
xsFormId	:: !String !d -> (FormId d)		// session 	  	, noform, string format
xpFormId	:: !String !d -> (FormId d)		// persistent 	, noform, string format
xrFormId	:: !String !d -> (FormId d)		// persistentRO	, noform, string format

tFormId 	:: !String !d -> (FormId d)		// temp 	  	, editable, string format
tdFormId 	:: !String !d -> (FormId d)		// temp 	  	, displayed non-editable, string format


nDFormId	:: !String !d -> (FormId d)		// page 	  	, editable, static dynamic format
sDFormId	:: !String !d -> (FormId d)		// session 	  	, editable, static dynamic format
pDFormId	:: !String !d -> (FormId d)		// persistent 	, editable, static dynamic format
rDFormId	:: !String !d -> (FormId d)		// persistentRO	, editable, static dynamic format

ndDFormId	:: !String !d -> (FormId d)		// page 	  	, displayed non-editable, static dynamic format
sdDFormId	:: !String !d -> (FormId d)		// session 	  	, displayed non-editable, static dynamic format
pdDFormId	:: !String !d -> (FormId d)		// persistent 	, displayed non-editable, static dynamic format 
rdDFormId	:: !String !d -> (FormId d)		// persistentRO	, displayed non-editable, static dynamic format 

extidFormId :: !(FormId d) !String -> (FormId d)		// make new id by adding sufix 
subFormId 	:: !(FormId a) !String !d 	-> (FormId d)	// make new id af new type by adding suffix
subnFormId 	:: !(FormId a) !String !d 	-> (FormId d)	// idem with lifespan Page
subsFormId 	:: !(FormId a) !String !d 	-> (FormId d)	// idem with lifespan Session
subpFormId 	:: !(FormId a) !String !d 	-> (FormId d)	// idem with lifespan Persitent
subtFormId 	:: !(FormId a) !String !d 	-> (FormId d)	// idem with lifespan Temp

setFormId 	:: !(FormId d) !d -> (FormId d)				// set new initial value in formid
reuseFormId :: !(FormId a) !d -> (FormId d)				// reuse id for new type (only to be used in gform)

initID		:: !(FormId d) 		-> InIDataId d	// (Init,FormId a)
setID		:: !(FormId d) !d 	-> InIDataId d	// (Set,FormId a)

onMode 		:: !Mode a a a -> a					// choose arg depending on Edit, Display, NoForm

// manipulating initial values

toViewId  :: !Init !d! (Maybe d) -> d					// copy second on Set or if third is Nothing
toViewMap :: !(d -> v) !Init !d !(Maybe v) -> v			// same, but convert to view domain

instance toBool Init
instance == Init, Mode, Lifespan
instance < Lifespan
