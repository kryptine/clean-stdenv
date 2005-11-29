implementation module htmlFormData

import htmlDataDef
import StdMaybe, StdBool, StdString

// utility for creating FormId's

pFormId :: !String -> FormId		// persitent formid
pFormId s = {id = s, lifespan = Persistent, mode = Edit, storage = PlainString}

sFormId :: !String -> FormId		// session formid
sFormId s = {id = s, lifespan = Session, mode = Edit, storage = PlainString}

nFormId :: !String -> FormId		// page formid
nFormId s = {id = s, lifespan = Page, mode = Edit, storage = PlainString}

pdFormId :: !String -> FormId	// persitent formid
pdFormId s = {id = s, lifespan = Persistent, mode = Display, storage = PlainString}

sdFormId :: !String -> FormId	// session formid
sdFormId s = {id = s, lifespan = Session, mode = Display, storage = PlainString}

ndFormId :: !String -> FormId	// page formid
ndFormId s = {id = s, lifespan = Page, mode = Display, storage = PlainString}

// store info as a dynamic

pDFormId :: !String -> FormId	// persitent formid
pDFormId s = {id = s, lifespan = Persistent, mode = Edit, storage = StaticDynamic}

sDFormId :: !String -> FormId	// session formid
sDFormId s = {id = s, lifespan = Session, mode = Edit, storage = StaticDynamic}

nDFormId :: !String -> FormId	// page formid
nDFormId s = {id = s, lifespan = Page, mode = Edit, storage = StaticDynamic}

pdDFormId :: !String -> FormId	// persitent formid
pdDFormId s = {id = s, lifespan = Persistent, mode = Display, storage = StaticDynamic}

sdDFormId :: !String -> FormId	// session formid
sdDFormId s = {id = s, lifespan = Session, mode = Display, storage = StaticDynamic}

ndDFormId :: !String -> FormId	// page formid
ndDFormId s = {id = s, lifespan = Page, mode = Display, storage = StaticDynamic}

// create id's

extendFormId :: !FormId !String -> FormId
extendFormId formid s = {formid & id = formid.id +++ s}

// frequently used variants of mkViewForm

toViewId :: !(Init d) !(Maybe d) -> d
toViewId (Init d) Nothing 	= d
toViewId (Set d) _ 			= d
toViewId _ (Just v) 		= v

toViewMap :: !(d -> v) !(Init d) !(Maybe v) -> v
toViewMap f (Init d) Nothing 	= f d
toViewMap f (Set d) _ 			= f d
toViewMap _ _ (Just v) 			= v

GetInit :: !(Init d) -> d
GetInit (Init d) 	= d
GetInit (Set d)		= d

PropInit :: !(Init v) !d -> (Init d)
PropInit (Init _) 	d = Init d
PropInit (Set _)	d = Set d

instance toBool (Init d) 
where
	toBool (Init _) = False
	toBool (Set _) = True
