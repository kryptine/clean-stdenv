implementation module systemid


//	Clean Object I/O library, version 1.2


import	StdInt, StdBool, StdOverloaded


::	SystemId
	=	SystemId [Int] Int


WorldSystemId :: !Int -> SystemId	// This systemid is always associated with the World
WorldSystemId nrCreated = SystemId [0] nrCreated

WorldChildId :: !Int -> SystemId	// This systemid is used for creating systemid from World
WorldChildId nrCreated = SystemId [nrCreated,0] 0

InitSystemId :: SystemId			// This systemid is always associated with the initial IOState
InitSystemId = SystemId [1] 0

NullSystemId :: SystemId			// Dummy systemid
NullSystemId = SystemId [] 0

IncrSystemId :: !SystemId -> (!SystemId,!SystemId)
IncrSystemId id=:(SystemId idpath nrCreated)
	= (SystemId idpath idmax`,SystemId [idmax`:idpath] 0)
where
	idmax`	= nrCreated+1

/*
SystemIdSetProcId :: !ProcId !SystemId -> SystemId
SystemIdSetProcId _ ionr = ionr
*/

instance == SystemId where
	(==) :: !SystemId !SystemId -> Bool
	(==) (SystemId idpath1 _) (SystemId idpath2 _)
			= eqidpath idpath1 idpath2
		 where
			eqidpath [x:xs] [y:ys]	= x==y && eqidpath xs ys
			eqidpath []		[]		= True
			eqidpath _		_		= False

instance < SystemId where
	(<) :: !SystemId !SystemId -> Bool
	(<) (SystemId idpath1 _) (SystemId idpath2 _)
			= lessidpath ids1 ids2
		where
			(ids1,ids2)		= removecommonprefix idpath1 idpath2
			
			removecommonprefix xs`=:[x:xs] ys`=:[y:ys]
				| x==y		= removecommonprefix xs ys
				| otherwise	= (xs`,ys`)
			removecommonprefix xs ys
				= (xs,ys)
			
			lessidpath [x:xs]	[y:ys]	= x<y
			lessidpath []		[_:_]	= True
			lessidpath _		_		= False
