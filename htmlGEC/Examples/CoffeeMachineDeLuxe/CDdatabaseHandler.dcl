definition module CDdatabaseHandler

::	CDdatabase		:== [(Item,CD)]

:: Item
 =	{	itemnr		:: !Int
 	,	instock		:: !Int
 	,	prize		:: !Int
 	}

::  CD
 =  {   group       :: !Group
    ,   album       :: !Album
    ,   year        :: !Year
	,   totaltime   :: !Duration
   ,   tracks      :: ![Track]
    }
::  Track
 =  {   nr          :: !Int
    ,   title       :: !String
    ,   playtime    :: !Duration
    }
::  Duration
 =  {   minutes     :: !Int
    ,   seconds     :: !Int
    }
::  Group           :== String
::  Album           :== String
::  Year            :== Int


readCD 			:: *World -> (*World,[CD])
readCDdatabase	:: *World -> (*World,CDdatabase)

instance toString Duration
showPrize :: Int -> String

