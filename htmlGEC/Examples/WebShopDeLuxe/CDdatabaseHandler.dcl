definition module CDdatabaseHandler

::	CD_Database		
 = 	{	item		:: Item
 	,	cd			:: CD
 	}

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
readCD_Database	:: *World -> (*World,[CD_Database])

:: SearchOption = AnyAlbum | AnyArtist | AnySong

searchDatabase :: SearchOption String [CD_Database] -> (Bool,[CD_Database])

instance toString Duration
showPrize :: Int -> String

