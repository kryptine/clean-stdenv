implementation module MinesBest

import StdBool, StdInt, StdString, StdFile, StdArray, StdTuple
import deltaSystem, deltaPicture, deltaFont
import MineTypes
    
BestX		:== 270
BestY		:== 240

NoThreeBest	:== ("",0,"",0,"",0)
NoBestTimes	:== (NoThreeBest,NoThreeBest,NoThreeBest)

//	Highscores file handling:
ReadHiScores :: !String !*Files -> (!(!*File,!BestTimes),!*Files)
ReadHiScores fname files
	# (exists,file,files)	= fopen fpath FReadText files
	| exists
	= ((file1,best),files)
	with
		(best,file1)		= ReadHighs file
	# (_,file,files)		= fopen fpath FWriteText files
	| otherwise
	= ((file,NoBestTimes),files)
where
	fpath = HomePath fname
	
	ReadHighs :: !*File -> (!BestTimes,!*File)
	ReadHighs file
		# (easy, file)	= ReadThreeTimes file
		  (inter,file)	= ReadThreeTimes file
		  (hard, file)	= ReadThreeTimes file
		= ((easy,inter,hard),file)
	where
		ReadThreeTimes :: !*File -> (!ThreeBest,!*File)
		ReadThreeTimes file
			# (  n1,file)	= freadline file
			  (_,b1,file)	= freadi    file
			  (_,_, file)	= freadc    file
			  (  n2,file)	= freadline file
			  (_,b2,file)	= freadi    file
			  (_,_, file)	= freadc    file
			  (  n3,file)	= freadline file
			  (_,b3,file)	= freadi    file
			  (_,_,file)	= freadc    file
			= ((StripNl n1,Decode b1,StripNl n2,Decode b2,StripNl n3,Decode b3),file)
		where
			StripNl :: !String -> String
			StripNl string
				| lmin1>0	= string%(0,lmin1-1)
				| otherwise	= ""
			where
				lmin1		= size string - 1

WriteHiScores :: !*File !BestTimes !*Files -> *Files
WriteHiScores file best files
	# (success,file)= freopen file FWriteText
	| not success
	= snd (fclose file files)
	# file			= WriteHighs best file
	| otherwise
	= snd (fclose file files)
where
	WriteHighs :: !BestTimes !*File -> *File
	WriteHighs (easy,inter,hard) file
		# file	= WriteThreeBest easy  file
		  file	= WriteThreeBest inter file
		  file	= WriteThreeBest hard  file
		= file
	where
		WriteThreeBest :: !ThreeBest !*File -> *File
		WriteThreeBest (n1,b1,n2,b2,n3,b3) file
			# file = file<<<n1<<<'\n'<<<Code b1<<<'\n'
			  file = file<<<n2<<<'\n'<<<Code b2<<<'\n'
			  file = file<<<n3<<<'\n'<<<Code b3<<<'\n'
			= file

Decode :: !Int -> Int
Decode best = (best-4187)/13

Code :: !Int -> Int
Code best = 4187+13*best 

//	Is it a best time? yes: add it to the hall of fame.
ItsABestTime :: !Int !Dimension !Time !BestTimes -> Bool
ItsABestTime nr dim (Running time) best
	| nr==EasyMines  && dim==EasyDim	= OneOfTheThreeBest time (fst3 best)
	| nr==InterMines && dim==InterDim	= OneOfTheThreeBest time (snd3 best)
	| nr==HardMines  && dim==HardDim	= OneOfTheThreeBest time (thd3 best)
	| otherwise							= False
where
	OneOfTheThreeBest :: !Int !ThreeBest -> Bool
	OneOfTheThreeBest time (name1,best1,name2,best2,name3,best3)
		= best1==0 || best2==0 || best3==0 || time<best1 || time<best2 || time<best3
ItsABestTime _ _ _ _
	= False

AddBestTime :: !String !Int !Dimension !Time !BestTimes -> BestTimes
AddBestTime name nr dim (Running time) (easy,inter,hard)
	| nr==EasyMines  && dim==EasyDim	= (AddToThreeBest name time easy,inter,hard)
	| nr==InterMines && dim==InterDim	= (easy,AddToThreeBest name time inter,hard)
	| nr==HardMines  && dim==HardDim	= (easy,inter,AddToThreeBest name time hard)
where
	AddToThreeBest :: !String !Int !ThreeBest -> ThreeBest
	AddToThreeBest name time (n1,t1,n2,t2,_,_)
		| time<t1 || t1==0	= (name,time,n1,t1,n2,t2)
		| time<t2 || t2==0	= (n1,t1,name,time,n2,t2)
		| otherwise			= (n1,t1,n2,t2,name,time)
AddBestTime _ _ _ _ best
	= best

//	Show the best times.
ShowBestTimes :: !BestTimes -> [DrawFunction]
ShowBestTimes (easy, inter, hard)
	= [	EraseRectangle	((0,0),(BestX,BestY))
	  ,	SetPenColour	GreenColour
	  ,	SetPenSize		(2,2)
	  ,	DrawRectangle	((3,3),(BestX-3,BestY-3))
	  ,	SetPenNormal
	  ,	DrawRectangle	((in` ,in`),(inx,BestY-in`))
	  ,	MovePenTo		(in`  ,y1)
	  ,	LinePenTo		(inx-1,y1)
	  ,	MovePenTo		(in`  ,y2)
	  ,	LinePenTo		(inx-1,y2)
	  ,	SetPenColour	BlackColour
	  ,	ShowThreeBest	"Easy"         in` in`		easy
	  ,	ShowThreeBest	"Intermediate" in` (y1+5)	inter
	  ,	ShowThreeBest	"Hard"         in` y2		hard
	  ]
where
	inx	= BestX-in`
	in`	= 6
	y1	= BestY/3
	y2	= y1*2
	
	ShowThreeBest :: !String !Int !Int !ThreeBest !Picture -> Picture
	ShowThreeBest skill xoff yoff (n1,b1,n2,b2,n3,b3) pict
		# pict			= SetFont	 newyork pict
		  pict			= DrawString skill		   (MovePenTo (xoff+10,yoff+18) pict)
		  pict			= SetFont	 helvetica pict
		  pict			= DrawString ("1. "+++ n1) (MovePenTo (noff,y1) pict)
		  pict			= DrawString ("2. "+++ n2) (MovePenTo (noff,y2) pict)
		  pict			= DrawString ("3. "+++ n3) (MovePenTo (noff,y3) pict)
		  pict			= DrawString (toString b1) (MovePenTo (toff,y1) pict)
		  pict			= DrawString (toString b2) (MovePenTo (toff,y2) pict)
		  pict			= DrawString (toString b3) (MovePenTo (toff,y3) pict)
		= pict
	where
		noff			= xoff+6
		toff			= xoff+220
		y1				= yoff+36
		y2				= yoff+51
		y3				= yoff+66
		(_,newyork)		= SelectFont "NewYork"   ["BoldStyle"] 14
		(_,helvetica)	= SelectFont "Helvetica" ["BoldStyle"] 12

LimitString :: !Int !String -> String
LimitString limit string
	| size string>limit	= string%(0,limit-1)
	| otherwise			= string
