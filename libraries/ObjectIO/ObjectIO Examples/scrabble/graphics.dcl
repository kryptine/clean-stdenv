definition module graphics

import	StdPicture, StdPSt
import	board

rbBoardGrey			:== RGB {r=191,g=191,b=191}			// The background colour of the board

displaywidth		:==	2+250+2		//250
displayheight		:==	2+130+2		//130
boardwidth			:==	391
boardheight			:==	391
squarewidth			::	Int
squareheight		::	Int

abs2rel				:: !(!Int,!Int) -> (!Int,!Int)

boardlook			:: !Board Point2 !SelectState !UpdateState	!*Picture		-> *Picture
redrawboard			:: !Id !Board Point2						!(IOSt .l .p)	-> IOSt .l .p
drawfocus			:: !Bool !Point2							!*Picture		-> *Picture

letterboxlook		:: ![Char] SelectState UpdateState			!*Picture		-> *Picture
drawletterbox		:: !Id ![Char]								!(IOSt .l .p)	-> IOSt .l .p

drawplayer1letters	:: !Id ![Char]								!(IOSt .l .p)	-> IOSt .l .p
drawplayer2letters	:: !Id ![Char]								!(IOSt .l .p)	-> IOSt .l .p
playerletterslook	:: ![Char] SelectState UpdateState			!*Picture		-> *Picture

drawplayer1score	:: !Id !Int									!(IOSt .l .p)	-> IOSt .l .p
drawplayer2score	:: !Id !Int									!(IOSt .l .p)	-> IOSt .l .p

drawcommunication	:: !Id ![String]							!(IOSt .l .p)	-> IOSt .l .p
displaylook			:: ![String] SelectState !UpdateState		!*Picture		-> *Picture
drawprogress		:: !Id !Player !Progress !Placing			!(IOSt .l .p)	-> IOSt .l .p
