implementation module StdPrint

//	Clean Object I/O library, version 1.2

import StdEnum, StdInt, StdList
import StdMaybe, StdPicture
import commondef, osPrint

print :: !Bool !Bool
		 .(PrintInfo !*Picture -> ([IdFun *Picture],!*Picture))
         !*printEnv 
      -> *printEnv
      | PrintEnvironments printEnv
print doDialog emulateScreen prFun printEnv
	# (_,printEnv) = OSprintPagePerPage doDialog emulateScreen 0 initFun stateTransition printEnv
	= printEnv
  where
  	initFun _ printInfo picture
  		# (drawFuns,picture) = prFun printInfo picture
  		= ((isEmpty drawFuns,zeroOrigin), (drawFuns,picture))
  	stateTransition ([drawFun:rest],picture)
  		=((isEmpty rest,zeroOrigin), (rest, drawFun picture))

zeroOrigin :== zero  		

printUpdateFunction :: !Bool (UpdateState -> *Picture -> *Picture) [!Rectangle] *printEnv 
					-> *printEnv
					| PrintEnvironments printEnv
printUpdateFunction doDialog updateFunc rectangles printEnv
	# (_, printEnv) = OSprintPagePerPage doDialog True Nothing initState pageTrans printEnv
	= printEnv
  where
	initState Nothing printInfo=:{ page={ w=wP,h=hP }, range=(first,last), copies } picture
		= (	( isEmpty printedClips,
			  if (isEmpty printedClips) zeroOrigin (hd printedClips).corner1
			),
			( printedClips, picture )
		  )
		where
			printedClips = flatten (repeatn copies (allClips % (first-1,last-1)))
			allClips = flatten (map clipsOfOneRectangle rectangles)
			clipsOfOneRectangle rectangle
				= clipRectangles
				where
					{rleft=x1,rtop=y1,rright=x2,rbottom=y2} = RectangleToRect rectangle
					wR = x2-x1+1
					hR = y2-y1+1
					columns = [0..(ceilOfRatio wR wP)-1]
					rows = [0..(ceilOfRatio hR hP)-1]
					clipRectangles = [ { corner1 = { x=c*wP+x1, y=r*hP+y1 },
										 corner2 = { x=min ((c+1)*wP+x1) x2,
										   			 y=min ((r+1)*hP+y1) y2
										 }} \\ r<-rows,c<-columns]
					ceilOfRatio num denum 		// ceil (num/denom)
						| num mod denum == 0
							= num/denum
						= num/denum + 1
					min x y
						| x>y = y
						=x
	pageTrans ([clipRect:rest], picture)
		# drawFunction = updateFunc (RectangleToUpdateState clipRect)
		= ( (isEmpty rest,if (isEmpty rest) zeroOrigin (hd rest).corner1),
		 	(rest, appClipPicture (toRegion clipRect) drawFunction picture )
		  )

printPagePerPage ::	!.Bool !Bool 
					.x
					.(.x -> .(PrintInfo -> .(*Picture -> ((.Bool,Point2),(.state,*Picture)))))
					((.state,*Picture) -> ((.Bool,Point2),(.state,*Picture)))
					!*printEnv 
				-> 	(Alternative .x .state,!*printEnv)
		        | PrintEnvironments printEnv
printPagePerPage doDialog emulateScreen x initFun transFun printEnv
	= OSprintPagePerPage doDialog emulateScreen x initFun transFun printEnv
