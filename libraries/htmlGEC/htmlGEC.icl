implementation module htmlGEC

// Graphical editor Component for the web

Start = 1

import StdEnv, StdHtml

::	HGECArgs t env							// The arguments to define a GEC:
	=	{	gec_value	:: !t				// the value t to display will be stored as state in html page 
		,	gec_show	:: !t -> Body		// a view of t expressed in html code
		,	update		:: !t env -> env	// the call back function called whenever input has changed in the body
		}

::	HGECVALUE t env
	=	{ gecSetValue :: t env -> env		// set a new value in the GEC 
		}
		

createHGEC :: (HGECArgs t *World) *World -> *(HGECVALUE t *World,*World) //| gGEC{|*|} a & bimap{|*|} ps
createHGEC args world
# htmlpage = args.gec_show args.gec_value
# htmlpage = storecombine (args.update,args.gec_value,htmlpage)
# world = printall htmlpage world
= (undef, world) 
where
	printall :: Html *World -> *World
	printall htmlpage world =  print_to_stdout htmlpage world

storecombine args = undef