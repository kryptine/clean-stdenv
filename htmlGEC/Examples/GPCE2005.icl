module GPCE2005

/**	Examples that appear in the GPCE 2005 paper.
*/
import StdEnv
import StdHtml

import tree

Start world = doHtml mypage3 world

//	Running example, using gHGEC:

mypage1 hst
   # ((list,body_list),hst) = gHGEC{|*|} /*"List"         */ HEdit    [1,5,2]              hst
     ((tree,body_tree),hst) = gHGEC{|*|} /*"Balanced Tree"*/ HDisplay (toBalanceTree list) hst
   = (Html (Head [] []) 
           (Body [] [ H1 [] "Balancing Tree From List"
                    , Br, Txt "List:",          body_list
                    , Hr []
                    , Br, Txt "Balanced Tree:", body_tree
                    ]
           )
     ,hst
     )


//	Running example, using generatePage:

mypage2 hst
   # ((list,body_list),hst) = generatePage "List"          HEdit    [1,5,2]              hst
     ((tree,body_tree),hst) = generatePage "Balanced Tree" HDisplay (toBalanceTree list) hst
   = (Html (Head [] []) 
           (Body [] [ H1 [] "Balancing Tree From List"
                    , Br, B [] "List:",          body_list
                    , Hr []
                    , Br, B [] "Balanced Tree:", body_tree
                    ]
           )
     ,hst
     )

//	Running example, using circuits:
mypage3 hst
	# ((_,body_list_to_tree),hst) = startCircuit mycircuit [1,5,2] hst
	= (Html (Head [] [])
	        (Body [] [ H1 [] "Balancing Tree From List"
	                 , Br
	                 , BodyTag body_list_to_tree
	                 ]
	        )
	  ,hst
	  )
where
	mycircuit = edit "List" >>> arr toBalanceTree >>> display "Balanced Tree"

toBalanceTree = fromListToBalTree

derive gHGEC []
derive gUpd  []
