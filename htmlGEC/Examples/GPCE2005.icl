module GPCE2005

/**	Examples that appear in the GPCE 2005 paper.
*/
import StdEnv
import StdHtml

import tree

Start world = doHtml mypage2 world

//	Simple Hello World example:
mypage0 :: *HSt -> (Html,*HSt)
mypage0 hst = mkHtml "Hi Folks!" [Big [] "Hello World"] hst


mkHtml :: String [BodyTag] *HSt -> (Html,*HSt)
mkHtml title body hst
   = ( Html (Head [] [Hd_Title title])
            (Body [] body)
     , hst
     )

//	Running example, using gHGEC:

mypage1 hst
   # ((list,body_list),hst) = gHGEC{|*|} /*"List"         */ HEdit    [1,5,2]              hst
     ((tree,body_tree),hst) = gHGEC{|*|} /*"Balanced Tree"*/ HDisplay (toBalanceTree list) hst
   = mkHtml "Balancing Tree From List"
            [ Br, Big [] "List:",          body_list
            , Hr []
            , Br, Big [] "Balanced Tree:", body_tree
            ] hst

//	Running example, using generatePage:

mypage2 hst
   # ((list,body_list),hst) = generatePage "List"          HEdit    [1,5,2]              hst
     ((tree,body_tree),hst) = generatePage "Balanced Tree" HDisplay (toBalanceTree list) hst
   = mkHtml "Balancing Tree From List"
            [ Br, Big [] "List:",          body_list
            , Hr []
            , Br, Big [] "Balanced Tree:", body_tree
            ] hst

//	Running example, using circuits:
mypage3 hst
   # ((_,[body_list,body_tree:_]),hst) = startCircuit mycircuit [1,5,2] hst
   = mkHtml "Balancing Tree From List"
            [ Br, Big [] "List:",          body_list
            , Hr []
            , Br, Big [] "Balanced Tree:", body_tree
            ] hst
where
	mycircuit = edit "List" >>> arr toBalanceTree >>> display "Balanced Tree"

toBalanceTree = fromListToBalTree

derive gHGEC []
derive gUpd  []
