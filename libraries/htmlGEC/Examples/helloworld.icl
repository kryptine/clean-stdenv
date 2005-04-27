module helloworld

import StdEnv, StdHtml

Start world  = doHtml helloWorld world
helloWorld hst
= (Html (Head [] []) (Body [] [mybody]),hst)
where
	mybody = Txt "Hello World!"
 