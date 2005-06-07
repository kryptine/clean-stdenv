module helloworld

import StdEnv, StdHtml

Start world  = doHtml helloWorld world

helloWorld hst
= mkHtml "Hello World Example" [Txt "Hello World!"] hst
 