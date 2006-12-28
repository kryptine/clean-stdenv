module sum

import StdEnv, StdHtml

// choose one of the following variants

Start world = doHtmlServer (singleUserTask count) world
// Start world = doHtmlServer (multiUserTask 3 [] countMU) world
// Start world = doHtmlServer (multiUserTask 3 [setTaskAttribute Persistent] countMU) world

count tst
# (v1,tst) 	= STask "Set" initVal tst
# (v2,tst) 	= STask "Set" initVal tst
# tst		= returnF [Hr []] tst
= returnTask (v1 + v2) tst

countMU tst
# (v1,tst) 	= ((1,"number") @: STask "Set" initVal) tst	// user 1
# (v2,tst) 	= ((2,"number") @: STask "Set" initVal) tst	// user 2
# tst		= returnF [Hr []] tst						// user 0
= returnTask (v1 + v2) tst								// user 0

// Change the type to any type one can apply addition to

initVal :: Int
initVal = createDefault

