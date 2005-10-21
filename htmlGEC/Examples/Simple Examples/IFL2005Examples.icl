module IFL2005Examples

import StdEnv
import StdHtml

//Start world  = doHtml example1 world
Start world  = doHtmlServer example6 world

//	Example: display an integer editor:
example0 hst
	# (nrF,hst) = mkEditForm (nFormId "nr") 1 hst
	= mkHtml "Int editor"
		[ H1 [] "Int editor"
		, column [nrF.form]
		] hst

//	Example: illustrate sharing of editors:
example1 hst
	# (nrF,hst)	= mkEditForm (nFormId "nr") 1 hst
	= mkHtml "Sharing of number editors"
		[ H1 [] "Sharing of number editors"
		, column (repeatn nr nrF.form)
		] hst
where
	nr	= 5

//	Example: illustrate sharing of editors, and use value:
example2 hst
	# (nrF,hst)	= mkEditForm (nFormId "nr") 1 hst
	= mkHtml "Sharing of number editors"
		[ H1 [] "Sharing of number editors"
		, column (repeatn nr nrF.form ++ [[toHtml (nr*nrF.value)]])
		] hst
where
	nr	= 5

//	Example: display a list of numbers vertically, and the sum of its values:
example3 hst
	# (nrListFs,hst) = seqList [mkEditForm (nFormId ("sum"<$i)) nr \\ nr<-numbers 5 & i<-[0..]] hst
	= mkHtml "Sum of Numbers"
		[ H1 [] "Sum of Numbers"
		, column      [form  \\ {form}  <- nrListFs]
		, toHtml (sum [value \\ {value} <- nrListFs])
		] hst


//	Example: display a list of numbers vertically, but use counter-editors instead of number-editors:
example4 hst
	# (nrListFs,hst) = seqList [counterForm (nFormId ("sum"<$i)) nr \\ nr<-numbers 5 & i<-[0..]] hst
	= mkHtml "Sum of Numbers"
		[ H1 [] "Sum of Numbers"
		, column      [form  \\ {form}  <- nrListFs]
		, toHtml (sum [value \\ {value} <- nrListFs])
		] hst

//	Example: display a list of numbers vertically, but use counter-editors instead of number-editors:
example5 hst
	# (nrListFs,hst) = seqList [mkEditForm (nFormId ("sum"<$i)) (Counter nr) \\ nr<-numbers 5 & i<-[0..]] hst
	= mkHtml "Sum of Numbers"
		[ H1 [] "Sum of Numbers"
		, column      [form  \\ {form}  <- nrListFs]
		, toHtml (sum [toInt value \\ {value} <- nrListFs])
		] hst

//	Example: display a list of numbers vertically, but use counter-editors instead of number-editors:
example6 hst
	# (nrListFs,hst) = seqList [mkEditForm (nFormId ("sum"<$i)) (M nr) \\ nr<-numbers 5 & i<-[0..]] hst
	= mkHtml "Sum of Numbers"
		[ H1 [] "Sum of Numbers"
		, column      [form  \\ {form}  <- nrListFs]
		, toHtml (sum [toInt value \\ {value} <- nrListFs])
		] hst

numbers :: Int -> [Int]
numbers	n = repeatn n zero

column = STable []

::	Counter = Counter Int
derive gParse Counter
derive gPrint Counter
derive gUpd   Counter

instance toInt Counter where toInt (Counter i) = i
instance toInt MInt    where toInt (M       i) = i

gForm{|Counter|} formId counter hst
	= specialize asCounter formId counter hst
where
	asCounter formId (Counter i) hst
		# (counterF,hst)	= counterForm formId i hst
		= ({changed=counterF.changed,value=Counter counterF.value,form=counterF.form},hst)
gForm{|MInt|} formId i hst
	= specialize asCounter formId i hst
where
	asCounter formId (M i) hst
		# (counterF,hst)	= counterForm formId i hst
		= ({changed=counterF.changed,value=M (toInt counterF.value),form=counterF.form},hst)
::	MInt = M Int
derive gParse MInt
derive gPrint MInt
derive gUpd   MInt
