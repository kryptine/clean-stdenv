module IFL2005Examples

import StdEnv
import StdHtml

//Start world  = doHtml example1 world
Start world  = doHtmlServer example5 world

//  Example: display an integer editor:
example1 hst
    # (nrF,hst) = mkEditForm (nFormId "nr") (Init 1) hst
    = mkHtml "Int editor"
        [ H1 [] "Int editor"
        , STable [] [nrF.form]
        ] hst

//  Example: display a list of numbers vertically, and the sum of its values:
example2 hst
    # (nrFs,hst) = seqList [mkEditForm (sumId nr) (Init nr) \\ nr<-[1..5]] hst
    # sumNrs     = sum [nrF.value \\ nrF <- nrFs]
    = mkHtml "Sum of Numbers"
        [ H1 [] "Sum of Numbers"
        , STable [] ([nrF.form \\ nrF <- nrFs] ++ [[toHtml sumNrs]])
        ] hst

//  Example: display a list of numbers vertically, and the sum of its values.
//           Do this twice, to illustrate sharing.
example3 hst
    # (nrFs,hst) = seqList [mkEditForm (sumId nr) (Init nr) \\ nr<-[1..5]] hst
    # sumNrs     = sum [nrF.value \\ nrF <- nrFs]
    = mkHtml "Sum of Numbers"
        [ H1 [] "Sum of Numbers"
        , STable [] ([nrF.form ++ nrF.form  \\ nrF <- nrFs] ++ [[toHtml sumNrs,toHtml sumNrs]])
        ] hst


//  Example: display a list of numbers vertically, but use counter-editors instead of number-editors:
example4 hst
    # (nrFs,hst) = seqList [counterForm (sumId nr) (Init nr) \\ nr<-[1..5]] hst
    # sumNrs     = sum [nrF.value \\ nrF <- nrFs]
    = mkHtml "Sum of Numbers"
        [ H1 [] "Sum of Numbers"
        , STable [] ([nrF.form \\ nrF <- nrFs] ++ [[toHtml sumNrs]])
        ] hst

//  Example: display a list of numbers vertically, but use counter-editors instead of number-editors:
example5 hst
    # (nrFs,hst) = seqList [mkEditForm (sumId nr) (Init (M nr)) \\ nr<-[1..5]] hst
    # sumNrs     = sum [toInt nrF.value \\ nrF <- nrFs]
    = mkHtml "Sum of Numbers"
        [ H1 [] "Sum of Numbers"
        , STable [] ([nrF.form \\ nrF <- nrFs] ++ [[toHtml sumNrs]])
        ] hst

sumId i = nFormId ("sum"<$i)

//	Define new type to specialize 'Int':
::  MInt = M Int
derive gParse MInt
derive gPrint MInt
derive gUpd   MInt
gForm{|MInt|} formId i hst = specialize asCounter formId 
i hst
where
    asCounter formId (M i) hst
        # (counterF,hst)    = counterForm formId i hst
        = ({changed=counterF.changed,value=M (toInt counterF.value),form=counterF.form},hst)

instance toInt MInt where toInt (M i) = i
