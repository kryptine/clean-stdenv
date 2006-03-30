implementation module judgementIData

import StdHtml, judgement

ExceptionStore :: (Judgement -> Judgement) *HSt -> (Judgement,!*HSt)
ExceptionStore judge hst 
# (judgef,hst) = mkStoreForm (Init,{nFormId "cfm_exception" OK & mode = NoForm, lifespan = Temp}) judge hst
= (judgef.value,hst)
