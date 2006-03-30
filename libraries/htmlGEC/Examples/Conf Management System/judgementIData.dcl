definition module judgementIData

import StdHtml, judgement

// temporal store to store judgements, handy for exception handling

ExceptionStore :: (Judgement -> Judgement) *HSt -> (Judgement,!*HSt)
