definition module ESMVizTool

import StdiData
import ESMSpec

/*
esmVizTool :: !(ESM s i o) *HSt -> ((Bool,String),Html,*HSt)
					| iData, gEq{|*|}, render s 
                    & iData, gEq{|*|}, render, ggen{|*|} i 
                    & iData, gEq{|*|}, render o
*/
esmVizTool :: !(ESM s i o) *World -> *World | iData, gEq{|*|}, render s & iData, gEq{|*|}, render, ggen{|*|} i & iData, gEq{|*|}, render o

toHtmlString :: a -> String | gPrint{|*|} a
