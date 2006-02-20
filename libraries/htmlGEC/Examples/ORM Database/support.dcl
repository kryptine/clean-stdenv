definition module support

import StdHtml

Alert :: ((Bool,String) -> (Bool,String)) -> (*HSt -> *((Form (Bool,String)),*HSt))

testUnique:: [String] -> (String,Bool)

generic gCollect a :: a [String] -> [String]

derive gCollect Int, Real, Char, Bool, String, PAIR, EITHER, CONS, FIELD, OBJECT, UNIT, {}

