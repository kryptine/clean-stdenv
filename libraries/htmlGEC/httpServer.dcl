definition module httpServer


// (c) 2005 Paul de Mast
// HIO - Breda - The Netherlands

// StartServer takes a port number + list of virtual pages

StartServer		:: Int [(String,(String String Arguments *World -> ([String],String,*World)))] *World -> *World

getArgValue		:: String Arguments -> String
getContentType	:: String -> String

:: Arguments	:== [(String, String)]
printArguments	:: Arguments -> String
