implementation module iDataSettings

import iDataHandler, ArgEnv, StdList, iDataTrivial, StdArray


// global names setting depending on kind of server used

ThisExe ::  String
ThisExe = myName 
//ThisExe = "clean" 

myName =: toString (reverse (takeWhile (\c -> (<>) c '\\') (drop 4 (reverse [x \\ x <-: getCommandLine.[0]])))) 

MyDir ::  String
MyDir  =: mkString (takeWhile ((<>) '.') (mkList ThisExe))

MyAbsDir ::  String
MyAbsDir  =:  toString (reverse (dropWhile (\c -> (<>) c '\\') (drop 4 (reverse [x \\ x <-: getCommandLine.[0]]))))

instance == ServerType
where
	(==) Internal Internal = True	
	(==) External External = True
	(==) TestMode TestMode = True
	(==) _ _ = False
	