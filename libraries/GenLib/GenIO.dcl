definition module GenIO

import StdMaybe
import StdGeneric

:: Pos 

//--------------------------------------------------------------------

class OStream s where
	streamWrite :: Char *s -> *s 

class IStream s where
	streamRead :: *s -> (Maybe Char, *s) 
	streamEnd :: *s -> (Bool, *s)

// stream with positioning
class RandomStream s where	
	streamGetPos :: *s -> (Pos, *s)
	streamSetPos :: Pos *s -> *s

//--------------------------------------------------------------------

:: *StringStream

instance OStream StringStream, File
instance IStream StringStream, File
instance RandomStream StringStream, File


//--------------------------------------------------------------------
:: GenIOEnv // abstract environement

generic gOutput a :: GenIOEnv -> (a -> (*s -> *s)) | IStream s & RandomStream s
generic gInput a :: GenIOEnv -> (*s -> (Maybe a, *s)) | IStream s & RandomStream s
