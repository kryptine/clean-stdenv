definition module httpSubServer

import httpUtil

:: Socket :== Int;

//required functions
RegisterSubProcToServer :: !Int !Int !Int !String !String -> Int
WaitForMessageLoop :: ([String] Int Socket *World -> (Socket,*World)) Socket !*World -> *World

//helper-functions for sending (suggested to use one of these)
SendString :: !String !String ![String] !Socket !*World -> (Socket,*World)
SendFile :: String ![String] !Socket !*World -> (Socket,*World)

//helper-functions for receiving (optional to use one of these)
ReceiveString :: !Int !Int !Socket !*World -> (Int,String,Socket,*World)
ReceiveFile :: !Int !Socket !*File !*World -> (Bool,*File,*World)

//extra functions (do not use these unless you know what you are doing, read the RFC first)
SendDataToClient :: !Socket !{#Char} !*env -> (!Socket,*env);
HTTPdisconnectGracefulC :: !Socket !*env -> *env;
DetectHttpVersionAndClose :: !String !String !Socket !*World -> (Socket,*World)