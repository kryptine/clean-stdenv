system module xkernel;

InitToplevelX :: !Int -> Int;
SetToplevelNameX :: !{#Char} -> Int;
CloseToplevelX :: !Int -> Int;
OpenToplevelX :: !Int -> Int;
ShowToplevelX :: !Int -> Int;
HideToplevelX :: !Int -> Int;
CatchXWidget :: !Int -> (!Int,!Int);
DestroyWidget :: !Int -> Int;
