definition module FamkeProcess

from FamkeKernel import :: Famke, :: FamkePort, processId

:: ProcessId :== Int

newProcess :: !(*Famke -> *Famke) !*Famke -> (!ProcessId, !*Famke)
reuseProcess :: !ProcessId !(*Famke -> *Famke) !*Famke -> *Famke
joinProcess :: !ProcessId !*Famke -> *Famke
killProcess :: !ProcessId !*Famke -> *Famke
shutdown :: !*Famke -> *Famke

reservePort :: !*Famke -> (!FamkePort .a .b, !*Famke)
freePort :: !(FamkePort .a .b) !*Famke -> *Famke

StartProcess :: !(*Famke -> *Famke) !*World -> *World
