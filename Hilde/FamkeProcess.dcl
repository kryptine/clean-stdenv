definition module FamkeProcess

from FamkeKernel import :: FamkePort, processId

:: ProcessId :== Int

newProcess :: !(*World -> *World) !*World -> (!ProcessId, !*World)
reuseProcess :: !ProcessId !(*World -> *World) !*World -> *World
joinProcess :: !ProcessId !*World -> *World
killProcess :: !ProcessId !*World -> *World
shutdown :: !*World -> *World

reservePort :: !*World -> (!FamkePort .a .b, !*World)
freePort :: !(FamkePort .a .b) !*World -> *World

StartProcess :: !(*World -> *World) !*World -> *World
