implementation module ostick

// to be placed in something bigger later

import StdEnv
import events

::	Tick	:== Int

pack_tick	::	!Int -> Tick
pack_tick i = i

unpack_tick	::	!Tick -> Int
unpack_tick tick = tick

os_getcurrenttick :: !*World -> (!Tick, !*World)
os_getcurrenttick world
	= (fst (TickCount 42), world)
