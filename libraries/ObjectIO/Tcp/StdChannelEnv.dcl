definition module StdChannelEnv

//	********************************************************************************
//	Clean Standard Object I/O library, version 1.2
//	
//	StdChannelEnv defines instances of ChannelEnv for PSt and IOSt.
//	********************************************************************************

import	channelenv
import	StdPStClass
from	iostate	import PSt, IOSt

instance ChannelEnv (PSt  .l)
instance ChannelEnv (IOSt .l)
