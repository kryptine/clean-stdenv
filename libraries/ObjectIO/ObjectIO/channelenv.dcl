definition module channelenv

import	StdFile
from	StdTime	import TimeEnv, Date, Tick, Time
from	StdId	import Ids
import	id

class ChannelEnv env	| Ids env & TimeEnv env & FileEnv env
where
	channelEnvKind	::	!*env	-> (!Int, !*env)
	mb_close_inet_receiver_without_id :: !Bool !(!Int, !Int) !*env -> *env
//	::	!Bool  !(!EndpointRef, !InetReceiverCategory) !*env -> *env
//	mb_close_inet_receiver_without_id:
//		iff the Boolean is True, this function closes the receiver, which is identified through
//		the (!EndpointRef, !InetReceiverCategory) pair
					

//channelEnvKind can return the following values:
WORLD	:==	0
IOST	:==	1
PST		:==	2
