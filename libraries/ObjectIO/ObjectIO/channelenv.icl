implementation module channelenv

import	StdFile
import	id
import	StdTime
from	StdId import Ids

class ChannelEnv env	| Ids env & TimeEnv env & FileEnv env
where
	channelEnvKind	::	!*env	-> (!Int, !*env)
	mb_close_inet_receiver_without_id :: !Bool !(!Int, !Int) !*env -> *env
	
//channelEnvKind can return the following values:
// (some C functions rely on these values)
WORLD	:==	0
IOST	:==	1
PST		:==	2
