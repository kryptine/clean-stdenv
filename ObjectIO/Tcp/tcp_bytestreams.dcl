definition module tcp_bytestreams

import	StdTCPDef

::	*TCP_RCharStream_ char
	=	{	rbs_rchan	::	!TCP_RChannel
		,	rbs_buffer	::	!{#Char}
		,	rbs_index	::	!Int
		}

::	*TCP_SCharStream_ char
	=	{	sbs_schan	::	!TCP_SChannel
		}
