implementation module odbccp;

import odbc;
import code from library "odbccp_library";

SQLConfigDataSource :: !Int !Int !{#Char} !{#Char} !*SqlState -> (!Int, !*SqlState);
SQLConfigDataSource hwndParent fRequest lpszDriver lpszAttributes sql_state
	= (SQLConfigDataSource_ hwndParent fRequest lpszDriver lpszAttributes, sql_state);

SQLConfigDataSource_ :: !Int !Int !{#Char} !{#Char} -> Int;
SQLConfigDataSource_ hwndParent fRequest lpszDriver lpszAttributes = code {
	ccall SQLConfigDataSource@16 "PIIss:I"
}
