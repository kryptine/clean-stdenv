implementation module glib;

import gtk_types;

g_free :: !Int !GtkSt -> GtkSt;
g_free p gs = code {
	ccall g_free "p:V:p"
}
