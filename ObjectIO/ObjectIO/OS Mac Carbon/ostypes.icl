implementation module ostypes

:: OSPictContext :== Int	// GrafPort
::	Ptr :== Int;
::	Handle :== Int;
::	OSRect =
	{ rleft :: !Int
	, rtop :: !Int
	, rright :: !Int
	, rbottom :: !Int};

//::	Rect :== (!Int,!Int,!Int,!Int);
//::	RgnHandle :== Int;
::	OSWindowPtr :== Int;
::	DialogPtr :== Int;
::	MacMenuHandle :== Int;

::	Toolbox :== Int;

NewToolbox :: *Toolbox;
NewToolbox = 0;

OSNoWindowPtr :== 0

::	DelayActivationInfo
	=	DelayActivatedWindow	OSWindowPtr				// the window has become active
	|	DelayDeactivatedWindow	OSWindowPtr				// the window has become inactive
	|	DelayActivatedControl	OSWindowPtr OSWindowPtr	// the control (@2) in window (@1) has become active
	|	DelayDeactivatedControl	OSWindowPtr OSWindowPtr	// the control (@2) in window (@1) has become inactive

