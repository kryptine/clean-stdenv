implementation module xpicture;

StartXDrawing :: !Int -> Int;
StartXDrawing _
	= code {
		.inline StartXDrawing
			ccall start_drawing "I-I"
		.end
	};

EndXDrawing :: !Int -> Int;
EndXDrawing _
	= code {
		.inline EndXDrawing
			ccall end_drawing "I-I"
		.end
	};

HidePenX :: !Int -> Int;
HidePenX _
	= code {
		.inline HidePenX
			ccall hide_pen "I-I"
		.end
	};

ShowPenX :: !Int -> Int;
ShowPenX _
	= code {
		.inline ShowPenX
			ccall show_pen "I-I"
		.end
	};

GetPenX :: !Int -> (!Int,!Int);
GetPenX _
	= code {
		.inline GetPenX
			ccall get_pen "I-II"
		.end
	};

PenSizeX :: !Int !Int !Int -> Int;
PenSizeX _ _ _
	= code {
		.inline PenSizeX
			ccall pen_size "III-I"
		.end
	};

PenModeX :: !Int !Int -> Int;
PenModeX _ _
	= code {
		.inline PenModeX
			ccall pen_mode "II-I"
		.end
	};

PenPatternX :: !Int !Int -> Int;
PenPatternX _ _
	= code {
		.inline PenPatternX
			ccall pen_pattern "II-I"
		.end
	};

PenNormalX :: !Int -> Int;
PenNormalX _
	= code {
		.inline PenNormalX
			ccall pen_normal "I-I"
		.end
	};

MoveToX :: !Int !Int !Int -> Int;
MoveToX _ _ _
	= code {
		.inline MoveToX
			ccall move_to "III-I"
		.end
	};

MoveRelativeX :: !Int !Int !Int -> Int;
MoveRelativeX _ _ _
	= code {
		.inline MoveRelativeX
			ccall move_relative "III-I"
		.end
	};

LineToX :: !Int !Int !Int -> Int;
LineToX _ _ _
	= code {
		.inline LineToX
			ccall line_to "III-I"
		.end
	};

LineRelativeX :: !Int !Int !Int -> Int;
LineRelativeX _ _ _
	= code {
		.inline LineRelativeX
			ccall line_relative "III-I"
		.end
	};

DrawStringX :: !{#Char} !Int -> Int;
DrawStringX _ _
	= code {
		.inline DrawStringX
			ccall draw_string "SI-I"
		.end
	};

GetColorX :: !Int -> Int;
GetColorX _
	= code {
		.inline GetColorX
			ccall get_color "I-I"
		.end
	};

ForegroundColorX :: !Int !Int -> Int;
ForegroundColorX _ _
	= code {
		.inline ForegroundColorX
			ccall foreground_color "II-I"
		.end
	};

BackgroundColorX :: !Int !Int -> Int;
BackgroundColorX _ _
	= code {
		.inline BackgroundColorX
			ccall background_color "II-I"
		.end
	};

RGBForegroundColorX :: !Real !Real !Real !Int -> Int;
RGBForegroundColorX _ _ _ _
	= code {
		.inline RGBForegroundColorX
			ccall rgb_fg_color "RRRI-I"
		.end
	};

RGBBackgroundColorX :: !Real !Real !Real !Int -> Int;
RGBBackgroundColorX _ _ _ _
	= code {
		.inline RGBBackgroundColorX
			ccall rgb_bg_color "RRRI-I"
		.end
	};

DrawLineX :: !Int !Int !Int !Int !Int -> Int;
DrawLineX _ _ _ _ _
	= code {
		.inline DrawLineX
			ccall draw_line "IIIII-I"
		.end
	};

DrawPointX :: !Int !Int !Int -> Int;
DrawPointX _ _ _
	= code {
		.inline DrawPointX
			ccall draw_point "III-I"
		.end
	};

FrameRectangleX :: !Int !Int !Int !Int !Int -> Int;
FrameRectangleX _ _ _ _ _
	= code {
		.inline FrameRectangleX
			ccall frame_rectangle "IIIII-I"
		.end
	};

PaintRectangleX :: !Int !Int !Int !Int !Int -> Int;
PaintRectangleX _ _ _ _ _
	= code {
		.inline PaintRectangleX
			ccall paint_rectangle "IIIII-I"
		.end
	};

EraseRectangleX :: !Int !Int !Int !Int !Int -> Int;
EraseRectangleX _ _ _ _ _
	= code {
		.inline EraseRectangleX
			ccall erase_rectangle "IIIII-I"
		.end
	};

InvertRectangleX :: !Int !Int !Int !Int !Int -> Int;
InvertRectangleX _ _ _ _ _
	= code {
		.inline InvertRectangleX
			ccall invert_rectangle "IIIII-I"
		.end
	};

MoveRectangleX :: !Int !Int !Int !Int !Int !Int !Int -> Int;
MoveRectangleX _ _ _ _ _ _ _
	= code {
		.inline MoveRectangleX
			ccall move_rectangle "IIIIIII-I"
		.end
	};

CopyRectangleX :: !Int !Int !Int !Int !Int !Int !Int -> Int;
CopyRectangleX _ _ _ _ _ _ _
	= code {
		.inline CopyRectangleX
			ccall copy_rectangle "IIIIIII-I"
		.end
	};

FrameRoundRectangleX :: !Int !Int !Int !Int !Int !Int !Int -> Int;
FrameRoundRectangleX _ _ _ _ _ _ _
	= code {
		.inline FrameRoundRectangleX
			ccall frame_round_rectangle "IIIIIII-I"
		.end
	};

PaintRoundRectangleX :: !Int !Int !Int !Int !Int !Int !Int -> Int;
PaintRoundRectangleX _ _ _ _ _ _ _
	= code {
		.inline PaintRoundRectangleX
			ccall paint_round_rectangle "IIIIIII-I"
		.end
	};

EraseRoundRectangleX :: !Int !Int !Int !Int !Int !Int !Int -> Int;
EraseRoundRectangleX _ _ _ _ _ _ _
	= code {
		.inline EraseRoundRectangleX
			ccall erase_round_rectangle "IIIIIII-I"
		.end
	};

InvertRoundRectangleX :: !Int !Int !Int !Int !Int !Int !Int -> Int;
InvertRoundRectangleX _ _ _ _ _ _ _
	= code {
		.inline InvertRoundRectangleX
			ccall invert_round_rectangle "IIIIIII-I"
		.end
	};

FrameOvalX :: !Int !Int !Int !Int !Int -> Int;
FrameOvalX _ _ _ _ _
	= code {
		.inline FrameOvalX
			ccall frame_oval "IIIII-I"
		.end
	};

PaintOvalX :: !Int !Int !Int !Int !Int -> Int;
PaintOvalX _ _ _ _ _
	= code {
		.inline PaintOvalX
			ccall paint_oval "IIIII-I"
		.end
	};

EraseOvalX :: !Int !Int !Int !Int !Int -> Int;
EraseOvalX _ _ _ _ _
	= code {
		.inline EraseOvalX
			ccall erase_oval "IIIII-I"
		.end
	};

InvertOvalX :: !Int !Int !Int !Int !Int -> Int;
InvertOvalX _ _ _ _ _
	= code {
		.inline InvertOvalX
			ccall invert_oval "IIIII-I"
		.end
	};

FrameArcX :: !Int !Int !Int !Int !Int !Int !Int -> Int;
FrameArcX _ _ _ _ _ _ _
	= code {
		.inline FrameArcX
			ccall frame_arc "IIIIIII-I"
		.end
	};

PaintArcX :: !Int !Int !Int !Int !Int !Int !Int -> Int;
PaintArcX _ _ _ _ _ _ _
	= code {
		.inline PaintArcX
			ccall paint_arc "IIIIIII-I"
		.end
	};

EraseArcX :: !Int !Int !Int !Int !Int !Int !Int -> Int;
EraseArcX _ _ _ _ _ _ _
	= code {
		.inline EraseArcX
			ccall erase_arc "IIIIIII-I"
		.end
	};

InvertArcX :: !Int !Int !Int !Int !Int !Int !Int -> Int;
InvertArcX _ _ _ _ _ _ _
	= code {
		.inline InvertArcX
			ccall invert_arc "IIIIIII-I"
		.end
	};

AllocatePolygonX :: !Int -> Int;
AllocatePolygonX _
	= code {
		.inline AllocatePolygonX
			ccall alloc_polygon "I-I"
		.end
	};

FreePolygonX :: !Int !Int -> Int;
FreePolygonX _ _
	= code {
		.inline FreePolygonX
			ccall free_polygon "II-I"
		.end
	};

SetPolygonPointX :: !Int !Int !Int !Int -> Int;
SetPolygonPointX _ _ _ _
	= code {
		.inline SetPolygonPointX
			ccall set_polygon_point "IIII-I"
		.end
	};

FramePolygonX :: !Int !Int !Int !Int !Int -> Int;
FramePolygonX _ _ _ _ _
	= code {
		.inline FramePolygonX
			ccall frame_polygon "IIIII-I"
		.end
	};

PaintPolygonX :: !Int !Int !Int !Int !Int -> Int;
PaintPolygonX _ _ _ _ _
	= code {
		.inline PaintPolygonX
			ccall paint_polygon "IIIII-I"
		.end
	};

ErasePolygonX :: !Int !Int !Int !Int !Int -> Int;
ErasePolygonX _ _ _ _ _
	= code {
		.inline ErasePolygonX
			ccall erase_polygon "IIIII-I"
		.end
	};

InvertPolygonX :: !Int !Int !Int !Int !Int -> Int;
InvertPolygonX _ _ _ _ _
	= code {
		.inline InvertPolygonX
			ccall invert_polygon "IIIII-I"
		.end
	};

GetNumberOfFontsX :: !Int -> Int;
GetNumberOfFontsX _
	= code {
		.inline GetNumberOfFontsX
			ccall get_number_fonts "I-I"
		.end
	};

GetFontNameX :: !Int -> {#Char};
GetFontNameX _
	= code {
		.inline GetFontNameX
			ccall get_font_name "I-S"
		.end
	};

GetFontInfoX :: !Int -> (!Int,!Int,!Int,!Int,!Int);
GetFontInfoX _
	= code {
		.inline GetFontInfoX
			ccall get_font_info "I-IIIII"
		.end
	};

GetFontFontInfoX :: !Int -> (!Int,!Int,!Int,!Int);
GetFontFontInfoX _
	= code {
		.inline GetFontFontInfoX
			ccall get_font_font_info "I-IIII"
		.end
	};

GetStringWidthX :: !Int !{#Char} -> (!Int,!Int);
GetStringWidthX _ _
	= code {
		.inline GetStringWidthX
			ccall get_string_width "IS-II"
		.end
	};

GetFontStringWidthX :: !Int !{#Char} -> Int;
GetFontStringWidthX _ _
	= code {
		.inline GetFontStringWidthX
			ccall get_font_string_width "IS-I"
		.end
	};

SetFontX :: !Int !Int !{#Char} !{#Char} !{#Char} -> Int;
SetFontX _ _ _ _ _
	= code {
		.inline SetFontX
			ccall set_font "IISSS-I"
		.end
	};

SetFontNameX :: !Int !{#Char} -> Int;
SetFontNameX _ _
	= code {
		.inline SetFontNameX
			ccall set_font_name "IS-I"
		.end
	};

SetStyleX :: !Int !{#Char} -> Int;
SetStyleX _ _
	= code {
		.inline SetStyleX
			ccall set_font_style "IS-I"
		.end
	};

SetSizeX :: !Int !{#Char} -> Int;
SetSizeX _ _
	= code {
		.inline SetSizeX
			ccall set_font_size "IS-I"
		.end
	};

SelectDefaultFontX :: !Int -> Int;
SelectDefaultFontX _
	= code {
		.inline SelectDefaultFontX
			ccall select_default_font "I-I"
		.end
	};

SelectFontX :: !{#Char} -> Int;
SelectFontX _
	= code {
		.inline SelectFontX
			ccall select_font "S-I"
		.end
	};

GetFontStylesX :: !{#Char} -> (!Int,!Int,!Int,!Int,!Int);
GetFontStylesX _
	= code {
		.inline GetFontStylesX
			ccall get_font_styles "S-IIIII"
		.end
	};

GetFontSizesX :: !{#Char} -> Int;
GetFontSizesX _
	= code {
		.inline GetFontSizesX
			ccall get_font_sizes "S-I"
		.end
	};

GetOneFontSizeX :: !Int -> Int;
GetOneFontSizeX _
	= code {
		.inline GetOneFontSizeX
			ccall get_one_font_size "I-I"
		.end
	};

