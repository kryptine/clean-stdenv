#include "util_12.h"
#include "intrface_12.h"
#include <math.h>


void InitPicture( HDC hdc );
void DonePicture( HDC hdc );

extern void SetLogFontData (LOGFONT*, char*, int, int);
static int PointsToPix(HDC hdc, int size);


