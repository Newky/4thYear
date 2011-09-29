#ifdef _CH_
#pragma package <opencv>
#endif

#include <stdio.h>
#include <stdlib.h>
#include "cv.h"
#include "highgui.h"
#include "../utilities.h"

// This routine creates a binary result image where the points are 255,255,255 when the corresponding
// source points are grey/white.  The rule for deciding which points are white/grey is very debatable.
// Should the minimum value be greater?  Should the ratio of max to min values in the point be allowed
// to vary more (or less)?
void select_white_points( IplImage* source, IplImage* result )
{
}

int main( int argc, char** argv )
{
    return 0;
}