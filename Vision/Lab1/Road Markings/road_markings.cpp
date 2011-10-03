#ifdef _CH_
#pragma package <opencv>
#endif

#include <stdio.h>
#include <stdlib.h>
#include "cv.h"
#include "highgui.h"
#include "../utilities.h"

#define THRESHOLD 200

int in_range(unsigned char* point) {
	int mean= ((double)(point[RED_CH] + point[GREEN_CH] + point[BLUE_CH]))/ 3;
	double stdev= ( (point[RED_CH] - mean) +
			(point[GREEN_CH] - mean) +
			(point[BLUE_CH] - mean))/3;
	return (stdev < 50);
}
	//// This routine creates a binary result image where the points are 255,255,255 when the corresponding
	//// source points are grey/white.  The rule for deciding which points are white/grey is very debatable.
	//// Should the minimum value be greater?  Should the ratio of max to min values in the point be allowed
	//// to vary more (or less)?
void select_white_points( IplImage* source, IplImage* result )
{
	int width_step=source->widthStep;
	int pixel_step=source->widthStep/source->width;
	int number_channels=source->nChannels;
	unsigned char white_pixel[4] = {255,255,255,0};
	int sum[3] = {0,0,0};
	cvZero( result );
	int row=0, col;
	for(row=0;row<result->height;row++) {
		for(col=0;col<result->width;col++){
			unsigned char* curr_point = GETPIXELPTRMACRO( source, col, row, width_step, pixel_step );
			//if(curr_point[RED_CH] + curr_point[BLUE_CH] + curr_point[GREEN_CH] >= 250){a
			if(in_range(curr_point)){
				PUTPIXELMACRO( result, col, row, white_pixel, width_step, pixel_step, number_channels );
			}
		}
	}

	//// Apply morphological opening and closing operations to clean up the image
	//cvMorphologyEx( result, result, NULL, NULL, CV_MOP_OPEN, 3 );
	//cvMorphologyEx( result, result, NULL, NULL, CV_MOP_CLOSE, 3 );
}

void grey_scale(IplImage* src, IplImage* result) {
	result = cvCreateImage( cvSize(src->width, src->height), IPL_DEPTH_8U, 1);
	cvCvtColor(src, result, CV_RGB2GRAY);
}


int main( int argc, char** argv )
{
    IplImage* img = 0;
    IplImage* res= 0;
    CvCapture* capture = cvCaptureFromAVI("StayingInLane.avi");
    int user_clicked_key = 0;
    if(!cvGrabFrame(capture)) {
	printf("Error in capturing frame");
	exit(0);
    }
    img = cvRetrieveFrame(capture);
    res = cvCloneImage(img);
    select_white_points(img, res);
    cvNamedWindow( "Original", 1 );
    cvNamedWindow( "Result", 1 );
    cvShowImage("Original", img);
    cvShowImage("Result", res);

    while ( user_clicked_key != ESC ) {
	    user_clicked_key = cvWaitKey(0);
    }
    return 0;
};
