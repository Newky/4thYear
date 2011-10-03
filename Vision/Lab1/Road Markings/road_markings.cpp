#ifdef _CH_
#pragma package <opencv>
#endif

#include <stdio.h>
#include <stdlib.h>
#include "cv.h"
#include "highgui.h"
#include "../utilities.h"

#define THRESHOLD 200


double stdev(unsigned char* rgb) {
	int mean= ((double)(rgb[RED_CH] + rgb[GREEN_CH] + rgb[BLUE_CH]))/ 3;
	return (((rgb[RED_CH] - mean)*(rgb[RED_CH] - mean))
		+
		((rgb[GREEN_CH] - mean)*(rgb[GREEN_CH] - mean))
		+
		((rgb[BLUE_CH] - mean)*(rgb[BLUE_CH] - mean))) / 3;
};	

int in_range(unsigned char* point) {
	return (stdev(point) > 80);
};

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
	cvZero( result );
	int row=0, col;
	for(row=0;row<result->height;row++) {
		for(col=0;col<result->width;col++){
			unsigned char* curr_point = GETPIXELPTRMACRO( source, col, row, width_step, pixel_step );
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
    IplImage* tmp= 0;
 
    CvCapture* capture = cvCaptureFromAVI("StayingInLane.avi");
    int fps = (int) cvGetCaptureProperty(capture, CV_CAP_PROP_FPS);
    printf("%d\n", fps);

    int user_clicked_key = 0;
    if(!cvGrabFrame(capture)) {
	printf("Error in capturing frame");
	exit(0);
    }

    img = cvRetrieveFrame(capture);
    tmp= cvCreateImage( cvSize(img->width, img->height), IPL_DEPTH_8U, 1);
    cvCvtColor(img, tmp, CV_RGB2GRAY);
    res = cvCloneImage(tmp);
    select_white_points(tmp, res);

    cvNamedWindow( "Original", 1 );
    cvNamedWindow( "Result", 1 );
    cvShowImage("Original", img);
    cvShowImage("Result", res);

    //while ( user_clicked_key != ESC ) {
    while(cvGrabFrame(capture)) {
    	    img = cvRetrieveFrame(capture);
    	    tmp= cvCreateImage( cvSize(img->width, img->height), IPL_DEPTH_8U, 1);
    	    cvCvtColor(img, tmp, CV_RGB2GRAY);
    	    res = cvCloneImage(tmp);
    	    select_white_points(tmp, res);
    	    cvShowImage("Original", img);
    	    cvShowImage("Result", res);
	    user_clicked_key = cvWaitKey(1000/fps);
    }
    return 0;
};
