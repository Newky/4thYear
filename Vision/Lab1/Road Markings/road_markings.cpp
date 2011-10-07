#ifdef _CH_
#pragma package <opencv>
#endif

#include <stdio.h>
#include <stdlib.h>
#include "cv.h"
#include "highgui.h"
#include "../utilities.h"

#define THRESHOLD 200

/* Given a greyscale image
 * Generate a histogram for it
 * Simple function which loops through the image data,
 * increment the relevant index
 */
int* generate_hist(IplImage* source) {
	int row=0,col=0, i=0;
	int* hist = (int*)malloc(sizeof(int) * 256);
	for(;i<256;i++)
		hist[i] = 0;
	int width_step=source->widthStep;
	for(;row<source->height;row++) {
		for(;col<source->width;col++){
			unsigned char* curr_point = ((unsigned char *) source->imageData + (row)*(width_step) + (col));
			hist[*curr_point]++;
		};
	};
	return hist;
};
/* This function finds the
 * average color value of the pictures
 * histogram.
 */
int get_threshold(int* hist) {
	int i=0,sum=0, num=0;;
	for(;i<256;i++){
		if(hist[i] != 0){
			sum += i;
			num++;
		}
	}
	return (int)((double)sum / num);
};

/* Simple
 * Histogram Printer
 */
void print_hist(int* hist) {
	int i=0;
	for(;i<256;i++)
		printf("%d - %d\n", i, hist[i]);
}

// This routine creates a binary result image where the points are 255,255,255 when the corresponding
// source points are grey/white.  The rule for deciding which points are white/grey is very debatable.
// Should the minimum value be greater?  Should the ratio of max to min values in the point be allowed
// to vary more (or less)?
void select_white_points( IplImage* source, IplImage* result, int threshold)
{
	int width_step=source->widthStep;
	int pixel_step=source->widthStep/source->width;
	int number_channels=source->nChannels;
	unsigned char white_pixel[4] = {255,255,255,0};
	cvZero( result );
	int row=40,dec=50;
	for(;row<source->height;row++) {
		int col=dec;
		for(;col<source->width-dec;col++){
			unsigned char* curr_point = ((unsigned char *) source->imageData + (row)*(width_step) + (col));
			if(*curr_point >= threshold){
				PUTPIXELMACRO( result, col, row, white_pixel, width_step, pixel_step, number_channels );
			}	
		}
		if(dec > 0)
			dec --;
	}
};


int main( int argc, char** argv )
{
    IplImage* img = 0;
    IplImage* res= 0;
    IplImage* tmp= 0;
 
    CvCapture* capture = cvCaptureFromAVI("StayingInLane.avi");
    int fps = (int) cvGetCaptureProperty(capture, CV_CAP_PROP_FPS);

    int user_clicked_key = 0;
    //if(!cvGrabFrame(capture)) {
	//printf("Error in capturing frame");
	//exit(0);
    //}

    //img = cvRetrieveFrame(capture);
    //tmp= cvCreateImage( cvSize(img->width, img->height), IPL_DEPTH_8U, 1);
    //cvCvtColor(img, tmp, CV_RGB2GRAY);

    //int * hist = generate_hist(tmp);
    //int threshold =  get_threshold(hist) * 1.5;
    //res = cvCloneImage(tmp);
    //select_white_points(tmp, res, threshold);
    cvNamedWindow( "Original", 1 );
    cvNamedWindow( "Result", 1 );
    //cvShowImage("Original", img);
    //cvShowImage("Result", res);

    while(cvGrabFrame(capture) && user_clicked_key != ESC) {
	    int * hist = generate_hist(tmp);
	    int threshold =  get_threshold(hist) *1.5;
	    img = cvRetrieveFrame(capture);
	    tmp= cvCreateImage( cvSize(img->width, img->height), IPL_DEPTH_8U, 1);
	    cvCvtColor(img, tmp, CV_RGB2GRAY);
	    select_white_points(tmp, res, threshold);
	    cvShowImage("Original", img);
	    cvShowImage("Result", res);
	    free(hist);
	    user_clicked_key = cvWaitKey(1000/fps);
    };
    return 0;
};
