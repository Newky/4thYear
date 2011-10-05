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
	return (stdev(point) > 81);
};

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

int get_threshold_wrong(int* hist) {
	int i=0, max = 0, index = 0;
	for(;i<256;i++){
		if(hist[i] > max)
			index= i;
	}
	return index;
};

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
			//unsigned char curr_point = GETPIXELPTRMACRO( source, col, row, width_step, pixel_step );
			unsigned char* curr_point = ((unsigned char *) source->imageData + (row)*(width_step) + (col));
			if(*curr_point >= threshold){
				PUTPIXELMACRO( result, col, row, white_pixel, width_step, pixel_step, number_channels );
			}	
		}
		if(dec > 0)
			dec --;
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

    int user_clicked_key = 0;
    if(!cvGrabFrame(capture)) {
	printf("Error in capturing frame");
	exit(0);
    }

    img = cvRetrieveFrame(capture);
    tmp= cvCreateImage( cvSize(img->width, img->height), IPL_DEPTH_8U, 1);
    cvCvtColor(img, tmp, CV_RGB2GRAY);

    int * hist = generate_hist(tmp);
    int threshold = (double) get_threshold(hist) / 2;
    res = cvCloneImage(tmp);
    select_white_points(tmp, res, threshold);
    cvNamedWindow( "Original", 1 );
    cvNamedWindow( "Result", 1 );
    cvShowImage("Original", img);
    cvShowImage("Result", res);

    while(cvGrabFrame(capture) && user_clicked_key != ESC) {
	    int * hist = generate_hist(tmp);
	    int threshold =  get_threshold(hist) + 20;
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
