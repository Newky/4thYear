#ifdef _CH_
#pragma package <opencv>
#endif

#include <stdio.h>
#include <stdlib.h>
#include "cv.h"
#include "highgui.h"
#include "../utilities.h"
#define ALPHA 0.005
#define ONEMINUS 0.995
#define K_VAL 3


void update_running_gaussian_averages( IplImage *current_frame, IplImage *averages_image, IplImage *stan_devs_image )
{
	// TO-DO:  Update the average and standard deviation for each channel for each pixel based on the values in the
	// current_frame
	int width_step=current_frame->widthStep;
	int pixel_step=current_frame->widthStep/current_frame->width;
	int width_step2=averages_image->widthStep;
	int pixel_step2=averages_image->widthStep/averages_image->width;
	int number_channels2=current_frame->nChannels;
	int row=0,col=0;

	for(;row<current_frame->height;row++){
		for(col=0;col<current_frame->width;col++){
			unsigned char* curr_point = GETPIXELPTRMACRO(current_frame, col, row, width_step, pixel_step );	
			float *average_point= (float *) GETPIXELPTRMACRO(averages_image, col, row, width_step2, pixel_step2 );	
			float *std_dev_point= (float *) GETPIXELPTRMACRO(stan_devs_image, col, row, width_step2, pixel_step2 );	
			int i=0;
			for(i=0;i<3;i++)
				std_dev_point[i] = (ALPHA *((((int) curr_point[i]) - average_point[i])*(((int) curr_point[i]) - average_point[i]))) + (ONEMINUS * std_dev_point[i]); 
			for(i=0;i<3;i++)
				average_point[i] = (ALPHA * (int) curr_point[i]) + (ONEMINUS * average_point[i]);
		}
	}
}
// 8 bit, 32 bit float, 32 bit float,  8 bit
void determine_moving_points_using_running_gaussian_averages( IplImage *current_frame, IplImage *averages_image, IplImage *stan_devs_image, IplImage *moving_mask_image )
{
	// TO-DO:  Determine which pixels on each channel are "foreground" by considering the absolute difference in comparison
	// with the standard deviation...
	int width_step=current_frame->widthStep;
	int width_step2=averages_image->widthStep;
	int pixel_step=current_frame->widthStep/current_frame->width;
	int pixel_step2=averages_image->widthStep/averages_image->width;
	int number_channels=current_frame->nChannels;

	int row=0,col=0;
	for(;row<current_frame->height;row++){
		for(col=0;col<current_frame->width;col++){
			unsigned char* curr_point = GETPIXELPTRMACRO(current_frame, col, row, width_step, pixel_step );	
			float *average_point= (float *) GETPIXELPTRMACRO(averages_image, col, row, width_step2, pixel_step2 );	
			float *std_dev_point= (float *) GETPIXELPTRMACRO(stan_devs_image, col, row, width_step2, pixel_step2 );	
			unsigned char * mask = GETPIXELPTRMACRO(moving_mask_image, col, row, width_step, pixel_step );	
			int i=0;
			for(;i<3;i++){
				if(abs(curr_point[i] - average_point[i]) > (K_VAL * (std_dev_point[i])))
					mask[i] = 255;
				else
					mask[i] = 0;	
			}
		}
	}
}


int main( int argc, char** argv )
{
	IplImage *current_frame=NULL;
	IplImage *running_average_background=NULL;

	IplImage *static_background_image=NULL;
	IplImage *static_moving_mask_image=NULL;
	IplImage *running_average_background_image=NULL;
	IplImage *running_average_moving_mask_image=NULL;
	IplImage *running_gaussian_average_background_average=NULL;
	IplImage *running_gaussian_average_background_sd=NULL;
	IplImage *running_gaussian_average_sd_image=NULL;
	IplImage *running_gaussian_average_background_image=NULL;
	IplImage *running_gaussian_average_moving_mask_image=NULL;

	IplImage *change_and_remain_changed_background_image=NULL;
	IplImage *subtracted_image=NULL;
	IplImage *moving_mask_image=NULL;

	int user_clicked_key=0;
	int show_ch = 'm';
	bool paused = false;
    
	// Load the video (AVI) file
	CvCapture *capture = cvCaptureFromAVI( "./CarPark.avi" );

	// Ensure AVI opened properly
	if( !capture )
		return 1;    
    
	// Get Frames Per Second in order to playback the video at the correct speed
	int fps = ( int )cvGetCaptureProperty( capture, CV_CAP_PROP_FPS );
    
	// Explain the User Interface
	printf( "Hot keys: \n"
		"\tESC - quit the program\n"
		"\tSPACE - pause/resume the video\n");

	// Create display windows for images
	cvNamedWindow( "Input video", 0 );
	cvMoveWindow("Input video", 360, 0);
	cvNamedWindow( "Static Background", 0 );
	cvMoveWindow("Static Background", 720, 0);
	cvNamedWindow( "Running Average Background", 0 );
	cvMoveWindow("Running Average Background", 1080, 0);
	cvNamedWindow( "Running Gaussian Average Background", 0 );
	cvMoveWindow("Running Gaussian Average Background", 0, 360);
	cvNamedWindow( "Running Gaussian Average Stan. Dev.", 0 );
	cvMoveWindow( "Running Gaussian Average Stan. Dev.", 360, 360 );
	cvNamedWindow( "Moving Points - Static", 0 );
	cvMoveWindow( "Moving Points - Static", 720, 360);
	cvNamedWindow( "Moving Points - Running Average", 0 );
	cvMoveWindow( "Moving Points - Running Average", 1080, 360);
	cvNamedWindow( "Moving Points - Running Gaussian Average", 0 );
	cvMoveWindow( "Moving Points - Running Gaussian Average", 0, 0);

	// Setup mouse callback on the original image so that the user can see image values as they move the
	// cursor over the image.
	cvSetMouseCallback( "Input video", on_mouse_show_values, 0 );
	window_name_for_on_mouse_show_values="Input video";

	while( user_clicked_key != ESC ) {
		// Get current video frame
		current_frame = cvQueryFrame( capture );
		if( !current_frame ) // No new frame available
				break;
		image_for_on_mouse_show_values = current_frame; // Assign image for mouse callback
		cvShowImage( "Input video", current_frame );

		if (static_background_image == NULL)
		{	// The first time around the loop create the images for processing
			// General purpose images
			subtracted_image = cvCloneImage( current_frame );
			// Static backgound images
			static_background_image = cvCloneImage( current_frame );
			static_moving_mask_image = cvCreateImage( cvGetSize(current_frame), 8, 3 );
			cvShowImage( "Static Background", static_background_image );
			// Running average images
			running_average_background = cvCreateImage( cvGetSize(current_frame), IPL_DEPTH_32F, 3 );
			//cvZero(running_average_background);
			cvConvert(current_frame, running_average_background);
			running_average_background_image = cvCloneImage( current_frame );
			running_average_moving_mask_image = cvCreateImage( cvGetSize(current_frame), 8, 3 );
			// Running Gaussian average images
			running_gaussian_average_background_image = cvCloneImage( current_frame );
			running_gaussian_average_sd_image = cvCloneImage( current_frame );
			running_gaussian_average_moving_mask_image = cvCreateImage( cvGetSize(current_frame), 8, 3 );
			running_gaussian_average_background_average = cvCreateImage( cvGetSize(current_frame), IPL_DEPTH_32F, 3 );
			cvConvert(current_frame, running_gaussian_average_background_average);
			running_gaussian_average_background_sd = cvCreateImage( cvGetSize(current_frame), IPL_DEPTH_32F, 3 );
			cvZero(running_gaussian_average_background_sd);
		}

		// Static Background Processing
		cvAbsDiff( current_frame, static_background_image, subtracted_image );
		cvThreshold( subtracted_image, static_moving_mask_image, 30, 255, CV_THRESH_BINARY );
		cvShowImage( "Moving Points - Static", static_moving_mask_image );

		// Running Average Background Processing
		cvRunningAvg( current_frame, running_average_background, 0.01 /*, moving_mask_image*/ );
		cvConvert( running_average_background, running_average_background_image );
		cvAbsDiff( current_frame, running_average_background_image, subtracted_image );
		cvThreshold( subtracted_image, running_average_moving_mask_image, 30, 255, CV_THRESH_BINARY );
		cvShowImage( "Running Average Background", running_average_background_image );
		cvShowImage( "Moving Points - Running Average", running_average_moving_mask_image );
		
		// Running Gaussian Average Background Processing
		update_running_gaussian_averages( current_frame, running_gaussian_average_background_average, running_gaussian_average_background_sd );
		cvConvertScaleAbs( running_gaussian_average_background_average, running_gaussian_average_background_image, 1.0, 0 );
		cvShowImage( "Running Gaussian Average Background", running_gaussian_average_background_image );
		cvConvertScaleAbs( running_gaussian_average_background_sd, running_gaussian_average_sd_image, 10.0, 0 );
		cvShowImage( "Running Gaussian Average Stan. Dev.", running_gaussian_average_sd_image );
		determine_moving_points_using_running_gaussian_averages( current_frame, running_gaussian_average_background_average, running_gaussian_average_background_sd, running_gaussian_average_moving_mask_image );
		cvShowImage( "Moving Points - Running Gaussian Average", running_gaussian_average_moving_mask_image );

		// Deal with user input, and wait for the delay between frames
		do {
			if( user_clicked_key == ' ' )
			{
				paused = !paused;
			}
			if (paused)
				user_clicked_key = (char)cvWaitKey(0);
			else user_clicked_key = (char) cvWaitKey( 1000 / fps );
		} while (( user_clicked_key != ESC ) && ( user_clicked_key != -1 ));
	}
    
	/* free memory */
	cvReleaseCapture( &capture );
	cvDestroyWindow( "Input video" );
	cvDestroyWindow( "Static Background" );
	cvDestroyWindow( "Running Average Background" );
	cvDestroyWindow( "Running Gaussian Average Background" );
	cvDestroyWindow( "Running Gaussian Average Stan. Dev." );
	cvDestroyWindow( "Moving Points - Static" );
	cvDestroyWindow( "Moving Points - Running Average" );
	cvDestroyWindow( "Moving Points - Running Gaussian Average" );

	return 0;
}
