#ifdef _CH_
#pragma package <opencv>
#endif

#include <stdio.h>
#include <stdlib.h>
#include "cv.h"
#include "highgui.h"
#include "../utilities.h"

#define VARIATION_ALLOWED_IN_PIXEL_VALUES 31
#define ALLOWED_MOTION_FOR_MOTION_FREE_IMAGE 1.0
#define NUMBER_OF_POSTBOXES 6
#define MINIMUM_GRADIENT_VALUE 5
int PostboxLocations[NUMBER_OF_POSTBOXES][5] = {
                                {   6,  73,  95, 5, 92 }, {   6,  73,  95, 105, 192 },
                                { 105, 158, 193, 5, 92 }, { 105, 158, 193, 105, 192 },
                                { 204, 245, 292, 5, 92 }, { 204, 245, 292, 105, 192 } };
#define POSTBOX_TOP_ROW 0
#define POSTBOX_TOP_BASE_ROW 1
#define POSTBOX_BOTTOM_ROW 2
#define POSTBOX_LEFT_COLUMN 3
#define POSTBOX_RIGHT_COLUMN 4


void indicate_post_in_box( IplImage* image, int postbox )
{
	write_text_on_image(image,(PostboxLocations[postbox][POSTBOX_TOP_ROW]+PostboxLocations[postbox][POSTBOX_BOTTOM_ROW])/2,PostboxLocations[postbox][POSTBOX_LEFT_COLUMN]+2, "Post in");
	write_text_on_image(image,(PostboxLocations[postbox][POSTBOX_TOP_ROW]+PostboxLocations[postbox][POSTBOX_BOTTOM_ROW])/2+19,PostboxLocations[postbox][POSTBOX_LEFT_COLUMN]+2, "this box");
}

void sobel(IplImage* input_image, int row, int col, int width_step, int pixel_step) {
	int mask[3][3] = {{1, 0, -1}, {2, 0, -2}, {1, 0, -1}};
	int i=0;
	int fd = 0;
	for(;i<3;i++) {
		int j=0;
		for(;j<3;j++) {
			if(!((row - (i - 2) < 0) && (row + (i - 2) >= input_image->height))){
				if(!((col - (j - 2) < 0) && (col + (j - 2) >= input_image->width))){
					int mod_row = row + (i - 2);
					int mod_col = col + (j - 2);
					unsigned char* curr_point = ((unsigned char *) input_image->imageData + (mod_row)*(width_step) + (mod_col));	
					fd += (curr_point[0] * mask[i][j]);
				}
			}
		}
	}
	unsigned char* curr_point = ((unsigned char *) input_image->imageData + (row)*(width_step) + (col));	
	if(fd > 41) {
		curr_point[0] = 255;	
	}else if(fd < -40) {
		curr_point[0] = 0;	
	}else{
		curr_point[0] = 127;	
	}
}

void compute_vertical_edge_image(IplImage* input_image, IplImage* output_image)
{
	// TO-DO:  Compute the partial first derivative edge image in order to locate the vertical edges in the passed image,
	//   and then determine the non-maxima suppressed version of these edges (along each row as the rows can be treated
	//   independently as we are only considering vertical edges). Output the non-maxima suppressed edge image. 
	// Note:   You may need to smooth the image first.
	cvSmooth(input_image, output_image, CV_GAUSSIAN, 3, 3, 0);
	int width_step=input_image->widthStep;
	int pixel_step=input_image->widthStep/input_image->width;
	int number_channels=input_image->nChannels;
	int row=0,col=0;
	for(;row < input_image->height; row ++) {
		for(col = 0;col < input_image->width; col ++ ) {
			sobel(input_image, row, col, width_step, pixel_step);
		}
	}
}



bool motion_free_frame(IplImage* current_frame, IplImage* previous_frame)
{
	// TO-DO:  Determine the percentage of the frames which have changed (by more than VARIATION_ALLOWED_IN_PIXEL_VALUES)
	//        and return whether that percentage is less than ALLOWED_MOTION_FOR_MOTION_FREE_IMAGE.
	return true;  // Just to allow the system to compile while the code is missing.
}

void check_postboxes(IplImage* input_image, IplImage* labelled_output_image, IplImage* vertical_edge_image )
{
	// TO-DO:  If the input_image is not motion free then do nothing.  Otherwise determine the vertical_edge_image and check
	//        each postbox to see if there is mail (by analysing the vertical edges).  Highlight the edge points used during your
	//        processing.  If there is post in a box indicate that there is on the labelled_output_image.
}


int main( int argc, char** argv )
{
	IplImage *current_frame=NULL;
	CvSize size;
	size.height = 300; size.width = 200;
	IplImage *corrected_frame = cvCreateImage( size, IPL_DEPTH_8U, 3 );
	IplImage *labelled_image=NULL;
	IplImage *vertical_edge_image=NULL;
	int user_clicked_key=0;

	// Load the video (AVI) file
	CvCapture *capture = cvCaptureFromAVI( "./Postboxes.avi" );
	// Ensure AVI opened properly
	if( !capture )
		return 1;    

	// Get Frames Per Second in order to playback the video at the correct speed
	int fps = ( int )cvGetCaptureProperty( capture, CV_CAP_PROP_FPS );

	// Explain the User Interface
	printf( "Hot keys: \n"
		    "\tESC - quit the program\n"
	    "\tSPACE - pause/resume the video\n");

	//What?
	CvPoint2D32f from_points[4] = { {3, 6}, {221, 11}, {206, 368}, {18, 373} };
	CvPoint2D32f to_points[4] = { {0, 0}, {200, 0}, {200, 300}, {0, 300} };
	//Multi Channel Matrix
	CvMat* warp_matrix = cvCreateMat( 3,3,CV_32FC1 );
	cvGetPerspectiveTransform( from_points, to_points, warp_matrix );

	// Create display windows for images
	cvNamedWindow( "Input video", 0 );
	cvMoveWindow("Input video", 0, 0);
	cvNamedWindow( "Vertical edges", 0 );
	cvMoveWindow( "Vertical edges", 0, 360);
	cvNamedWindow( "Results", 0 );
	cvMoveWindow( "Results", 0, 720);

	// Setup mouse callback on the original image so that the user can see image values as they move the
	// cursor over the image.
	cvSetMouseCallback( "Input video", on_mouse_show_values, 0 );
	window_name_for_on_mouse_show_values="Input video";

	while( user_clicked_key != ESC ) {
		// Get current video frame
		current_frame = cvQueryFrame( capture );
		image_for_on_mouse_show_values=current_frame; // Assign image for mouse callback
		if( !current_frame ) // No new frame available
				break;

		cvWarpPerspective( current_frame, corrected_frame, warp_matrix );

		if (labelled_image == NULL)
		{	// The first time around the loop create the image for processing
			labelled_image = cvCloneImage( corrected_frame );
			vertical_edge_image = cvCloneImage( corrected_frame );
		}
		check_postboxes( corrected_frame, labelled_image, vertical_edge_image );
		IplImage *gframe= cvCreateImage( cvGetSize(current_frame),8, 1 );
		cvConvertImage(current_frame, gframe);
		compute_vertical_edge_image(gframe, gframe);
		// Display the current frame and results of processing
		cvShowImage( "Input video", gframe );
		cvShowImage( "Vertical edges", vertical_edge_image );
		cvShowImage( "Results", labelled_image );

		// Wait for the delay between frames
		user_clicked_key = (char) cvWaitKey( 1000 / fps );
		if (user_clicked_key == ' ')
		{
			user_clicked_key = cvWaitKey(0);
		}
	}

	/* free memory */
	cvReleaseCapture( &capture );
	cvDestroyWindow( "video" );

	return 0;
}
