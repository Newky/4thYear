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
int thresholds[NUMBER_OF_POSTBOXES] = {305, 240, 420, 440, 400, 400};
//bool post[NUMBER_OF_POSTBOXES] = {false, false, false, false, false, false};
IplImage * prev_frame;
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

int first_derivative_with_mask(IplImage* input_image, int row, int col, int mask[3][3]) {
	int width_step=input_image->widthStep;
	int pixel_step=input_image->widthStep/input_image->width;
	int i=0;
	int fd = 0;
	// Calculate first partial
	for(;i<3;i++) {
		int j=0;
		for(;j<3;j++) {
			if(!((row - (i - 2) < 0) && (row + (i - 2) >= input_image->height))){
				if(!((col - (j - 2) < 0) && (col + (j - 2) >= input_image->width))){
					int mod_row = row + (i - 2);
					int mod_col = col + (j - 2);
					unsigned char* curr_point = (unsigned char *) GETPIXELPTRMACRO(input_image, mod_col, mod_row, width_step, pixel_step);
					fd += (curr_point[0] * mask[i][j]);
				}
			}
		}
	}
	return fd;
};

int gradient(IplImage* input_image, int row, int col) {
	int hmask[3][3] = {{1, 0, -1}, {2, 0, -2}, {1, 0, -1}};
	int vmask[3][3] = {{1, 2, 1}, {0, 0, 0}, {-1, -2, -1}};
	int fd1 = first_derivative_with_mask(input_image, row, col, hmask);
	int fd2 = first_derivative_with_mask(input_image, row, col, vmask);
	return sqrt(pow(fd1, 2)+ pow(fd2, 2));
}

void sobel(IplImage* input_image,IplImage*output_image,IplImage*fd_image, int row, int col){
	int width_step=fd_image->widthStep;
	int pixel_step=fd_image->widthStep/fd_image->width;
	int width_step2=output_image->widthStep;
	int pixel_step2=output_image->widthStep/output_image->width;
	int mask[3][3] = {{1, 0, -1}, {2, 0, -2}, {1, 0, -1}};
	
	int fd = abs(first_derivative_with_mask(input_image, row, col, mask));
	unsigned char fd_[] = {fd};
	PUTPIXELMACRO(fd_image, col, row,fd_,width_step, pixel_step, fd_image->nChannels);
	if(fd> 90 ){
		unsigned char red[] = {0,0,255,0};
		PUTPIXELMACRO(output_image, col, row, red,width_step2, pixel_step2, output_image->nChannels);
	}else{
		unsigned char black[] = {0,0,0,0};
		PUTPIXELMACRO(output_image, col, row, black,width_step2, pixel_step2, output_image->nChannels);
	}
}

void non_maxima(IplImage * output_image, IplImage * fd_image, int row, int col) {
	int width_step=fd_image->widthStep;
	int pixel_step=fd_image->widthStep/fd_image->width;
	int width_step2=output_image->widthStep;
	int pixel_step2=output_image->widthStep/output_image->width;

	
	unsigned char* curr_point = (unsigned char *) GETPIXELPTRMACRO(output_image, col, row, width_step2, pixel_step2);
	if(curr_point[RED_CH] == 255) {
		unsigned char* fd_point = (unsigned char *) GETPIXELPTRMACRO(fd_image, col, row, width_step, pixel_step);
		if(col -1 > 0) {
			unsigned char* to_the_left   = (unsigned char *) GETPIXELPTRMACRO(fd_image, col-1, row, width_step, pixel_step);
			if(fd_point[0] < to_the_left[0]){
				curr_point[RED_CH] = 0;
				return;
			}
		}
		if(col + 1 < fd_image->width) {
			unsigned char* to_the_right = (unsigned char *) GETPIXELPTRMACRO(fd_image, col+1, row, width_step, pixel_step);
			if(fd_point[0] < to_the_right[0]){
				curr_point[RED_CH] = 0;
				return;
			}
		}
	}
}

void compute_vertical_edge_image(IplImage* input_image, IplImage* output_image)
{
	// TO-DO:  Compute the partial first derivative edge image in order to locate the vertical edges in the passed image,
	//   and then determine the non-maxima suppressed version of these edges (along each row as the rows can be treated
	//   independently as we are only considering vertical edges). Output the non-maxima suppressed edge image. 
	// Note:   You may need to smooth the image first.
	IplImage *gray_input= cvCreateImage( cvGetSize(input_image), 8, 1 );
	// Store for the partial first derivate (vertical) of the image
	IplImage *fd_image= cvCreateImage( cvGetSize(input_image), 8, 1 );
	cvConvertImage(input_image, gray_input);
	cvSmooth(gray_input, gray_input, CV_GAUSSIAN, 3, 3, 0);
	int width_step=gray_input->widthStep;
	int pixel_step=gray_input->widthStep/gray_input->width;
	int number_channels=gray_input->nChannels;
	int row=0,col=0;
	for(;row < gray_input->height; row ++) {
		for(col = 0;col < gray_input->width; col ++ ) {
			sobel(gray_input, output_image,fd_image,  row, col);
		}
	}
	for(row=0;row < gray_input->height; row ++) {
		for(col = 0;col < gray_input->width; col ++ ) {
			non_maxima(output_image, fd_image, row, col);
		}
	}
	//cvMorphologyEx(  output_image,output_image, NULL, NULL, CV_MOP_CLOSE,1 );
}



bool motion_free_frame(IplImage* current_frame, IplImage* previous_frame)
{
	// TO-DO:  Determine the percentage of the frames which have changed (by more than VARIATION_ALLOWED_IN_PIXEL_VALUES)
	//        and return whether that percentage is less than ALLOWED_MOTION_FOR_MOTION_FREE_IMAGE.
	int width_step=current_frame->widthStep;
	int pixel_step=current_frame->widthStep/current_frame->width;
	int number_channels=current_frame->nChannels;
	
	int row=0,col=0;
	int changed = 0;
	for(;row < current_frame->height; row ++) {
		for(col = 0;col < current_frame->width; col ++ ) {
			unsigned char* curr_point = (unsigned char *) GETPIXELPTRMACRO(current_frame,col, row,width_step, pixel_step);
			unsigned char* prev_point = (unsigned char *) GETPIXELPTRMACRO(previous_frame,col, row,width_step, pixel_step);
			int i=0, sum1 =0, sum2=0;
			for(;i<number_channels;i++){
				sum1 += curr_point[i];
				sum2 += prev_point[i];
			}
			changed += (abs(sum1 -sum2) > (VARIATION_ALLOWED_IN_PIXEL_VALUES*3));
		}
	}

	int percentage = (((double) changed)/((double) current_frame->height * current_frame->width)) * 100;
	return (percentage < ALLOWED_MOTION_FOR_MOTION_FREE_IMAGE);
}

void check_postboxes(IplImage* input_image, IplImage* labelled_output_image, IplImage* vertical_edge_image )
{
	// TO-DO:  If the input_image is not motion free then do nothing.  Otherwise determine the vertical_edge_image and check
	//        each postbox to see if there is mail (by analysing the vertical edges).  Highlight the edge points used during your
	//        processing.  If there is post in a box indicate that there is on the labelled_output_image.
	if(motion_free_frame(input_image, prev_frame)){
		compute_vertical_edge_image(input_image, vertical_edge_image);
		int width_step=vertical_edge_image->widthStep;
		int pixel_step=vertical_edge_image->widthStep/vertical_edge_image->width;
		for(int pb=0;pb<6;pb++){
			//#define POSTBOX_TOP_ROW 0
			//#define POSTBOX_TOP_BASE_ROW 1
			//#define POSTBOX_BOTTOM_ROW 2
			//#define POSTBOX_LEFT_COLUMN 3
			//#define POSTBOX_RIGHT_COLUMN 4
			int *postbox = PostboxLocations[pb];
			int row=postbox[POSTBOX_TOP_ROW],col=postbox[POSTBOX_LEFT_COLUMN], redcount=0;
			for(;row < postbox[POSTBOX_BOTTOM_ROW]; row++) {
				for(col=postbox[POSTBOX_LEFT_COLUMN];col < postbox[POSTBOX_RIGHT_COLUMN]; col++) {
					unsigned char* curr_point = (unsigned char *) GETPIXELPTRMACRO(vertical_edge_image,col,row,width_step, pixel_step);
					redcount += (curr_point[RED_CH] == 255);
				}
			}
			if(redcount < thresholds[pb]) 
				indicate_post_in_box(labelled_output_image, pb);
		}
	}
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
	cvMoveWindow( "Vertical edges", 360, 0);
	cvNamedWindow( "Results", 0 );
	cvMoveWindow( "Results", 720, 0);

	// Setup mouse callback on the original image so that the user can see image values as they move the
	// cursor over the image.
	cvSetMouseCallback( "Input video", on_mouse_show_values, 0 );
	window_name_for_on_mouse_show_values="Input video";
	
	while( user_clicked_key != ESC ) {
		// Get current video frame
		prev_frame = cvCloneImage(corrected_frame);
		current_frame = cvQueryFrame( capture );
		image_for_on_mouse_show_values=current_frame; // Assign image for mouse callback
		if( !current_frame ) // No new frame available
				break;

		cvWarpPerspective( current_frame, corrected_frame, warp_matrix );

		if (labelled_image == NULL)
		{	// The first time around the loop create the image for processing
			prev_frame = cvCloneImage(corrected_frame);
			labelled_image = cvCloneImage( corrected_frame );
			vertical_edge_image = cvCloneImage( corrected_frame );
		}
		labelled_image = cvCloneImage( corrected_frame );
		check_postboxes( corrected_frame, labelled_image, vertical_edge_image );

		//IplImage *gframe= cvCreateImage( cvGetSize(corrected_frame),8, 1 );
		//IplImage *outputframe= cvCreateImage( cvGetSize(corrected_frame),8, 3 );
		//cvConvertImage(corrected_frame, gframe);
		//compute_vertical_edge_image(gframe, outputframe);
		// Display the current frame and results of processing
		cvShowImage( "Input video", current_frame);
		cvShowImage( "Vertical edges",vertical_edge_image);
		cvShowImage( "Results",labelled_image);

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
