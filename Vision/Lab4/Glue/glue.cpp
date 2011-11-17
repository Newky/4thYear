#ifdef _CH_
#pragma package <opencv>
#endif

#include "cv.h"
#include "highgui.h"
#include <stdio.h>
#include <stdlib.h>
#include "../utilities.h"


#define NUM_IMAGES 9
#define FIRST_LABEL_ROW_TO_CHECK 390
#define LAST_LABEL_ROW_TO_CHECK 490
#define ROW_STEP_FOR_LABEL_CHECK 20
#define ALLOWED_VARIATION 4


bool find_label_edges( IplImage* edge_image, IplImage* result_image, int row, int& left_label_column, int& right_label_column )
{
	// TO-DO: Search for the sides of the labels from both the left and right on "row".  The side of the label is taken	
	//        taken to be the second edge located on that row (the side of the bottle being the first edge).  If the label
	//        are found set the left_label_column and the right_label_column and return true.  Otherwise return false.
	//        The routine should mark the points searched (in yellow), the edges of the bottle (in blue) and the edges of the
	//        label (in red) - all in the result_image.

	int width_step=edge_image->widthStep;
	int pixel_step=edge_image->widthStep/edge_image->width;
	int width_step2=result_image->widthStep;
	int pixel_step2=result_image->widthStep/result_image->width;
	int point_found = 0;
	left_label_column =0;
	right_label_column =0;
	unsigned char blue[] = {255, 0, 0};
	unsigned char red[] = {0, 0,255};
	unsigned char yellow[] = {0, 255, 255};
	unsigned char black[] = {0, 0, 0};
	for(int i=0; i < edge_image->width / 2; i++) {
		unsigned char* curr_point = (unsigned char *) GETPIXELPTRMACRO(edge_image,i,row,width_step, pixel_step);
		if(curr_point[0] == 255){
			if(point_found ==0)
				PUTPIXELMACRO(result_image, i, row,blue ,width_step2, pixel_step2, result_image->nChannels);
			else if(point_found == 1)
				PUTPIXELMACRO(result_image, i, row,red,width_step2, pixel_step2, result_image->nChannels);
			point_found++;
		}else {
			PUTPIXELMACRO(result_image, i, row,yellow,width_step2, pixel_step2, result_image->nChannels);
		}
		if(point_found == 2){
			left_label_column = i;
			break;
		}
	}
	point_found = 0;
	for(int i=edge_image->width -1/2; i>0; i--) {
		unsigned char* curr_point = (unsigned char *) GETPIXELPTRMACRO(edge_image,i,row,width_step, pixel_step);
		if(curr_point[0] == 255){
			if(point_found ==0)
				PUTPIXELMACRO(result_image, i, row,blue ,width_step2, pixel_step2, result_image->nChannels);
			else if(point_found == 1)
				PUTPIXELMACRO(result_image, i, row,red,width_step2, pixel_step2, result_image->nChannels);
			point_found++;
		}else {
			PUTPIXELMACRO(result_image, i, row,yellow,width_step2, pixel_step2, result_image->nChannels);
		}
		if(point_found == 2){
			right_label_column = i;
			break;
		}
	}
	return false;
}

void check_glue_bottle( IplImage* original_image, IplImage* result_image )
{

	IplImage * gray= cvCreateImage(cvGetSize(original_image),IPL_DEPTH_8U, 1);
	IplImage * gray2= cvCreateImage(cvGetSize(original_image),IPL_DEPTH_8U, 1);
	cvConvertImage(original_image, gray);
	cvSmooth(gray, gray, CV_GAUSSIAN, 11,11);
	cvCanny(gray, gray2, 10, 40);
	
	int i=0, blank=0;
	int left_column = -1, temp_left_column = -1;
	int right_column = -1, temp_right_column = -1;
	int * left_col_arr = new int[5];
	int * right_col_arr = new int[5];
	int * lptr = left_col_arr;
	int * rptr = right_col_arr;
	cvZero(result_image);
	for(i = FIRST_LABEL_ROW_TO_CHECK; i < LAST_LABEL_ROW_TO_CHECK; i+=ROW_STEP_FOR_LABEL_CHECK) {
		find_label_edges(gray2, result_image, i, *lptr, *rptr);
		if(*rptr == 0 || *lptr ==0)
			blank =1;
		lptr++;
		rptr++;
	};

	int lmin= result_image->width, lmax = 0;
	int rmin= result_image->width, rmax = 0;

	for(int i=0;i<5;i++){
		if(left_col_arr[i] > lmax)
			lmax = i;
		if(right_col_arr[i] > rmax)
			rmax = i;
		if(left_col_arr[i] < lmin)
			lmin = i;
		if(right_col_arr[i] < rmin)
			rmin = i;
	};
	if(blank){
		write_text_on_image(result_image, 50, 50, "No Label");
		return;
	}
	if(abs(left_col_arr[lmax] -left_col_arr[lmin]) > ALLOWED_VARIATION){
		write_text_on_image(result_image, 50, 50, "Crooked");
	}else if(abs(right_col_arr[rmax] -right_col_arr[rmin]) > ALLOWED_VARIATION){
		write_text_on_image(result_image, 50, 50, "Crooked");
	}else {
		write_text_on_image(result_image, 50, 50, "Label Present");
	}
	//         To implement this you may need to use smoothing (cv::GaussianBlur() perhaps) and edge detection (cvCanny() perhaps).
	//        You might also need cvConvertImage() which converts between different types of image.
}

int main( int argc, char* argv[] )
{
	int selected_image_num = 1;
	IplImage* selected_image = NULL;
	IplImage* images[NUM_IMAGES];
	IplImage* result_image = NULL;

	// Load all the images.
	for (int file_num=1; (file_num <= NUM_IMAGES); file_num++)
	{
		char filename[100];
		sprintf(filename,"./Glue%d.jpg",file_num);
		if( (images[file_num-1] = cvLoadImage(filename,-1)) == 0 )
			return 0;
	}

	// Explain the User Interface
	printf( "Hot keys: \n\tESC - quit the program\n");
	printf( "\t1..%d - select image\n",NUM_IMAGES);
    
	// Create display windows for images
	cvNamedWindow( "Original", 1 );
	cvMoveWindow( "Original", 0, 0);
	cvNamedWindow( "Processed Image", 1 );
	cvMoveWindow( "Processed Image", 360, 0);

	// Create images to do the processing in.
	selected_image = cvCloneImage( images[selected_image_num-1] );
	result_image= cvCloneImage(selected_image);
	//result_image= cvCreateImage(cvGetSize(selected_image),IPL_DEPTH_8U, 1);

	// Setup mouse callback on the original image so that the user can see image values as they move the
	// cursor over the image.
	cvSetMouseCallback( "Original", on_mouse_show_values, 0 );
	window_name_for_on_mouse_show_values="Original";
	image_for_on_mouse_show_values=selected_image;

	int user_clicked_key = 0;
	do {
		// Process image (i.e. setup and find the number of spoons)
		cvCopyImage( images[selected_image_num-1], selected_image );
		cvShowImage( "Original", selected_image );
		image_for_on_mouse_show_values=selected_image;
		check_glue_bottle( selected_image, result_image );
		cvShowImage( "Processed Image", result_image );

		// Wait for user input
		user_clicked_key = cvWaitKey(0);
		if ((user_clicked_key >= '1') && (user_clicked_key <= '0'+NUM_IMAGES))
		{
			selected_image_num = user_clicked_key-'0';
		}
	} while ( user_clicked_key != ESC );

    return 1;
}
