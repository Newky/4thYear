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

/*Given a row and column value for an edge image.(i.e binary image highlighting edges)
 *And a reference to a left column and right column int value
 *If less than two edge points are found in either the left scan
 *or right scan then return false.
 * */
bool find_label_edges( IplImage* edge_image, IplImage* result_image, int row, int& left_label_column, int& right_label_column )
{
	/* Get the width and pixel step for
	 * the edge image and the result image.
	 */
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
	/* For loop which loops till half way through the image
	 * if it finds an edge, point_found is incremented
	 * If the second point is found then the left label column is set.
	 */
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
	/*If the two edge points aren't found then return a false*/
	if(point_found < 2)
		return false;
	// Reset point found
	point_found = 0;
	/* Same as the last for loop except it
	 * decrements from the right hand side of image.
	 */
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
	if(point_found < 2)
		return false;
	/* if you get to here, both edges and labels have been found, return a true */
	return true;
}
/* Check glue bottle takes the original image, 
 * Runs over each row in a given area and a given row step increment,
 * And checks each row using the above function.
 * */
void check_glue_bottle( IplImage* original_image, IplImage* result_image )
{
	/* Needs two gray scale versions of the image
	 * Convert the original image.
	 * Smooth the image with a gaussian filter.
	 * Use Canny edge detection on the image,which leaves u with a binary image.
	 */
	IplImage * gray= cvCreateImage(cvGetSize(original_image),IPL_DEPTH_8U, 1);
	IplImage * gray2= cvCreateImage(cvGetSize(original_image),IPL_DEPTH_8U, 1);
	cvConvertImage(original_image, gray);
	cvSmooth(gray, gray, CV_GAUSSIAN, 11,11);
	cvCanny(gray, gray2, 10, 40);

	int i=0, blank=0;
	int left_column = -1, temp_left_column = -1;
	int right_column = -1, temp_right_column = -1;
	/* We take the left and right hand points 
	 * in as an array, so we can account for point drift, 
	 * which wouldn't be accounted for using previous and current point
	 */
	int * left_col_arr = new int[5];
	int * right_col_arr = new int[5];
	int * lptr = left_col_arr;
	int * rptr = right_col_arr;
	cvZero(result_image);
	/*This merges the original canny image into the result image. (Overaly edges with the algorithm for finding label edges.*/
	cvMerge(gray2, gray2, gray2, NULL, result_image);
	/* Given a starting label row, and an ending label row, and a row step
	 * Find the label edge on that row,  If either the left or right point of the label edge are equal to 0
	 * the bottle is blank
	 * */
	for(i = FIRST_LABEL_ROW_TO_CHECK; i < LAST_LABEL_ROW_TO_CHECK; i+=ROW_STEP_FOR_LABEL_CHECK) {
		find_label_edges(gray2, result_image, i, *lptr, *rptr);
		if(*rptr == 0 || *lptr ==0)
			blank =1;
		lptr++;
		rptr++;
	};
	if(blank){
		write_text_on_image(result_image, 50, 50, "No Label");
		return;
	}
	/* Set up the left and right min
	 * And get the max and min values of the array (both left and right)
	 */
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
	/* If the difference between the max and min value of left column is more than some
	 * allowed variation then its crooked, same for right column
	 * Otherwise the label is present */
	if(abs(left_col_arr[lmax] -left_col_arr[lmin]) > ALLOWED_VARIATION){
		write_text_on_image(result_image, 50, 50, "Crooked");
	}else if(abs(right_col_arr[rmax] -right_col_arr[rmin]) > ALLOWED_VARIATION){
		write_text_on_image(result_image, 50, 50, "Crooked");
	}else {
		write_text_on_image(result_image, 50, 50, "Label Present");
	}
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
