#ifdef _CH_
#pragma package <opencv>
#endif

#include "cv.h"
#include "highgui.h"
#include <stdio.h>
#include <stdlib.h>
#include "../utilities.h"

#define NUM_IMAGES 5

// Locate the red pixels in the source image.
/* My thinking behind finding the red points is	
 * look for cases where Red >  Green && Red > Bluea
 * Also the pixel values are red when there is a difference of more than 15 on blue
 * and green. This is somewhat of a manual thresholding.
 */
void find_red_points( IplImage* source, IplImage* result, IplImage* temp )
{
	int width_step=result->widthStep;
	int pixel_step=result->widthStep/result->width;
	int number_channels=result->nChannels;
	cvZero( result );

	int row=0,col=0;
	unsigned char white_pixel[4] = {255,0,0,0};
	for (row=0; row < result->height; row++){
		for (col=0; col < result->width; col++)
		{
			
			unsigned char* curr_point = GETPIXELPTRMACRO( source, col, row, width_step, pixel_step);
				if((curr_point[RED_CH] >= curr_point[GREEN_CH]) ||
					(curr_point[RED_CH] >= curr_point[BLUE_CH])){
					if((curr_point[RED_CH]-curr_point[BLUE_CH]) >= 15){
						if((curr_point[RED_CH]-curr_point[GREEN_CH]) >= 19){
								PUTPIXELMACRO( result, col, row, white_pixel, width_step, pixel_step, number_channels );
						}
					}
				}
		}
	}
	// Apply morphological opening and closing operations to clean up the image
	cvMorphologyEx( result, result, NULL, NULL, CV_MOP_CLOSE, 1);
	cvMorphologyEx( result, result, NULL, NULL, CV_MOP_CLOSE, 2 );
};

/* Returns a CvSeq which is basically a list of 
 * Points which are connected
 */
CvSeq* connected_components( IplImage* source, IplImage* result )
{
	//Create a one channel image of the source file.
	IplImage* binary_image = cvCreateImage( cvGetSize(source), 8, 1 );
	cvConvertImage( source, binary_image );
	//Allocate some storage
	CvMemStorage* storage = cvCreateMemStorage(0);
	CvSeq* contours = 0;
	//cvThreshold applies a threshold to a single channel array
	//Takes the newly single channeled src and thresholds
	//in regards to a threshold of 1 apparently.
	// 255 is max value obv.
	cvThreshold( binary_image, binary_image, 1, 255, CV_THRESH_BINARY );
	// Storage is given as container of retrieved contours.
	// &contours becomes the pointer to the first outer contour
	// Next is size of sequence header
	// CV_RETR_CCOMP is one way the contours retrieved can be organised
	// top level is external boundaries
	// second level is bound boundaries
	//Last argument is method
	//CV Chain approx simple seems to compress points, 
	//i.e it doesn't tell you all the points in between, jsut the ending points
	cvFindContours( binary_image, storage, &contours, sizeof(CvContour),CV_RETR_CCOMP, CV_CHAIN_APPROX_SIMPLE );
	// If result is an initialised Image
	if (result)
	{
		//Black out the image
		cvZero( result );
		//For each contour found
		for(CvSeq* contour = contours ; contour != 0; contour = contour->h_next )
		{
			//Pick a random color (No contour should have the same color)
			CvScalar color = CV_RGB( rand()&255, rand()&255, rand()&255 );
			/* replace CV_FILLED with 1 to see the outlines */
			/* Takes the result image and a contour (one contour)
			 * external color and hole color
			 * Not sure what max level (next param) really does, something to do
			 * with how much of the contour is drawn.
			 * CV_FILLED fills the contours if 1 given highlights the outline.*/
			cvDrawContours( result, contour, color, color, -1, CV_FILLED, 8 );
		}
	}
	//Returns the outer most contour as a list of contours
	return contours;
}

/* Invert image simply cycles through each point and inverts the pixel
 * by simply doing 255 - pixel value */
void invert_image( IplImage* source, IplImage* result )
{
	int width_step=source->widthStep;
	int pixel_step=source->widthStep/source->width;
	int number_channels=source->nChannels;
	cvZero( result );
	int row=0,col=0, i;
	for (row=0; row < result->height; row++){
		for (col=0; col < result->width; col++){
			unsigned char* curr_point = GETPIXELPTRMACRO( source, col, row, width_step, pixel_step );
			unsigned char temp_point[number_channels];
			for(i=0;i<number_channels;i++) {
				temp_point[i] = 255-curr_point[i];
				PUTPIXELMACRO( result, col, row, temp_point, width_step, pixel_step, number_channels );
			};
		}
	}
}

// Assumes a 1D histogram of 256 elements.
/* Uses the optimal thresholding algo
 * as seen in class
 */
int determine_optimal_threshold( CvHistogram* hist )
{
	int curr = 127,old_curr=0;
	while(curr != old_curr) {
		int sum1=0, num1=0, sum2=0, num2=0, i=0;
		for(i=0;i<256;i++){
			int val = ((int) *cvGetHistValue_1D(hist, i));
			if(i > curr){
				sum2+=(i*val);num2+=val;
			}else{
				sum1+=(i*val);num1+=val;
			}
		}
		if(sum1 != 0 && num1 != 0)
			sum1 = (int)(((double)sum1)/num1);
		if(sum2 != 0 && num2 != 0)
			sum2 = (int)(((double)sum2)/num2);
		old_curr = curr;
		curr = (int)(((double)sum1 + sum2)/2);
	};
	return curr;
}
/* The mask (1 channel) tells us what part of the grayscale (1 channel) are important
 * We then check that greyscale pixel against the threshold
 * if above white
 * if below black
 */
void apply_threshold_with_mask(IplImage* grayscale_image,IplImage* result_image,IplImage* mask_image,int threshold)
{
	int width_step=mask_image->widthStep;
	int pixel_step=mask_image->widthStep/mask_image->width;
	int width_step2=result_image->widthStep;
	int pixel_step2=result_image->widthStep/result_image->width;
	int number_channels=grayscale_image->nChannels;
	int number_channels2=result_image->nChannels;
	int row=0,col=0;
	unsigned char black[] = {0,0,0};
	unsigned char white[] = {255,255,255};
	for(;row<mask_image->height;row++) {
		for(col=0;col<mask_image->width;col++) {
			unsigned char* mask_point = GETPIXELPTRMACRO( mask_image, col, row, width_step, pixel_step );
			unsigned char* curr_point = GETPIXELPTRMACRO( grayscale_image, col, row, width_step, pixel_step );
			int i=0;

			if(*mask_point != 0){
				if(*curr_point <= threshold){
					PUTPIXELMACRO( result_image, col, row, black, width_step2, pixel_step2,number_channels2);
				}else {
					PUTPIXELMACRO( result_image, col, row, white, width_step2, pixel_step2,number_channels2);
				}
			}
		}
	}	

}

//Example:determine_optimal_sign_classification( selected_image, red_point_image, red_components, background_components, result_image );
void determine_optimal_sign_classification( IplImage* original_image, IplImage* red_point_image, CvSeq* red_components, CvSeq* background_components, IplImage* result_image )
{
	int width_step=original_image->widthStep;
	int pixel_step=original_image->widthStep/original_image->width;
	//There are three one channel images created of size of original
	//One is the original converted to grayscale
	IplImage* mask_image = cvCreateImage( cvGetSize(original_image), 8, 1 );
	IplImage* grayscale_image = cvCreateImage( cvGetSize(original_image), 8, 1 );
	cvConvertImage( original_image, grayscale_image );
	IplImage* thresholded_image = cvCreateImage( cvGetSize(original_image), 8, 1 );
	//The resulting Image and the thresholded_image are both blacked.
	cvZero( thresholded_image );
	cvZero( result_image );
	int row=0,col=0;
	// curr_red_region is a sequence of contours which represent the red components in the image.
	CvSeq* curr_red_region = red_components;
	// For every connected red component
	while (curr_red_region != NULL)
	{
		
		cvZero( mask_image );
		CvScalar color = CV_RGB( 255, 255, 255 );
		CvScalar mask_value = cvScalar( 255 );
		// Determine which background components are contained within the red component (i.e. holes)
		//  and create a binary mask of those background components.
		//  v_next refers to the next node on a vertical hierarchy
		//  while h_next refers to next node on same level as itself.
		CvSeq* curr_background_region = curr_red_region->v_next;
		// if a background region even exists
		if (curr_background_region != NULL)
		{
			while (curr_background_region != NULL)
			{
				cvDrawContours( mask_image, curr_background_region, mask_value, mask_value, -1, CV_FILLED, 8 );
				cvDrawContours( result_image, curr_background_region, color, color, -1, CV_FILLED, 8 );
				curr_background_region = curr_background_region->h_next;
			}
			int hist_size=256;
			CvHistogram* hist = cvCreateHist( 1, &hist_size, CV_HIST_ARRAY );
			//The mask here is important as it tells it what part of the image to histogram!
			cvCalcHist( &grayscale_image, hist, 0, mask_image );
			// Determine an optimal threshold on the points within those components (using the mask)
			int optimal_threshold = determine_optimal_threshold( hist );
			apply_threshold_with_mask(grayscale_image,result_image,mask_image,optimal_threshold);
		}
		curr_red_region = curr_red_region->h_next;
	}

	for (row=0; row < result_image->height; row++)
	{
		unsigned char* curr_red = GETPIXELPTRMACRO( red_point_image, 0, row, width_step, pixel_step );
		unsigned char* curr_result = GETPIXELPTRMACRO( result_image, 0, row, width_step, pixel_step );
		for (col=0; col < result_image->width; col++)
		{
			curr_red += pixel_step;
			curr_result += pixel_step;
			if (curr_red[0] > 0)
				curr_result[2] = 255;
		}
	}

	cvReleaseImage( &mask_image );
}

int main( int argc, char** argv )
{
	int selected_image_num = 1;
	char show_ch = 's';
	IplImage* images[NUM_IMAGES];
	IplImage* selected_image = NULL;
	IplImage* temp_image = NULL;
	IplImage* red_point_image = NULL;
	IplImage* connected_reds_image = NULL;
	IplImage* connected_background_image = NULL;
	IplImage* result_image = NULL;
	CvSeq* red_components = NULL;
	CvSeq* background_components = NULL;
	
	// Load all the images.
	for (int file_num=1; (file_num <= NUM_IMAGES); file_num++)
	{
		if( (images[0] = cvLoadImage("./RealRoadSigns.jpg",-1)) == 0 )
			return 0;
		if( (images[1] = cvLoadImage("./RealRoadSigns2.jpg",-1)) == 0 )
			return 0;
		if( (images[2] = cvLoadImage("./ExampleRoadSigns.jpg",-1)) == 0 )
			return 0;
		if( (images[3] = cvLoadImage("./Parking.jpg",-1)) == 0 )
			return 0;
		if( (images[4] = cvLoadImage("./NoParking.jpg",-1)) == 0 )
			return 0;
	}

	// Explain the User Interface
	printf( "Hot keys: \n"
            "\tESC - quit the program\n"
			"\t1 - Real Road Signs (image 1)\n"
			"\t2 - Real Road Signs (image 2)\n"
			"\t3 - Synthetic Road Signs\n"
			"\t4 - Synthetic Parking Road Sign\n"
			"\t5 - Synthetic No Parking Road Sign\n"
			"\tr - Show red points\n"
			"\tc - Show connected red points\n"
			"\th - Show connected holes (non-red points)\n"
			"\ts - Show optimal signs\n"
			);
    
	// Create display windows for images
	cvNamedWindow( "Original", 1 );
	cvNamedWindow( "Processed Image", 1 );

	// Setup mouse callback on the original image so that the user can see image values as they move the
	// cursor over the image.
	cvSetMouseCallback( "Original", on_mouse_show_values, 0 );
	window_name_for_on_mouse_show_values="Original";
	image_for_on_mouse_show_values=selected_image;

	int user_clicked_key = 0;
	do {
		// Create images to do the processing in.
		if (red_point_image != NULL)
		{
			cvReleaseImage( &red_point_image );
			cvReleaseImage( &temp_image );
			cvReleaseImage( &connected_reds_image );
			cvReleaseImage( &connected_background_image );
			cvReleaseImage( &result_image );
		}
		//Selected image is the image as normal
		selected_image = images[selected_image_num-1];
		//Red points is a binary version of the image which highlights the red components
		red_point_image = cvCloneImage( selected_image );
		result_image = cvCloneImage( selected_image );
		// Temp image used for some transformations and processing
		temp_image = cvCloneImage( selected_image );

		connected_reds_image = cvCloneImage( selected_image );
		connected_background_image = cvCloneImage( selected_image );

		// Process image
		image_for_on_mouse_show_values = selected_image;
		//Find red points of selected image and return them in
		// red point image
		// This should highlight the red surroundings.
		// Why does it crap out with morphological operations?
		find_red_points( selected_image, red_point_image, temp_image );
		// Red components image is finding connected components in the binary red image
		red_components = connected_components( red_point_image, connected_reds_image );
		invert_image( red_point_image, temp_image );
		background_components = connected_components( temp_image, connected_background_image );
		determine_optimal_sign_classification( selected_image, red_point_image, red_components, background_components, result_image );

		// Show the original & result
		cvShowImage( "Original", selected_image );
		do {
			if ((user_clicked_key == 'r') || (user_clicked_key == 'c') || (user_clicked_key == 'h') || (user_clicked_key == 's'))
				show_ch = user_clicked_key;
			switch (show_ch)
			{
			case 'c':
				cvShowImage( "Processed Image", connected_reds_image );
				break;
			case 'h':
				cvShowImage( "Processed Image", connected_background_image );
				break;
			case 'r':
				cvShowImage( "Processed Image", red_point_image );
				break;
			case 's':
			default:
				cvShowImage( "Processed Image", result_image);
				break;
			}
			user_clicked_key = cvWaitKey(0);
		} while ((!((user_clicked_key >= '1') && (user_clicked_key <= '0'+NUM_IMAGES))) &&
			     ( user_clicked_key != ESC ));
		/* End of nested do while */
		if ((user_clicked_key >= '1') && (user_clicked_key <= '0'+NUM_IMAGES))
		{
			selected_image_num = user_clicked_key-'0';
		}
	} while ( user_clicked_key != ESC );
	/*End of first do while*/
    return 1;
}
