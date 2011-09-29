#ifdef _CH_
#pragma package <opencv>
#endif

#include "cv.h"
#include "highgui.h"
#include <stdio.h>
#include <stdlib.h>
#include "../utilities.h"


#define NUM_IMAGES 4
#define THRESHOLD 50

// Locate the red pixels in the source image and return the percentage of red points found.
int find_spoons( IplImage* source, IplImage* result, IplImage* temp )
{
	int red_point_count = 0;
	int width_step=source->widthStep;
	int pixel_step=source->widthStep/source->width;
	int number_channels=source->nChannels;
	cvZero( result );
	unsigned char white_pixel[4] = {255,255,255,0};
	int row=0,col=0;
	// Find all red points in the image
	for (row=0; row < result->height; row++)
		for (col=0; col < result->width; col++)
		{
			unsigned char* curr_point = GETPIXELPTRMACRO( source, col, row, width_step, pixel_step );
			if ((curr_point[RED_CH] >= THRESHOLD) && ((curr_point[BLUE_CH] < THRESHOLD) || (curr_point[GREEN_CH] < THRESHOLD)))
			{
				PUTPIXELMACRO( result, col, row, white_pixel, width_step, pixel_step, number_channels );
			}
		}

	// Apply morphological opening and closing operations to clean up the image
	cvMorphologyEx( result, temp, NULL, NULL, CV_MOP_OPEN, 3 );
	cvMorphologyEx( temp, result, NULL, NULL, CV_MOP_CLOSE, 3 );

	// Count the red points remaining
	for (row=0; row < result->height; row++)
		for (col=0; col < result->width; col++)
		{
			unsigned char* curr_point = GETPIXELPTRMACRO( result, col, row, width_step, pixel_step );
			if (curr_point[RED_CH] == 255)
			{
				red_point_count++;
			}
		}

	return (red_point_count*100) / (result->height*result->width);
}

// Write out the number of spoons likely to be in the image on the basis of the percentage of red points
// located.  Normally this would be done by using a large number of training images in order to allow
// optimal classification.
void write_number_of_spoons_on_image( IplImage* image, int percentage_of_red_points )
{
	int num_spoons = (percentage_of_red_points < 3) ? 0 : (percentage_of_red_points > 7) ? 2 : 1;
	char spoons_text[100];
	sprintf(spoons_text,"%d spoon%c in can (Redness %% = %d%%)",num_spoons,(num_spoons==1) ? ' ':'s',percentage_of_red_points);
	write_text_on_image(image,1,1,spoons_text);
}

int main( int argc, char** argv )
{
	int selected_image_num = 1;
	IplImage* selected_image = NULL;
	IplImage* images[NUM_IMAGES];
	IplImage* temp_image = NULL;
	IplImage* result_image = NULL;

	// Load all the images.
	for (int file_num=1; (file_num <= NUM_IMAGES); file_num++)
	{
		char filename[100];
		sprintf(filename,"../Baby Food/BabyFoodCan%d.jpg",file_num);
		if( (images[file_num-1] = cvLoadImage(filename,-1)) == 0 )
			return 0;
	}

	// Explain the User Interface
    printf( "Hot keys: \n"
            "\tESC - quit the program\n");
    printf( "\t1..%d - select image\n",NUM_IMAGES);
    
	// Create display windows for images
    cvNamedWindow( "Original", 1 );
    cvNamedWindow( "Processed Image", 1 );

	// Create images to do the processing in.
	selected_image = cvCloneImage( images[selected_image_num-1] );
    result_image = cvCloneImage( selected_image );
    temp_image = cvCloneImage( selected_image );

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
		int red_percentage = find_spoons( selected_image, result_image, temp_image );
		write_number_of_spoons_on_image( result_image, red_percentage );
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
