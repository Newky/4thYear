#ifdef _CH_
#pragma package <opencv>
#endif

#include "cv.h"
#include "highgui.h"
#include <stdio.h>
#include <stdlib.h>
#include "../utilities.h"
#define NUM_IMAGES 9
#define NUMBER_OF_KNOWN_CHARACTERS 10
#define NUMBER_OF_BLACK_PIXELS 10
#define NUMBER_OF_NB_PIXELS 100 

/*A simple pair structure for storing the number and its position in the picture
 * Used for printing out the picture.*/
struct pair {
	int number;
	float position;
};
/* Takes a source image
 * and returns a binary version of that image.
 */
IplImage * make_binary_image(IplImage * source){
        IplImage* binary_image = cvCreateImage( cvGetSize(source), 8, 1 );
        cvConvertImage( source, binary_image );
        IplImage * temp = cvCloneImage(binary_image);
        cvSmooth(temp, binary_image);
        CvScalar c = cvAvg(source);
        float threshold = c.val[0];
        cvThreshold( binary_image, binary_image, threshold, 255, CV_THRESH_BINARY );

        cvReleaseImage(&temp);
        return binary_image;
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
/* Returns a CvSeq which is basically a list of 
 * Points which are connected
 */
CvSeq* connected_components( IplImage* source, IplImage* result)
{
	//Create a one channel image of the source file.
	IplImage* binary_image = cvCreateImage( cvGetSize(source), 8, 1 );
	cvConvertImage( source, binary_image );
	//Allocate some storage
	CvMemStorage* storage = cvCreateMemStorage(0);
	CvSeq* contours = 0;
	CvScalar c = cvAvg(source);
	float threshold = c.val[0];
	cvThreshold( binary_image, binary_image, threshold-15, 255, CV_THRESH_BINARY );
	IplImage * inverted = cvCreateImage(cvGetSize(binary_image),8, 1);
	invert_image(binary_image, inverted);
	cvFindContours( inverted, storage, &contours, sizeof(CvContour),CV_RETR_CCOMP, CV_CHAIN_APPROX_SIMPLE );
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
			cvDrawContours( result, contour, color, color, -1, CV_FILLED, 8 );
		}
	}
	//Returns the outer most contour as a list of contours
	return contours;
}

// Structure to store features of a known or unknown character.
typedef struct tLicensePlateCharacterFeatures_tag {
	char name[10];
	double ratio;
	int holes;
	int pixels;
} tLicensePlateCharacterFeatures;


/* Takes a image and a template
 * with which to compare.
 * XOR's the two, leaving any differences as 255.
 * This counts the number of different pixels
 * and returns its as an int.
 */
int template_match(IplImage* src, IplImage* dst) {
	IplImage * temp = cvCreateImage(cvGetSize(src), src->depth, src->nChannels);
	cvXor(src, dst, temp);
	int width_step=src->widthStep;
	int pixel_step=src->widthStep/src->width;
	int diff =0;
	int row=0,col=0;
	for (row=0; row < temp->height; row++){
		for(col=0;col<temp->width;col++){
			unsigned char* curr_point = GETPIXELPTRMACRO( temp, col, row, width_step, pixel_step );
			diff += (*curr_point == 255);
		}
	}
	return diff;
}

/* Given a 3 channel image.
 * Convert it to a single channel
 * But make anything that is black white, 
 * and anything that is any other color black.
 */
void convert_color_to_black(IplImage * src, IplImage * dst) {
	int width_step=src->widthStep;
	int pixel_step=src->widthStep/src->width;
	int width_step2=dst->widthStep;
	int pixel_step2=dst->widthStep/dst->width;
	int row=0,col=0;
	unsigned char black[] = {0};
	unsigned char white[] = {255};
	for (row=0; row < src->height; row++){
		for(col=0;col<src->width;col++){
			unsigned char* curr_point = GETPIXELPTRMACRO( src, col, row, width_step, pixel_step );
			if(curr_point[RED_CH] == 0 && curr_point[BLUE_CH] == 0 && curr_point[GREEN_CH] == 0) {
				PUTPIXELMACRO( dst, col, row, white, width_step2, pixel_step2,dst->nChannels);	
			}else{
				PUTPIXELMACRO( dst, col, row, black, width_step2, pixel_step2,dst->nChannels);
			}
		}
	}
}
/* Main function, Takes the initial picture
 * And a picture to render the results on.
 * It also takes an array of the sample photos.
 */
void match_images(IplImage * incoming, IplImage* display, IplImage* images[]) {
	// An image which holds the connected components image.
	IplImage * connected = cvCreateImage(cvGetSize(incoming), incoming->depth, incoming->nChannels);
	cvShowImage( "Original", incoming);
	CvSeq* numbers = connected_components(incoming, connected);
	// Count the number of numbers in the licence plate.
	// This will count letters.
	int no_of_no = 0;
	for(CvSeq* numb = numbers;numb !=0;numb = numb->h_next)
		if(cvContourArea(numb) > 50)
			no_of_no++;
	struct pair num_hold[no_of_no];
	int i=0, j=0;
	//For each contour found using connected components
	for(i=0;numbers !=0;numbers = numbers->h_next, i++){
		//Discard anything which shows an area greater than some number.
		if(cvContourArea(numbers) < 50)
			continue;
		//Find its Centre and Radius (used for writing on image.)
		CvPoint2D32f cent;
		float radius;
		cvMinEnclosingCircle(numbers, &cent, &radius);	
		/* Temp is a single component.
		 * But connected components preserves image position, so first I must render it on
		 * an image the size of the original.
		 */
		IplImage*temp= cvCreateImage(cvGetSize(incoming), incoming->depth, incoming->nChannels);
		IplImage*cropped_num;
		cvZero(temp);
		CvScalar color = CV_RGB(255, 255, 255);
		cvDrawContours(temp, numbers, color, color, -1, CV_FILLED, 8 );
		/* Next I use cvBoundingBox
		 * to draw around the number so that I can crop the image to that size.
		 * I add a border of size 3 all around the image.
		 * This helps with the processing.
		 */
		CvRect bounding = cvBoundingRect(numbers, 0);
		cvSetImageROI(temp, bounding);
		cropped_num= cvCreateImage(cvSize(bounding.width+6, bounding.height+6), temp->depth, temp->nChannels);
		CvPoint offset = cvPoint(3, 3);
		cvCopyMakeBorder(temp, cropped_num, offset, IPL_BORDER_CONSTANT);
		/* To do the template match, we must make 
		 * the image 1 channel grayscale.*/
		IplImage * other3  = cvCreateImage(cvGetSize(cropped_num), cropped_num->depth, 1);
		IplImage* resized = cvCreateImage(cvGetSize(cropped_num), cropped_num->depth, 1);
		convert_color_to_black(cropped_num, other3);
		int best_diff=-1, best_diff_i=0;
		/* Also get the number of holes present in the image.*/
		CvSeq* holes = connected_components(cropped_num, cropped_num);
		int hole_no=0;
		for(CvSeq* cont =  holes; cont!= 0; cont= cont->h_next, hole_no++ );
		hole_no--;
		// If theres two holes, u assume its an 8 (only number with two holes)
		if(hole_no == 2) {
			num_hold[j].number= 8;
			num_hold[j].position = cent.x;
			write_text_on_image(display, cent.y, cent.x, "8");
		}else{
			// Otherwise just loop through all the sample images 
			// and pick out the template with the least difference.
			for(int i=0;i<NUMBER_OF_KNOWN_CHARACTERS;i++) {
				if(hole_no == 0)
					if(i == 8 || i ==0 || i == 4 || i == 6 || i == 9)
						continue;
				else if(hole_no == 1)
					if(i == 8 || i == 1 || i == 2 || i == 3 || i == 5 || i == 6 || i == 7)
						continue;
				IplImage * bin_image = cvCloneImage(images[i]);
				cvResize(bin_image, resized, CV_INTER_NN);
				int diff = template_match(other3,resized);
				if(best_diff == -1 | diff < best_diff){
					best_diff = diff;
					best_diff_i = i;
				}
				
			}
			char * num_st = (char *)malloc(sizeof(char) * 2);
			sprintf(num_st, "%d", best_diff_i);
			num_hold[j].number= best_diff_i;
			num_hold[j].position = cent.x;
			write_text_on_image(display, cent.y, cent.x, num_st);
			free(num_st);
		}
		//Release the images.
		cvReleaseImage(&other3);
		cvReleaseImage(&resized);
		cvReleaseImage(&cropped_num);
		cvReleaseImage(&connected);
		j++;
	}
	/* Because the connected components takes the numbers
	 * in a different order than what would be 
	 * expected, I had to store the numbers and their position
	 * in an array, I then used this to print them to the output 
	 * correctly, so that I could submit proper terminal output.
	 */
	float min = display->width, limit =0;
       	int min_index = -1;
	for(i=0;i<no_of_no;i++) {
		for(j=0;j<no_of_no;j++) {
			if(num_hold[j].position < min && num_hold[j].position > limit) {
				min = num_hold[j].position;
				min_index = num_hold[j].number;
			}
		}
		printf("%d", min_index);
		limit = min;
		min = display->width;
	}
	printf("\n");
	cvShowImage("Processing", display);
}


int main( int argc, char** argv )
{
	int selected_image_num = 1;
	IplImage* selected_image = NULL;
	IplImage* sample_number_images[NUMBER_OF_KNOWN_CHARACTERS];
	IplImage* images[NUM_IMAGES];
	tLicensePlateCharacterFeatures known_object_features[NUMBER_OF_KNOWN_CHARACTERS];
	tLicensePlateCharacterFeatures unknown_object_features[100];
	// Load all the sample images and determine feature values for these characters.
	//
	// These are the plain text images.
	for (int character=0; (character<NUMBER_OF_KNOWN_CHARACTERS); character++)
	{
		char filename[100];
		sprintf(filename,"RealNumbers/%d.jpg",character);
		if( (sample_number_images[character] = cvLoadImage(filename,-1)) == 0 )
			return 0;
		sprintf(known_object_features[character].name,"%d",character);
		sample_number_images[character] = make_binary_image(sample_number_images[character]);
	}




	// Load all the unknown license plate images.
	for (int file_num=1; (file_num <= NUM_IMAGES); file_num++)
	{
		char filename[100];
		sprintf(filename,"./LicensePlate%d.jpg",file_num);
		if( (images[file_num-1] = cvLoadImage(filename,-1)) == 0 )
			return 0;
		int x = 24, y=10;
		cvSetImageROI(images[file_num-1],cvRect(x,y, images[file_num-1]->width - x,images[file_num-1]->height - y));
	}


	// Explain the User Interface
	printf( "Hot keys: \n"
	    "\tESC - quit the program\n");
	printf( "\t1..%d - select image\n",NUM_IMAGES);

	// Create display windows for images
	cvNamedWindow( "Original", 1 );
	cvMoveWindow("Original", 0, 0);
	cvNamedWindow( "Processing", 0 );
	cvMoveWindow("Processing", 360, 0);

	// Create images to do the processing in.
	selected_image = cvCreateImage(cvGetSize(images[selected_image_num]), images[selected_image_num]->depth, images[selected_image_num]->nChannels);

	// Setup mouse callback on the original image so that the user can see image values as they move the
	// cursor over the image.
	cvSetMouseCallback( "Original", on_mouse_show_values, 0 );
	window_name_for_on_mouse_show_values="Original";
	image_for_on_mouse_show_values=selected_image;

	int user_clicked_key = 0;
	do {
		cvCopyImage( images[selected_image_num-1], selected_image );
		IplImage * display = cvCloneImage(selected_image);
		match_images(selected_image,display, sample_number_images);
		user_clicked_key = (char)cvWaitKey(0);
		if ((user_clicked_key >= '1') && (user_clicked_key <= '0'+NUM_IMAGES))
		{
			selected_image_num = user_clicked_key-'0';
		}
	} while ( user_clicked_key != ESC );
    return 1;
}
