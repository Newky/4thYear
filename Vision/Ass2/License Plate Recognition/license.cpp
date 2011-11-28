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
//Decide on numbers by
//hull convex to min bounding box ratio
//number of holes
//Number of concavities

struct pair {
	int start;
	int end;
};

void crop_image(IplImage*source, IplImage*result, int start, int end) {
	int width_step=source->widthStep;
	int pixel_step=source->widthStep/source->width;
	int width_step2=result->widthStep;
	int pixel_step2=result->widthStep/result->width;
	cvZero(result);
	int row=0,col;
	for(row =0;row<source->height;row++) {
		for (col=start; col < end; col++){
			unsigned char* curr_point = GETPIXELPTRMACRO(source,col,row,width_step, pixel_step);
			PUTPIXELMACRO( result, col-start, row, curr_point, width_step2, pixel_step2,result->nChannels);
		}
	}
}

//source is 3 channel
struct pair find_number(IplImage *source, int last_col) {
	IplImage* binary_image = cvCreateImage( cvGetSize(source), 8, 1 );
	cvConvertImage( source, binary_image );
	cvThreshold( binary_image, binary_image, 125, 255, CV_THRESH_BINARY );
	int width_step=binary_image->widthStep;
	int pixel_step=binary_image->widthStep/binary_image->width;
	int row=0, col=0;
	int start = -1, end=-1;
	for(col=last_col;col<source->width;col++) {
		int column_count = 0;
		for(row=0;row<source->height;row++){
			unsigned char* curr_point = GETPIXELPTRMACRO(binary_image,col,row,width_step, pixel_step);
			if(*curr_point != 255)
				column_count++;
		}
		if(start == -1 && column_count > NUMBER_OF_BLACK_PIXELS){
			start = col;
			printf("%d", column_count);
			//break;
		}
		if(start != -1 && column_count  < 2) {
			end = col;
			break;
		}
	}
	if(end == -1) 
		end = col;
	struct pair x;
	x.start = start;
	x.end = end;
	return x;
}

double area_with_n_vertices(int tiX,int tiY,int riX,int riY,int biX,int biY,int liX,int liY) {
	double area = ((tiX* riY) - (tiY*riX));
	area += ((riX* biY) - (riY*biX));
	area += ((biX* liY) - (biY*liX));
	area += ((liX* tiY) - (liY*tiX));
	return area;
};

int number_of_holes(IplImage*source) {
	IplImage* binary_image = cvCreateImage( cvGetSize(source), 8, 1 );
	cvConvertImage( source, binary_image );
	CvMemStorage* storage = cvCreateMemStorage(0);
	CvSeq* contour = 0;
	cvThreshold( binary_image, binary_image, 125, 255, CV_THRESH_BINARY );
	cvFindContours( binary_image, storage, &contour, sizeof(CvContour),CV_RETR_CCOMP, CV_CHAIN_APPROX_SIMPLE );
	cvZero(source);
	int counter =0;
	for(;contour !=0;contour = contour->h_next, counter++){
		CvScalar color = CV_RGB(rand(), rand(), rand());
		cvDrawContours( source, contour, color, color, -1, CV_FILLED, 8 );
	}
	return counter -1;
};

double ratio_mbr_convex_hull(IplImage*binary_image) {
	int width_step=binary_image->widthStep;
	int pixel_step=binary_image->widthStep/binary_image->width;
	int row=0,col;
	int liX=-1,riX=-1,tiY=-1,biY=-1;
	int liY=-1,riY=-1,tiX=-1,biX=-1;
	for(;row<binary_image->height;row++) {
		for(col=0;col<binary_image->width;col++){
			unsigned char* curr_point = GETPIXELPTRMACRO(binary_image,col,row,width_step, pixel_step);
			if(*curr_point < 100){
				biY = row;biX = col;
				if(col > riX){
					riX = col;riY = row;
				}
				if(tiY == -1){
					tiY = row;tiX=col;
				}
				if(liX == -1){
					liX=col;liY=row;
				}
				if(col < liX){
					liX=col;liY=row;
				}
			}
		}	
	}
	double convex_area =area_with_n_vertices(tiX,tiY, riX,riY, biX,biY,liX, liY);
	int mbr_area = (biY-tiY) * (riX- liX);
	return convex_area / mbr_area;
}


// Structure to store features of a known or unknown character.
typedef struct tLicensePlateCharacterFeatures_tag {
	char name[10];
	double ratio;
	int holes;
} tLicensePlateCharacterFeatures;



int main( int argc, char** argv )
{
	int selected_image_num = 1;
	IplImage* selected_image = NULL;
	IplImage* sample_number_images[NUMBER_OF_KNOWN_CHARACTERS];
	IplImage* images[NUM_IMAGES];
	tLicensePlateCharacterFeatures known_object_features[NUMBER_OF_KNOWN_CHARACTERS];
	tLicensePlateCharacterFeatures unknown_object_features[100];

	// Load all the sample images and determine feature values for these characters.
	// These are the plain text images.
	for (int character=0; (character<NUMBER_OF_KNOWN_CHARACTERS); character++)
	{
		char filename[100];
		sprintf(filename,"./%d.jpg",character);
		if( (sample_number_images[character] = cvLoadImage(filename,-1)) == 0 )
			return 0;
		sprintf(known_object_features[character].name,"%d",character);
		known_object_features[character].ratio = ratio_mbr_convex_hull(sample_number_images[character]);
		known_object_features[character].holes = number_of_holes(sample_number_images[character]);
		printf("%d) %lf , %d\n", character, known_object_features[character].ratio, known_object_features[character].holes);
	}

	cvNamedWindow("Sample Window", 1);
	cvShowImage("Sample Window", sample_number_images[4]);


	// Load all the unknown license plate images.
	for (int file_num=1; (file_num <= NUM_IMAGES); file_num++)
	{
		char filename[100];
		sprintf(filename,"./LicensePlate%d.jpg",file_num);
		if( (images[file_num-1] = cvLoadImage(filename,-1)) == 0 )
			return 0;
		int x = 24, y=15;
		cvSetImageROI(images[file_num-1],cvRect(x,y, images[file_num-1]->width - x,images[file_num-1]->height - y));
	}


	// Explain the User Interface
	printf( "Hot keys: \n"
	    "\tESC - quit the program\n");
	printf( "\t1..%d - select image\n",NUM_IMAGES);

	// Create display windows for images
	cvNamedWindow( "Original", 1 );
	cvNamedWindow( "Processing", 0 );
	cvNamedWindow( "Processing2", 0 );

	// Create images to do the processing in.
	selected_image = cvCreateImage(cvGetSize(images[selected_image_num]), images[selected_image_num]->depth, images[selected_image_num]->nChannels);

	// Setup mouse callback on the original image so that the user can see image values as they move the
	// cursor over the image.
	cvSetMouseCallback( "Original", on_mouse_show_values, 0 );
	//window_name_for_on_mouse_show_values="Original";
	//image_for_on_mouse_show_values=selected_image;

	int user_clicked_key = 0;
	do {
		// Process image (i.e. setup and find the number of spoons)
		cvCopyImage( images[selected_image_num-1], selected_image );
		struct pair image_coords = find_number(selected_image, 0);
		printf("\n%d %d\n", image_coords.start, image_coords.end);
		int height = selected_image->height;
		IplImage * cropped = cvCreateImage(cvSize(image_coords.end - image_coords.start, height), selected_image->depth, selected_image->nChannels);
		crop_image(selected_image, cropped, image_coords.start, image_coords.end);
		cvShowImage( "Processing2", cropped);
		printf("\nRatio:%lf Holes:%d\n",ratio_mbr_convex_hull(cropped), number_of_holes(cropped));
		printf("\nProcessing Image %d:\n",selected_image_num);
		cvShowImage( "Processing", cropped);
		cvShowImage( "Original", selected_image);

		// Wait for user input
		user_clicked_key = cvWaitKey(0);
		if ((user_clicked_key >= '1') && (user_clicked_key <= '0'+NUM_IMAGES))
		{
			selected_image_num = user_clicked_key-'0';
		}
	} while ( user_clicked_key != ESC );

    return 1;
}
