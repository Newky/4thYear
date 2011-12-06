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
//Decide on numbers by
//hull convex to min bounding box ratio
//number of holes
//Number of concavities

struct pair {
	int start;
	int end;
};


void crop_image(IplImage*source, IplImage*result, int start, int end, int padding) {
	int width_step=source->widthStep;
	int pixel_step=source->widthStep/source->width;
	int width_step2=result->widthStep;
	int pixel_step2=result->widthStep/result->width;
	cvZero(result);
	int row=0,col;
	//Make it all white
	unsigned char white[] = {255, 255, 255};
	for(row =0;row<source->height;row++)
		for (col=0; col <source->width; col++)
			PUTPIXELMACRO( result, col, row, white, width_step2, pixel_step2,result->nChannels);

	for(row =0;row<source->height;row++) {
		for (col=start; col < end; col++){
			unsigned char* curr_point = GETPIXELPTRMACRO(source,col,row,width_step, pixel_step);
			PUTPIXELMACRO( result, (col-start)+(padding/2), row+(padding/2), curr_point, width_step2, pixel_step2,result->nChannels);
		}
	}
}

//source is 3 channel
//Find a number in a big image.
struct pair find_number(IplImage *source, int last_col) {
	IplImage* binary_image = cvCreateImage( cvGetSize(source), 8, 1 );
	cvConvertImage( source, binary_image );
	CvScalar c = cvAvg(source);
        float threshold = c.val[0];
	cvThreshold( binary_image, binary_image,threshold , 255, CV_THRESH_BINARY );
	int width_step=binary_image->widthStep;
	int pixel_step=binary_image->widthStep/binary_image->width;
	int row=0, col=0;
	int start = -1, end=-1;
	for(col=last_col;col<source->width;col++) {
		int column_count = 0;
		for(row=0;row<source->height;row++){
			unsigned char* curr_point = GETPIXELPTRMACRO(binary_image,col,row,width_step, pixel_step);
			if(*curr_point != 0)
				column_count++;
		}
		if(start == -1 && column_count > 6){
			start = col;
			//printf("Column Count:%d\n", column_count);
		}
		if(start != -1 && column_count ==0) {
			//printf("Column Count:%d\n", column_count);
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

IplImage * make_binary_image(IplImage * source){
        IplImage* binary_image = cvCreateImage( cvGetSize(source), 8, 1 );
        cvConvertImage( source, binary_image );
        IplImage * temp = cvCloneImage(binary_image);
        cvSmooth(temp, binary_image);
        CvScalar c = cvAvg(source);
        float threshold = c.val[0];
        cvThreshold( binary_image, binary_image, threshold, 255, CV_THRESH_BINARY );
        //cvMorphologyEx(binary_image, binary_image, NULL, NULL, CV_MOP_CLOSE, 1);

        cvReleaseImage(&temp);
        return binary_image;
}

int number_of_holes(IplImage*source, IplImage*binary_image) {
	//IplImage* binary_image = cvCreateImage( cvGetSize(source), 8, 1 );
	cvConvertImage( source, binary_image );
	CvMemStorage* storage = cvCreateMemStorage(0);
	CvSeq* contour = 0;
	cvThreshold( binary_image, binary_image, 150, 255, CV_THRESH_BINARY );
	cvFindContours( binary_image, storage, &contour, sizeof(CvContour),CV_RETR_CCOMP, CV_CHAIN_APPROX_SIMPLE);
	cvZero(source);
	int counter =0;
	for(;contour !=0;contour = contour->h_next, counter++){
		CvScalar color = CV_RGB(rand()&255, rand()&255, rand()&255);
		cvDrawContours( source, contour, color, color, -1, CV_FILLED, 8 );
	}
	int width_step=source->widthStep;
	int pixel_step=source->widthStep/source->width;
	//unsigned char white[] = {255, 255, 255};
	//for(int row=0;row<source->height;row+= source->height-1) 
		//for(int col=0;col<source->width;col++)
			//PUTPIXELMACRO( source, col, row, white, width_step, pixel_step,source->nChannels);
	//for(int col=0;col<source->width;col+= source->width-1)
		//for(int row=0;row<source->height;row++)
			//PUTPIXELMACRO( source, col, row, white, width_step, pixel_step,source->nChannels);
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
CvSeq* connected_components( IplImage* source, IplImage* result , int hack)
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

int number_of_pixels(IplImage * source) {
	int width_step=source->widthStep;
	int pixel_step=source->widthStep/source->width;
	int row=0,col=0,count=0;
	for(;row<source->height;row++) {
		for (col=0; col < source->width; col++){
			unsigned char* curr_point = GETPIXELPTRMACRO( source, col, row, width_step, pixel_step );
			count += (curr_point[RED_CH]!=0 && curr_point[GREEN_CH]!=0 && curr_point[BLUE_CH]!=0);
		}
	}
	return count;
}

// Structure to store features of a known or unknown character.
typedef struct tLicensePlateCharacterFeatures_tag {
	char name[10];
	double ratio;
	int holes;
	int pixels;
} tLicensePlateCharacterFeatures;


tLicensePlateCharacterFeatures analyse_image(IplImage * num_image , CvSeq * contour) {
	tLicensePlateCharacterFeatures features;
	int nb_pixels = number_of_pixels(num_image);
	if(nb_pixels > NUMBER_OF_NB_PIXELS) {
		features.pixels = nb_pixels;
		IplImage* hole_image= cvCloneImage(num_image);
		CvSeq* holes= connected_components(num_image, hole_image, 0);
		int i=0;
		for(CvSeq* cont =  holes; cont!= 0; cont= cont->h_next, i++ );
		features.holes = i-1;
		CvRect boundingbox = cvBoundingRect(contour);
		holes = cvConvexHull2(contour, 0, CV_CLOCKWISE, 1);
		//printf("MBR AREA:%d\n", boundingbox.width * boundingbox.height);
		float area = cvContourArea(holes);
		//printf("CH_AREA:%lf\n", area);
		features.ratio= area / (boundingbox.width * boundingbox.height);
	}else{
		features.pixels = nb_pixels;
		features.holes = -1;
	}
	return features;
}

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

void match_images(IplImage * incoming, IplImage* images[]) {
	//IplImage * binary_inc = make_binary_image(incoming);
	IplImage * connected = cvCreateImage(cvGetSize(incoming), incoming->depth, incoming->nChannels);
	cvShowImage( "Original", incoming);
	CvSeq* numbers = connected_components(incoming, connected, 15);
	int i=0;
	for(i=0;numbers !=0;numbers = numbers->h_next, i++){
		if(cvContourArea(numbers) < 50)
			continue;
		CvPoint2D32f cent;
		float radius;
		cvMinEnclosingCircle(numbers, &cent, &radius);
		char * window = (char *)malloc(sizeof(char) * 9);
		sprintf(window, "Number %d", (i+1));
		//temp is the number component but on the big image. (Single image)
		IplImage*temp= cvCreateImage(cvGetSize(incoming), incoming->depth, incoming->nChannels);
		IplImage*cropped_num;
		cvZero(temp);
		CvScalar color = CV_RGB(rand()&255, rand()&255, rand()&255);
		cvDrawContours(temp, numbers, color, color, -1, CV_FILLED, 8 );
		//This is working and the numbers are always consistent.
		//struct pair image_coords;
		//image_coords.start = 0;
		//image_coords.end= 0;
		//image_coords= find_number(temp, image_coords.end);
		CvRect bounding = cvBoundingRect(numbers, 0);
		// If either is -1, there is no number in it.
		//printf("\ne:%d s:%d\n", image_coords.end, image_coords.start);
			cvNamedWindow("Window2", 0);
			cvNamedWindow("Window3", 0);
			cvNamedWindow("Window4", 0);
			cvShowImage("Window2", temp);
			//cvSetImageROI(temp,cvRect(image_coords.start, 0,image_coords.end - image_coords.start,temp->height));
			//This is the small image
			cvSetImageROI(temp, bounding);
			cropped_num= cvCreateImage(cvSize(bounding.width+6, bounding.height+6), temp->depth, temp->nChannels);
			//cvCopyImage( temp, cropped_num );
			//IplImage*other2= cvCreateImage(cvSize(cropped_num->width+4, cropped_num->height+4), cropped_num->depth, cropped_num->nChannels);
			CvPoint offset = cvPoint(3, 3);
			cvCopyMakeBorder(temp, cropped_num, offset, IPL_BORDER_CONSTANT);
			cvShowImage("Window4", cropped_num);
			//cvWaitKey();
			IplImage * other3  = cvCreateImage(cvGetSize(cropped_num), cropped_num->depth, 1);
			IplImage* resized = cvCreateImage(cvGetSize(cropped_num), cropped_num->depth, 1);
			convert_color_to_black(cropped_num, other3);
			int best_diff=-1, best_diff_i=0;
			//cvShowImage("Sample1", other2);
			//cvWaitKey();
			IplImage * nothing = cvCloneImage(cropped_num);
			CvSeq* holes = connected_components(cropped_num, cropped_num , 0);
			int hole_no=0;
			//cvShowImage("Sample1", nothing);
			//cvWaitKey();
			for(CvSeq* cont =  holes; cont!= 0; cont= cont->h_next, hole_no++ );
			hole_no--;
			printf("%d\n", hole_no);
			if(hole_no == 2) {
				printf("8");
				write_text_on_image(connected, cent.y, cent.x, "8");
			}else{
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
				printf("%s", num_st);
				write_text_on_image(connected, cent.y, cent.x, num_st);
				free(num_st);
			}
			cvReleaseImage(&other3);
			cvReleaseImage(&resized);
		cvReleaseImage(&cropped_num);
	}
	printf("\n");
	cvShowImage("Processing", connected);
}


int main( int argc, char** argv )
{
	int selected_image_num = 2;
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
		match_images(selected_image, sample_number_images);
		user_clicked_key = cvWaitKey(0);
		if ((user_clicked_key >= '1') && (user_clicked_key <= '0'+NUM_IMAGES))
		{
			selected_image_num = user_clicked_key-'0';
		}
		
		//for(i=0;i<10;i++)
			//free(images[i]);
		//free(images);
		//free(numbers);

	} while ( user_clicked_key != ESC );

    return 1;
}
