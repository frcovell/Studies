#include <stdio.h>

int main(void){

    int myarray1[5]; // explicite definition/sizing of array
    myarray1[0] = 0; // initialize index from 0, without initialising array contain garbage


    int x; 
    for( x = 0; x < 5; ++x){ // use loop to initialise array
        myarray1[x] = 0;
    }

    int myarray2[] = {7, 9, 21, 55, 4, 18}; // implicite 



    return 0;

}