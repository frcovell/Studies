#include <stdio.h>
#include <stdlib.h>
#include <string.h>


void* my_malloc(size_t s){

    void* new_mem = NULL;

    new_mem = malloc(s);
    if(new_mem){
        mumset(new_mem, 0, s); //Inirialises all 0s
    } else{
        printf("Insufficient memory\n");
        exit(EXIT_FAILURE)); //control program crash
    }
}
int main(void){

    int* intblock;

    intblock = NULL; //0x0

    intblock = my_malloc(10 * sizeof(int));
    if (intblock != NULL){
    free(intblock); //free allocated pointer, can't free unallocated memory
    intblock = NULL; // stops memory is floating
    }
   

    return 0;
}