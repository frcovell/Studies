#include <stdio.h>


int y; // file level scope, avalible to anything after this line
int x =1; 
int main(void){

    int x = 4; // masks previouse initalisation 

    {
        int x= = 5; // doesn't masks makes new memory new level of scope
    }

    printf("The value of x: %i\n", x);

    return 0;
}