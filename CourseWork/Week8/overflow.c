#include <stdio.h>
#include <unistd.h> // << not portable: Unix Standard

int main(void){
    unsigned short int i = 1;

    do{
        i *= 2; // i = i * 2
        printf("%u\n", i);
        sleep(1);    
    } while(1);

    return 0;
}