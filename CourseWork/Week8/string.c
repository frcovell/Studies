#include <stdio.h>
#include <string.h>

int main(void){

    char chars[] = {'H', 'e', 'K', '\0'};

    char msg[] = "HeK"; // string representation

    printf(" length of chars: %i\n", strlen(chars));
    printf("length of msg: %i\n", strlen(msg));

    
    return 0;
}