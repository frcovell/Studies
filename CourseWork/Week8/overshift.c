#include <stdio.h>

int main(void){

    signed char s; //"signed" isnt needed
    unsigned char u;

    signed char sres; //"signed" isnt needed, truncates absolute value
    unsigned char ures;

    s = 255; // 1 in binary: 00000001
    u = 255; // in binary: 11111111

    sres = s <<((char)7);
    printf("%i\n", sres);
    ures =  u << ((char)7);
    printf("%u\n", ures);

    printf("%i\n", s << (char)8);
    printf("%u\n", u << (char)8);
    
    printf("%i\n", s >> (char)8);
    printf("%u\n", u >> (char)8);
    
    

    return 0;
}