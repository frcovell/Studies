#include <stdio.h>

int main(void){
    int a;
    int b;
    int c;

    a = 1 + 2;
    b = 7;
    c = a + b;
    printf("The result of a + b using c: %i\n", c);
    printf("The result of a + b %i\n", a + b);

    c = b / a;

    printf("The result of b / a : %i\n", b / a);

    float d;
//    float e;

//   e = b;
// d = e / a;

    d = (float)b / a; // type casting, be careful when using 

    printf("The result of b / a : %f\n", d);

    //using constant literals

//   d = 7 / 3 // will trucate assumed to be int 
//   printf("The result of b / a : %f\n", d);

//   d = 7f / 3 // 7 becomes a float 
//   printf("The result of b / a : %f\n", d);


    return 0;
    
}