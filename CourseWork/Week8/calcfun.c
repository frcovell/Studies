# include <stdio.h>

float divide(float num, float denom); //prototype function: allow main call divide before function defined



int main(void){

    int x, y;
    float f;

    x = 7;
    y = 3;

    f = x / y;
    
    printf("The result of int devision: %f\n", f);

    f = divide(x, y);

    printf("The result of devide using a function: %f\n",f);

    return 0;

}

float divide(float num, float denom){

    float res; // automatic local variable

    res = num / denom;

    return res;
}
