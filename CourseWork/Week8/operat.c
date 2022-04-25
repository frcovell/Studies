#include <stdoi.h>
include <stdbool.h> // allow bool instead of _Bool

int main(void){

    bool x = false; // can store 0 = 1-bite
    bool y = true; // def:1

    if (x){
        printf("x is true\n"); //wont run
    }

    int i = 1;
    if (i == 0){
        printf("i is false\n"); // will run
    } else {
        printf("i is true\n");
    }

  // binary logical operator
  // var1 && var2 : AND
  // var1 || var1 : OR


  // ternay conditional:  x ? y : z 
  

  
    return 0;
}