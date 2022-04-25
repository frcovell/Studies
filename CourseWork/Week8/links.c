#include <stdio.h>
#include <stdlib.h>


struct ilink{
  
    int data;
    struct ilink *next;
    struct ilink *back;

};
typedef struct ilink ilink; //give alias to stop the need to type struct

void traverse_list(ilink *p){
    if(p != NULL){
        //printf("%i\n", p->data); //start from 1 continue on
        traverse_list(p->next); //equivilant:((*p)next)
        printf("%i\n", p->data); // start at end and goes back
    }
}
int main(void){

    ilink i1;
    ilink i2;
    ilink i3;

    i1.data = 47;
    i2.data = 23;
    i3.data = 9;

    i1.next = &i2;
    i2.next = &i3;
    i3.next = NULL;

    traverse_list(&i1);

    //removing elements frm list
    printf("eliminating element:\n"); 
    
    //i1.next = &i3; // eliminates i2
    //OR
    i1.next = i1.next -> next;

    traverse_list(&i1); 
    return 0;
}