#include <stdio.h>
#include <stdlib.h>

struct node{
    struct node* left;
    struct node* right;
    struct node* parent;
    int index;
    char* name;

};

typedef struct node node;

void traverse_node(node *n){

    traverse_node(n)

}

int main(void){

    node n1;
    node n2;
    node n3;

    n3.left = &n1;
    n3.right = &n2;


    return 0;
}