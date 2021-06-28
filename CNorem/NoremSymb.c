#include "Norem.h"
#include <string.h>


typedef symb_t char*;

typedef struct SymbTree_t {
    union {
        SymbTree_t page[256];
        char name[1024];
    }
} SymbTree_t;

SymbTree_t* symb_root;

void symb_root_init() {
    symb_root = empty_tree();
}

symb_t* to_symb(char* str) {
    SymbTree_t* branch;
    branch = symb_root;
    char* str_rest = str;
    char init = str_rest[0];
    
    while(init != '\0') {
        if(branch->page[init] == NULL) {
            // a new page is created
            branch->page[init] = empty_tree();
        }
        // enter the branch
        branch = branch->page[init];
        // read next init char
        str_rest ++;
        init = str_rest[0];
    }
    if(branch->page['\0'] == NULL) {
        // a new page is created
        branch->page['\0'] = empty_tree();
        strcpy(branch->page['\0'],str);
    }
    return branch.page['\0'].name;
}

SymbTree_t* empty_tree() {
    SymbTree_t* tree = malloc(sizeof(SymbTree_t));
    for(int i=0; i<256; i++) {
        tree.page[i] = NULL;
    }
    return tree;
}







Symb_t* dict_base;
Symb_t* dict_ceil;
Symb_t* dict_ptr;

void dict_init() {
    dict_base = malloc(DICT_SIZE * sizeof(Symb_t));
    printf("dict_init: %o\n", dict_base);
    dict_ceil = dict_base + DICT_SIZE;
    dict_ptr = dict_base;
}


Symb_t* def_basic(char* name, Term_t* (fn)()) {
    Symb_t* symb = to_symb(name);
    symb->state = BASIC;
    symb->fn = fn;
    return symb;
}

Symb_t* def_symb(char* name, Term_t* value) {
    Symb_t* symb = to_symb(name);
    symb->state = LINK;
    symb->value = value;
    return symb;
}

void show_dict() {
    Symb_t* ptr = dict_base;
    printf("---------------------------\n");
    printf("-- DICTIONARY DEFINATION --\n");
    printf("---------------------------\n");
    while(ptr < dict_ptr) {
        printf("%s\t->\t",ptr->key);
        switch(ptr->state) {
            case UNDEF:
                printf("?\n");
                break;
            case BASIC:
                printf("basic\n");
                break;
            case LINK:
                show_term(ptr->value);
                printf("\n");
                break;
        }
        ptr ++;
    }
    printf("---------------------------\n");
}






typedef struct DictNode_t {
    int key;
    int value;
    uint32_t height;
    struct DictNode_t* left;
    struct DictNode_t* right;
} DictNode_t;






DictNode_t* new_node(DictNode_t* left, DictNode_t* right) {
    DictNode_t* node;
    node = malloc(sizeof(DictNode_t));
    node->height = left->height + right->height;
    node->left = left;
    node->right = right;
}


int balance_factor(DictNode_t* node) {
    if(node == NULL) {
        return 0;
    } else {
        return node->left->height - node->right->height;
    }
}


DictNode_t* 


DictNode_t* tree_rotate_right(DictNode_t* node) {
    DictNode_t* temp = node->left;
    
    node->left = node->right;
    temp->right = node;

    left->height = max()




}

nodeptr_t treeRotateRight(nodeptr_t root) {
    nodeptr_t left = root->left;
    
    root->left = left->right; // 将将要被抛弃的节点连接为旋转后的 root 的左孩子
    left->right = root; // 调换父子关系

    left->height = max(treeHeight(left->left), treeHeight(left->right))+1;
    right->height = max(treeHeight(right->left), treeHeight(right->right))+1;
    
    return left;
}


DictNode_t* tree_rebalance(DictNode_t* node) {
    int factor = balance_factor(node);
    if(factor > 1 && balance_factor(node->left) > 0) // LL
        return treeRotateRight(root);
    else if(factor > 1 && balance_factor(node->left) <= 0) { //LR
        node->left = treeRotateLeft(node->left);
        return treeRotateRight(temp);
    } else if(factor < -1 && balance_factor(node->right) <= 0) // RR
        return treeRotateLeft(root);
    else if((factor < -1 && balance_factor(node->right) > 0) { // RL
        node->right = treeRotateRight(root->right);
        return treeRotateLeft(root);
    } else { // Nothing happened.
        return node;
    }
}