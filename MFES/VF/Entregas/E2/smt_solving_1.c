#include <stdio.h>
#define N 4

int main(){
    int M[N][N];
    int i = 1, j = 1;
    M[i][j] = i+j;
    j++;
    M[i][j] = i+j;
    j++;
    M[i][j] = i+j;
    j = 1; i++;
    M[i][j] = i+j;
    j++;
    M[i][j] = i+j;
    j++;
    M[i][j] = i+j;
    j = 1; i++;
    M[i][j] = i+j;
    j++;
    M[i][j] = i+j;
    j++;
    M[i][j] = i+j;

    printf("Sucess with i = %d, j = %d, M[i][j] = %d", i, j, M[i][j]);

    return 0;
}
