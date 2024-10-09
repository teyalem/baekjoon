#include <stdio.h>

int max(int a, int b) {
    return a > b ? a : b;
}

int min(int a, int b) {
    return a < b ? a : b;
}

int ip[3] = {0, 0, 0};
int ap[3] = {0, 0, 0};
int in[3];
int an[3];
int row[3];

int main() {
    int i, n;
    scanf("%d", &n);

    while (n > 0) {
        for (i = 0; i < 3; i++) {
            scanf("%d", &row[i]);
        }

        in[0] = row[0] + min(ip[0], ip[1]);
        in[1] = row[1] + min(ip[0], min(ip[1], ip[2]));
        in[2] = row[2] + min(ip[1], ip[2]);

        an[0] = row[0] + max(ap[0], ap[1]);
        an[1] = row[1] + max(ap[0], max(ap[1], ap[2]));
        an[2] = row[2] + max(ap[1], ap[2]);

        for (i = 0; i < 3; i++) {
            ip[i] = in[i];
            ap[i] = an[i];
        }

        n--;
    }

    printf("%d %d",
            max(ap[0], max(ap[1], ap[2])),
            min(ip[0], min(ip[1], ip[2])));

    return 0;
}
