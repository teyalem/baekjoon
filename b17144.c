#include <stdio.h>
#include <stdlib.h>

#define MAXN 50

int rd[4] = { 0, 0, -1, 1 };
int cd[4] = { -1, 1, 0, 0 };

int map[MAXN+1][MAXN+1];
int tmp[MAXN+1][MAXN+1];

void diffuse(int r, int c) {
    int i, j, k, d, cnt, x, y;

    for (i = 1; i <= r; i++) {
        for (j = 1; j <= c; j++) {
            cnt = 0;
            d = map[i][j] / 5;

            for (k = 0; k < 4; k++) {
                x = j + cd[k];
                y = i + rd[k];
                if (x < 1 || c < x || y < 1 || r < y || map[y][x] == -1) continue;
                tmp[y][x] += d;
                cnt++;
            }

            map[i][j] -= cnt * d;
        }
    }

    for (i = 1; i <= r; i++) {
        for (j = 1; j <= c; j++) {
            map[i][j] += tmp[i][j];
            tmp[i][j] = 0;
        }
    }
}

void circulate(int r, int c, int at) {
    int i;

    for (i = at-2; i >= 1; i--)
        map[i+1][1] = map[i][1];
    for (i = at+3; i <= r; i++)
        map[i-1][1] = map[i][1];

    for (i = 2; i <= c; i++)
        map[1][i-1] = map[1][i];
    for (i = 2; i <= c; i++)
        map[r][i-1] = map[r][i];

    for (i = 2; i <= at; i++)
        map[i-1][c] = map[i][c];
    for (i = r-1; i >= at+1; i--)
        map[i+1][c] = map[i][c];

    for (i = c-1; i >= 2; i--)
        map[at][i+1] = map[at][i];
    for (i = c-1; i >= 2; i--)
        map[at+1][i+1] = map[at+1][i];

    map[at][2] = 0;
    map[at+1][2] = 0;
}

int count_dust(int r, int c) {
    int i, j, cnt;
    cnt = 0;

    for (i = 1; i <= r; i++) {
        for (j = 1; j <= c; j++) {
            if (map[i][j] > 0)
                cnt += map[i][j];
        }
    }

    return cnt;
}

int main() {
    int r, c, t, i, j, at;
    at = 0;

    scanf("%d %d %d", &r, &c, &t);

    for (i = 1; i <= r; i++) {
        for (j = 1; j <= c; j++) {
            scanf(" %d", &map[i][j]);
            if (map[i][j] == -1 && at == 0) at = i;
            tmp[i][j] = 0;
        }
    }

    for (i = 0; i < t; i++) {
        diffuse(r, c);
        circulate(r, c, at);
    }

    printf("%d", count_dust(r, c));

    return 0;
}
