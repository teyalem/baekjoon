#include <stdio.h>
#include <stdlib.h>

#define MAX_N 10

int rd[8] = { -1, -1, -1, 0, 1, 1,  1,  0 };
int cd[8] = { -1,  0,  1, 1, 1, 0, -1, -1 };

typedef struct tree {
    int r, c, age, cnt;
    struct tree *next, *prev;
} Tree;

typedef struct tree_head {
    Tree *first;
} TreeHead;

Tree *new_tree(int r, int c, int age, int cnt) {
    Tree *t = malloc(sizeof(Tree));
    t->r = r;
    t->c = c;
    t->age = age;
    t->cnt = cnt;
    t->next = NULL;
    t->prev = NULL;

    return t;
}

void add_tree(TreeHead *head, Tree *t) {
    t->next = head->first;
    if (head->first != NULL)
        head->first->prev = t;
    head->first = t;
}

void del_tree(TreeHead *head, Tree *t) {
    if (head->first == t) {
        head->first = t->next;
        return;
    }

    if (t->prev != NULL)
        t->prev->next = t->next;
    if (t->next != NULL)
        t->next->prev = t->prev;
}

int map[MAX_N+1][MAX_N+1];
int fert[MAX_N+1][MAX_N+1];
int dead[MAX_N+1][MAX_N+1];
int seed[MAX_N+1][MAX_N+1];

TreeHead live = { NULL };

void year(int n) {
    Tree *t, *nt;
    int r, c, live_cnt, dead_cnt;

    // spring & autumn
    t = live.first;
    while (t != NULL) {
        nt = t->next;

        live_cnt = map[t->r][t->c] / t->age;
        if (live_cnt > t->cnt) live_cnt = t->cnt;
        dead_cnt = t->cnt - live_cnt;

        map[t->r][t->c] -= t->age * live_cnt;
        dead[t->r][t->c] += t->age/2 * dead_cnt;

        t->age++;
        t->cnt = live_cnt;

        if (live_cnt > 0 && t->age % 5 == 0) {
            for (int i = 0; i < 8; i++) {
                r = t->r + rd[i];
                c = t->c + cd[i];

                if (r < 1 || n < r || c < 1 || n < c) continue;
                seed[r][c] += live_cnt;
            }
        }

        if (live_cnt == 0) del_tree(&live, t);

        t = nt;
    }

    // summer & winter
    for (r = 1; r <= n; r++) {
        for (c = 1; c <= n; c++) {
            map[r][c] += fert[r][c] + dead[r][c];
            if (seed[r][c] > 0)
                add_tree(&live, new_tree(r, c, 1, seed[r][c]));
            dead[r][c] = 0;
            seed[r][c] = 0;
        }
    }
}

int count_living() {
    int cnt = 0;
    Tree *t = live.first;

    while (t != NULL) {
        cnt += t->cnt;
        t = t->next;
    }

    return cnt;
}

int main() {
    int n, m, k;
    int i, j;
    int r, c, age;

    scanf("%d %d %d", &n, &m, &k);
    for (i = 1; i <= n; i++) {
        for (j = 1; j <= n; j++) {
            map[i][j] = 5;
            dead[i][j] = 0;
            seed[i][j] = 0;
            scanf(" %d", &fert[i][j]);
        }
    }
    for(i = 0; i < m; i++) {
        scanf(" %d %d %d", &r, &c, &age);
        add_tree(&live, new_tree(r, c, age, 1));
    }

    for (i = 0; i < k; i++) year(n);

    printf("%d", count_living());

    return 0;
}
