#include <stdio.h>

/*

ritning:

ls :: [Int]
ls = [93, 98, 59, 29, 100, 9, 23, 45, 7, 12, 1, 99,
      -2, 0, 15, 4, 11, 9, 32, -10, -11, 95, 92]

qsort :: [Int] -> [Int]
qsort [] = []
qsort (piv:all) = qsort smal ++ [piv] ++ qsort larg
  where larg = larger piv all
        smal = smaller piv all

larger :: Int -> [Int] -> [Int]
larger pivot ls = [l | l <- ls, l > pivot]

smaller :: Int -> [Int] -> [Int]
smaller pivot ls = [l | l <- ls, l < pivot]

*/

void quick_sort(int[]);
void bubble_sort(int[]);

extern int swap_counter = 0;
extern int len = 0;

// O(n)
int is_sorted(int a[]) {
  int sorted = 1;
  int i, j;
  j = 0;
  for(i=1; i<len; i++) {
    if(a[i] < a[j]) {
      sorted = 0;
      break;
    }
    j++; // j to be one step behind i
  }
  return sorted;
}

void main()
{
  int a[] = {93, 98, 59, 29, 100, 9, 23, 45, 7, 12, 1, 99,
             -2, 0, 15, 4, 11, 9, 32, -10, -11, 95, 92};
  int i;
  int sort1, sort2;

  len = sizeof(a) / sizeof(int);

  sort1 = is_sorted(a);

  printf("\n\nUnsorted array is: ");
  for(i=0; i<len; ++i) {
    printf("%d ", a[i]);
  }
  printf("\n\n");

  quick_sort(a);

  sort2 = is_sorted(a);

  printf("\n\nSorted array is: \n");
  for(i=0; i<len; ++i) {
    printf("%d ", a[i]);
  }
  printf("\n\n");

  printf("\n before %d after %d \n", sort1, sort2);

  printf("\n swaps: %d\n", swap_counter);
  printf("\n");
}

void swap(int* a, int *b) {
  swap_counter ++;
  int t = *a;
  *a = *b;
  *b = t;
}

void quick_sort(int a[]) {
  return;
}
