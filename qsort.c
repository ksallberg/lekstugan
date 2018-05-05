#include <stdio.h>

void quick_sort(int[], int, int);
int partition(int[], int, int);
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

/* Swaps using quick sort: 29
 * Swaps using bubble sort: 163
 */
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

  quick_sort(a, 0, 22);
  /* bubble_sort(a); */

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

// ---- bubble sort:

void bubble_sort(int a[]) {
  extern int swap_counter;
  int sorted = 0;
  int i, j, t;
  j = 0;
  while(sorted == 0) {
    for(i=1; i < len; i++) {
      if(a[i] < a[j]) {
        t = a[j];
        swap_counter ++;
        a[j] = a[i];
        a[i] = t;
      }
      j++;
    }
    j = 0;
    sorted = is_sorted(a);
  }
}

// ---- quick sort: (https://www.geeksforgeeks.org/quick-sort/)

void swap(int* a, int *b) {
  swap_counter ++;
  int t = *a;
  *a = *b;
  *b = t;
}

void quick_sort(int a[], int low, int high) {
  int pivot_index;
  if(low < high) {
    pivot_index = partition(a, low, high);
    // pivot_index är nu i mitten och varje sida är sorterad
    // efter den
    quick_sort(a, low, pivot_index-1);
    quick_sort(a, pivot_index+1, high);
  }
}

int partition(int a[], int low, int high) {
  int pivot = a[high];
  int i = (low-1);

  for(int j = low; j <= high-1; j++) {
    if(a[j] <= pivot) {
      i++;
      swap(&a[i], &a[j]);
    }
  }
  swap(&a[i+1], &a[high]);
  return i+1;
}



// has any side reached the pivot element?
