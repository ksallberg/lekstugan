#include <stdio.h>

void quick_sort(int[], int low, int high);

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
  int i = 0;
  len = sizeof(a) / sizeof(int);

  printf("\n\nUnsorted array is: ");
  for(i=0; i<len; ++i) {
    printf("%d ", a[i]);
  }
  printf("\n\n");

  quick_sort(a, 0, len-1);

  printf("\n\nSorted array is: \n");
  for(i=0; i<len; ++i) {
    printf("%d ", a[i]);
  }
  printf("\n\n");
}

void swap(int* a, int *b) {
  int t = *a;
  *a = *b;
  *b = t;
}

void quick_sort(int number[], int first, int last) {
  int piv = first;
  int left = first;
  int right = last;
  printf("Pivot: %d\n", piv);
  if(first > last) {
    return;
  }
  // Mål, få alla värden mindre än vad piv pekar på,
  // på vänster sida om vad piv pekar på.

  // Få alla värden större än vad piv pekar på, på
  // höger sida om piv

  // Om left är större än right, så har dom gått förbi
  // varandra
  while(left < right) {
    // flytta left så att värdet på det den pekar på
    // är större än värdet på det som piv pekar på
    while(number[left] <= number[piv] && left < last) {
      left ++;
    }
    // flytta right så att värdet på det den pekar på
    // är mindre eller lika med det som piv pekar på
    while(number[right] > number[piv]) {
      right --;
    }
    // I detta läget pekar left på något som är
    // högre än det right pekar på, samtidigt som
    // left är mindre än right, då ska vi byta position
    if(left < right) {
      swap(&number[right], &number[left]);
    }
    printf("left: %d right: %d\n", left, right);
  }
  printf("final left: %d final right: %d\n", left, right);
  // Left är nu större eller lika med right
  // Byt ut pivot-elementet med vad right pekar på
  swap(&number[right], &number[piv]);
  // Den vänstra delen, right-1 tar inte med pivot
  quick_sort(number, first, right - 1);
  // Den högra delen, right+1 tar inte med pivot
  quick_sort(number, right + 1, last);
}
