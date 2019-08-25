#include "list.h"
#include <stdlib.h>
#include <string.h>

/*
 * Create a List that holds Items.
 * returns NULL if the create call failed (malloc failure)
 */
List *l_create(void) {
  struct list *l;

  if ((l = (struct list *) malloc(sizeof(struct list))) != NULL) {
    l->head = NULL;
    l->tail = NULL;
    l->size = 0;
  }
  return l;
}

/*
 * In order to keep only a certain amount of items in the
 * queue at any one time, remove one before adding if needed.
 */
int l_shift(List *l, Item i) {
  if(l_size(l) >= 40) {
    l_remove(l);
  }
  return l_add(l, i);
}

/*
 * Add Item to the List; 3rd arg is priority in MIN_PRIO..MAX_PRIO;
 * return 1/0 if successful/not-successful
 */
int l_add(List *l, Item i) {
  struct l_element *p;
  int old_size = l->size;

  p = (struct l_element *) malloc(sizeof(struct l_element));
  if (p != NULL) {
    p->value = i;
    p->next = NULL;
    // If the queue is empty, then don't assign a prev
    // for the first element
    if (l->head == NULL) {
      l->head = p;
    } else {
      l->tail->next = p;
    }

    if(l->tail == NULL) {
      p->prev = NULL;
    } else {
      p->prev = l->tail;
    }
    l->tail = p;
    l->size = old_size + 1;
    return 1;
  }
  return 0;
}

/*
 * remove next Item from queue; returns NULL if queue is empty
 */
Item l_remove(List *l) {
  struct l_element *p;
  int old_size = l->size;
  Item i;

  if (l->head == NULL) {
    l->tail = NULL;
    return NULL;
  }
  p = l->head;
  l->head = p->next;
  if(l->head != NULL) {
    l->head->prev = NULL; /* first should not have prev */
  }
  l->size = old_size-1;
  i = p->value;
  free(p);
  return i;
}

Item l_get_last(List *l) {
  struct l_element *p;
  Item i;

  if(l->tail == NULL) {
    return NULL;
  }
  p = l->tail;
  i = p->value;
  return i;
}

/* Optimization: Keep the size of the list as an int instead
 * of iterating over it to find out the size
 */
int l_size(List *l) {
  return l->size;
}
