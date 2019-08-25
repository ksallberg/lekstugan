#ifndef _LIST_INCLUDED
#define _LIST_INCLUDED

typedef struct list List;
typedef void *Item;

struct l_element {
  struct l_element *next;
  struct l_element *prev;
  void *value;
};

struct list {
  struct l_element *head;
  struct l_element *tail;
  int size;
};

/*
 * create a List that holds Items
 * returns NULL if the create call failed (malloc failure)
 */
List *l_create(void);
/*
 * remove one, add one
 */
int l_shift(List *q, Item i);
/*
 * add Item to the List
 * return 1/0 if successful/not-successful
 */
int l_add(List *q, Item i);
/*
 * remove next Item from list; returns NULL if list is empty
 */
Item l_remove(List *q);

Item l_get_last(List *q);

int l_size(List *q);

#endif
