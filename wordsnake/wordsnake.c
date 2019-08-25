/* Algoritm from zhengyang zhao: https://github.com/zhezha/Pokemons */

#include <stdio.h>

#include "list.h"

const char *pokemons[] = {
  "audino","bagon","baltoy","banette","bidoof","braviary","bronzor",
  "carracosta","charmeleon","cresselia","croagunk","darmanitan",
  "deino","emboar","emolga","exeggcute","gabite","girafarig","gulpin",
  "haxorus","heatmor","heatran","ivysaur","jellicent","jumpluff",
  "kangaskhan","kricketune","landorus","ledyba","loudred","lumineon",
  "lunatone","machamp","magnezone","mamoswine","nosepp","petilil",
  "pidgeotto","pikachu","pinsir","poliwrath","poochyena","porygon2",
  "porygonz","registeel","relicanth","remoraid","rufflet","sableye",
  "scolipede","scrafty","seaking","sealeo","silcoon","simisear",
  "snivy","snorlax","spoink","starly","tirtouga","trapinch","treecko",
  "tyrogue","vigoroth","vulpix","wailord","wartortle","whismur",
  "wingull","yamask"};

#define BUFSIZE 70

struct Node {
  int id;
  list_t *edges_in;
  list_t *edges_out;
};

void reset_max(list_t *max_chain, list_t *new_max) {
  /* printf("old %lu new %lu\n", max_chain->len, new_max->len); */
  list_node_t *x;
  if(max_chain->len < new_max->len) {

    while(max_chain->len > 0) {
      list_lpop(max_chain);
    }
    /* list_destroy(max_chain); */
    /* max_chain = list_new(); */

    x = new_max->head;
    while(x != NULL) {
      list_rpush(max_chain, list_node_new(x->val));
      x = x->next;
    }
    /* max_chain->head = new_max->head; */
  }
}

void dfs(int start_node, struct Node nodes[BUFSIZE], list_t *max_chain) {
  int stack[BUFSIZE];
  int node;
  list_t *cur_chain;
  int stackpt = 0;

  cur_chain = list_new();

  // stack
  stack[stackpt] = start_node;
  stackpt ++;

  while(stackpt > 0) {
    node = stack[stackpt-1];
    if(in_cur(node, cur_chain)==1) {
      if(last_pos(cur_chain) == node) {
        // pop stack
        stackpt--;
        // remove node from chain
        list_rpop(cur_chain);
      } else {
        // pop stack
        stackpt--;
      }
    } else {
      list_node_t *new_node = list_node_new(node);
      list_rpush(cur_chain, new_node);
      list_t *temp = nodes[node].edges_out;
      int edges_len = temp->len;
      list_node_t *cur = nodes[node].edges_out->head;
      if(edges_len>0) {
        while(cur != NULL) {
          stack[stackpt] = cur->val;
          cur=cur->next;
          stackpt++;
        }
      } else {
        reset_max(max_chain, cur_chain);
        //pop stack
        stackpt--;
        if (in_cur(node, cur_chain)==1) {
          list_node_t *result = list_find(cur_chain, node);
          list_remove(cur_chain, result);
        }
      }
    }
  }
}

int last_pos(list_t *chain) {
  list_node_t *result = list_at(chain, (chain->len)-1);
  return result->val;
}

int in_cur(int look_for, list_t *chain) {
  list_node_t *result = list_find(chain, look_for);
  if(result == NULL)
    return 0;
  if(result->val == look_for) {
    return 1;
  } else {
    return 0;
  }
}

int main() {
  list_t *max_chain;
  struct Node nodes[BUFSIZE];
  char first_char;
  char last_char;
  int i, j;
  char comp;

  max_chain = list_new();

  // build graph
  for(i = 0; i < BUFSIZE; i++) {
    /* edges_out_counter=0; */
    nodes[i].id = i;
    nodes[i].edges_in = list_new();
    nodes[i].edges_out = list_new();

    first_char = pokemons[i][0];
    last_char = pokemons[i][strlen(pokemons[i])-1];

    for(j=0; j < BUFSIZE; j++) {
      comp = pokemons[j][strlen(pokemons[j])-1];

      // add nodes pointing at me
      if(first_char == comp && i != j) {
        list_rpush(nodes[i].edges_in, list_node_new(j));
      }

      // add nodes im pointing at
      if(last_char == pokemons[j][0] && i != j) {
        list_rpush(nodes[i].edges_out, list_node_new(j));
      }
    }
    nodes[i].id = i;
  }

  // populate start_nodes
  for(i = 0; i < BUFSIZE; i++) {
    if(nodes[i].edges_in->len == 0) {
      dfs(i, nodes, max_chain);
    }
  }

  // clean up
  for(i = 0; i < BUFSIZE; i++) {
    list_destroy(nodes[i].edges_in);
    list_destroy(nodes[i].edges_out);
  }

  list_node_t *it = max_chain->head;
  if(it == NULL) printf("shit...\n");
  while(it != NULL) {

    printf("%s\n", pokemons[(int) it->val]);
        it = it->next;
  }

  exit(0);
}
