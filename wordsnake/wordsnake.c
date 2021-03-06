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
  List *edges_in;
  List *edges_out;
};

void remove_arr(int look_for, int *chain) {
  int *rem = chain;
  int *replace;
  while(*rem != look_for) {
    rem++;
  }
  // now standing at the thing to remove
  replace = rem+1;
  while(*replace != -1) {
    *rem = *replace;
    rem++;
    replace++;
  }
  *rem = -1;
}

void reset_max(int *max_chain, int *new_max, int newlen, int *cur_max_len) {
  if(newlen > *cur_max_len) {
    *cur_max_len = newlen;
    for(int i = 0; i < newlen; i++) {
      if(new_max[i] == -1) {
        max_chain[i] = -1;
        break;
      }
      max_chain[i] = new_max[i];
    }
  }
}

void dfs(int start_node, struct Node *nodes,
         int *max_chain, int *cur_chain, int *stack,
         int *cur_max_len) {
  int node;
  int stackpt = 0;
  int cur_chain_last = 0;
  int sz = sizeof(-1) * BUFSIZE;
  memset(cur_chain, -1, sz);

  // stack
  stack[stackpt] = start_node;
  stackpt ++;

  while(stackpt > 0) {
    node = stack[stackpt-1];
    if(in_cur(node, cur_chain, cur_chain_last, cur_max_len)==1) {
      if(cur_chain[cur_chain_last-1] == node) {
        // pop stack
        stackpt--;
        // remove node from chain
        cur_chain[cur_chain_last] = -1;
        cur_chain_last --;
      } else {
        // pop stack
        stackpt--;
      }
    } else {
      cur_chain[cur_chain_last] = node;
      cur_chain_last ++;
      int edges_len = l_size(nodes[node].edges_out);
      struct l_element *cur = nodes[node].edges_out->head;
      if(edges_len>0) {
        while(cur != NULL) {
          stack[stackpt] = cur->value;
          cur=cur->next;
          stackpt++;
        }
      } else {
        reset_max(max_chain, cur_chain, cur_chain_last, cur_max_len);
        //pop stack
        stackpt--;
        if(in_cur(node, cur_chain, cur_chain_last, cur_max_len)==1) {
          remove_arr(node, cur_chain);
          cur_chain_last --;
        }
      }
    }
  }
}

int in_cur(int look_for, int *chain,
           int tmp_chain_len, int *cur_max_len) {
  int loop_to = -1;
  if(tmp_chain_len > *cur_max_len) {
    loop_to = tmp_chain_len;
  } else {
    loop_to = *cur_max_len;
  }
  for(int i=0; i < loop_to; i++) {
    if(chain[i] == look_for) {
      return 1;
    }
  }
  return 0;
}

int main() {
  int stack[BUFSIZE];
  int tmp_chain[BUFSIZE];
  int max_chain[BUFSIZE];
  struct Node nodes[BUFSIZE];
  char first_char;
  char last_char;
  int i, j;
  char comp;
  int cur_max_len = 0;

  // build graph
  for(i = 0; i < BUFSIZE; i++) {
    /* edges_out_counter=0; */
    nodes[i].id = i;
    nodes[i].edges_in = l_create();
    nodes[i].edges_out = l_create();

    first_char = pokemons[i][0];
    last_char = pokemons[i][strlen(pokemons[i])-1];

    for(j=0; j < BUFSIZE; j++) {
      comp = pokemons[j][strlen(pokemons[j])-1];

      // add nodes pointing at me
      if(first_char == comp && i != j) {
        l_add(nodes[i].edges_in, j);
      }

      // add nodes im pointing at
      if(last_char == pokemons[j][0] && i != j) {
        l_add(nodes[i].edges_out, j);
      }
    }
    nodes[i].id = i;
  }

  // populate start_nodes
  for(i = 0; i < BUFSIZE; i++) {
    if(l_size(nodes[i].edges_in) == 0) {
      dfs(i, nodes, max_chain, tmp_chain, stack, &cur_max_len);
    }
  }

  for(i = 0; i < BUFSIZE; i ++) {
    while(l_size(nodes[i].edges_in) > 0) {
      printf("remove node %d edge in\n", i),
      l_remove(nodes[i].edges_in);
    }

    while(l_size(nodes[i].edges_out) > 0) {
      printf("remove node %d edge out\n", i),
      l_remove(nodes[i].edges_out);
    }

    free(nodes[i].edges_in);
    free(nodes[i].edges_out);
  }

  for(i=0; i < cur_max_len; i++) {
    if(max_chain[i] == -1) {
      break;
    }
    printf("%s\n", pokemons[max_chain[i]]);
  }

  exit(0);
}
