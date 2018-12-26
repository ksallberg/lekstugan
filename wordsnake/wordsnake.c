/* Algoritm from zhengyang zhao: https://github.com/zhezha/Pokemons */

#include <stdio.h>

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

extern int cur_max_len = 0;

struct Node {
  int id;
  char word[20];
  int edges_in[20];
  int edges_in_cnt;
  int edges_out[20];
  int edges_out_cnt;
};

void remove_arr(int look_for, int chain[BUFSIZE]) {
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

void reset_max(int max_chain[BUFSIZE], int new_max[BUFSIZE], int newlen) {
  if(newlen > cur_max_len) {
    cur_max_len = newlen;
    for(int i = 0; i < BUFSIZE; i++) {
      if(new_max[i] == -1) {
        max_chain[i] = -1;
        break;
      }
      max_chain[i] = new_max[i];
    }
  }
}

void dfs(int start_node, struct Node nodes[BUFSIZE], int max_chain[BUFSIZE]) {
  int stack[BUFSIZE];
  int node;
  int cur_chain[BUFSIZE];
  int stackpt = 0;
  int cur_chain_last = 0;

  for(int i = 0; i < BUFSIZE; i ++) {
    cur_chain[i] = -1;
  }

  // stack
  stack[stackpt] = start_node;
  stackpt ++;

  while(stackpt > 0) {
    node = stack[stackpt-1];
    if(in_cur(node, cur_chain)==1) {
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
      int edges_len = nodes[node].edges_out_cnt;
      if(edges_len>0) {
        for(int i = 0; i < edges_len; i++) {
          stack[stackpt] = nodes[node].edges_out[i];
          stackpt++;
        }
      } else {
        reset_max(max_chain, cur_chain, cur_chain_last);
        //pop stack
        stackpt--;
        if (in_cur(node, cur_chain)==1) {
          remove_arr(node, cur_chain);
          cur_chain_last --;
        }
      }
    }
  }
}

int in_cur(int look_for, int chain[BUFSIZE]) {
  for(int i=0; i < BUFSIZE; i++) {
    if(chain[i] == look_for) {
      return 1;
    }
  }
  return 0;
}

int main() {
  int max_chain[BUFSIZE];
  struct Node nodes[BUFSIZE];
  char first_char;
  char last_char;
  int i, j, edges_in_counter, edges_out_counter=0;
  char comp;

  for(int i = 0; i < BUFSIZE; i ++) {
    max_chain[i] = -1;
  }

  // build graph
  for(i = 0; i < BUFSIZE; i++) {
    edges_in_counter=0;
    edges_out_counter=0;
    strcpy(nodes[i].word, pokemons[i]);
    nodes[i].id = i;
    nodes[i].edges_in_cnt = 0;
    nodes[i].edges_out_cnt = 0;
    first_char = pokemons[i][0];
    last_char = pokemons[i][strlen(pokemons[i])-1];

    for(j=0; j < BUFSIZE; j++) {
      comp = pokemons[j][strlen(pokemons[j])-1];

      // add nodes pointing at me
      if(first_char == comp && i != j) {
        nodes[i].edges_in[edges_in_counter] = j;
        nodes[i].edges_in_cnt ++;
        edges_in_counter++;
      }

      // add nodes im pointing at
      if(last_char == pokemons[j][0] && i != j) {
        nodes[i].edges_out[edges_out_counter] = j;
        nodes[i].edges_out_cnt ++;
        edges_out_counter++;
      }
    }
    nodes[i].id = i;
  }

  // populate start_nodes
  for(i = 0; i < BUFSIZE; i++) {
    if(nodes[i].edges_in_cnt == 0) {
      dfs(i, nodes, max_chain);
    }
  }

  for(i=0; i < BUFSIZE; i++) {
    if(max_chain[i] == -1) {
      break;
    }
    printf("%s\n", pokemons[max_chain[i]]);
  }

  exit(0);
}
