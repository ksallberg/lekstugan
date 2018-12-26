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

struct Node {
  int id;
  char word[20];
  int edges_in[20];
  int edges_in_cnt;
  int edges_out[20];
};


void dfs(int start_node) {
  int stack[100];
  int *stack_pointer = stack[0];

}

int main() {
  int max_chain[70];
  int start_nodes[70];
  struct Node nodes[70];
  char first_char;
  char last_char;
  int i, j, edges_in_counter, edges_out_counter=0;
  char comp;

  // build graph
  for(i = 0; i < 70; i++) {
    edges_in_counter=0;
    edges_out_counter=0;
    strcpy(nodes[i].word, pokemons[i]);
    nodes[i].edges_in_cnt = 0;
    first_char = pokemons[i][0];
    last_char = pokemons[i][strlen(pokemons[i])-1];

    for(j=0; j < 70; j++) {
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
        edges_out_counter++;
      }
    }

    nodes[i].id = i;
  }

  j=0;
  // populate start_nodes
  for(i = 0; i < 70; i++) {
    if(nodes[i].edges_in_cnt == 0) {
      start_nodes[j] = i;
      j++;
      printf("start node: %s\n", pokemons[i]);
    }
  }
}
