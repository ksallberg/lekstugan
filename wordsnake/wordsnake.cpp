/* Algoritm from zhengyang zhao: https://github.com/zhezha/Pokemons */

#include <stdio.h>
#include <fstream>
#include <exception>
#include <memory>
#include <stack>
#include <list>

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

std::list<int> max_chain;

struct Node {
  int id;
  char word[20];
  int edges_in[20];
  int edges_in_cnt;
  int edges_out[20];
  int edges_out_cnt;
};

void dfs(int start_node,
         struct Node nodes[BUFSIZE]) {
  std::stack<int> stack;
  int node;
  std::list<int> cur_chain;

  // stack
  stack.push(start_node);

  while(!stack.empty()) {
    node = stack.top();
    bool found = (std::find(cur_chain.begin(), cur_chain.end(), node)
                  != cur_chain.end());
    if(found) {
      if(cur_chain.back() == node) {
        stack.pop();
        // remove node from chain
        cur_chain.remove(node);
      } else {
        stack.pop();
      }
    } else {
      cur_chain.push_back(node);
      int edges_len = nodes[node].edges_out_cnt;
      if(edges_len>0) {
        for(int i = 0; i < edges_len; i++) {
          stack.push(nodes[node].edges_out[i]);
        }
      } else {
        if(cur_chain.size() > max_chain.size()) {
          max_chain.clear();
          for(int n : cur_chain) {
            max_chain.push_back(n);
          }
        }
        stack.pop();
        cur_chain.remove(node);
      }
    }
  }
}

int main() {
  struct Node nodes[BUFSIZE];
  char first_char;
  char last_char;
  int i, j, edges_in_counter, edges_out_counter=0;
  char comp;

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
      dfs(i, nodes);
    }
  }

  for(int n : max_chain) {
    printf("%s\n", pokemons[n]);
  }

  exit(0);
}
