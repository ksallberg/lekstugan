/* Algoritm from zhengyang zhao: https://github.com/zhezha/Pokemons */

import java.util.List;
import java.util.ArrayList;
import java.util.Stack;

public class wordsnake {

    static int BUFSIZE=70;

    private static class Node {
        int id;
        String word;
        int[] edges_in;
        int edges_in_cnt;
        int[] edges_out;
        int edges_out_cnt;
        public Node() {
            edges_in = new int[BUFSIZE];
            edges_out = new int[BUFSIZE];
        }
    }

    static String[] pokemons = {
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

    static Node[] nodes;

    static List<Integer> max_chain;

    static void dfs(int start_node) {
        Stack<Integer> stack = new Stack<Integer>();
        int node;
        List<Integer> cur_chain = new ArrayList<Integer>();

        // stack
        stack.push(start_node);
        while(!stack.empty()) {
            node = stack.peek();
            boolean found = cur_chain.contains(node);
            if(found) {
                if(cur_chain.size() > 0 &&
                   cur_chain.get(cur_chain.size()-1).equals(node)) {
                    stack.pop();
                    // remove node from chain
                    cur_chain.remove(new Integer(node));
                } else {
                    stack.pop();
                }
            } else {
                cur_chain.add(node);
                int edges_len = nodes[node].edges_out_cnt;
                if(edges_len>0) {
                    for(int i = 0; i < edges_len; i++) {
                        stack.push(nodes[node].edges_out[i]);
                    }
                } else {
                    if(cur_chain.size() > max_chain.size()) {
                        max_chain.clear();
                        for(int n : cur_chain) {
                            max_chain.add(n);
                        }
                    }
                    stack.pop();
                    cur_chain.remove(new Integer(node));
                }
            }
        }
    }

    public static void main(String[] args) {
        char first_char;
        char last_char;
        int i, j, edges_in_counter, edges_out_counter=0;
        char comp;

        nodes = new Node[BUFSIZE];
        max_chain = new ArrayList<Integer>();

        // build graph
        for(i = 0; i < BUFSIZE; i++) {
            edges_in_counter=0;
            edges_out_counter=0;
            nodes[i] = new Node();
            nodes[i].word = pokemons[i];
            nodes[i].id = i;
            nodes[i].edges_in_cnt = 0;
            nodes[i].edges_out_cnt = 0;
            first_char = pokemons[i].charAt(0);
            last_char = pokemons[i].charAt(pokemons[i].length()-1);

            for(j=0; j < BUFSIZE; j++) {
                comp = pokemons[j].charAt(pokemons[j].length()-1);

                // add nodes pointing at me
                if(first_char == comp && i != j) {
                    nodes[i].edges_in[edges_in_counter] = j;
                    nodes[i].edges_in_cnt ++;
                    edges_in_counter++;
                }

                // add nodes im pointing at
                if(last_char == pokemons[j].charAt(0) && i != j) {
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
                dfs(i);
            }
        }

        for(int n : max_chain) {
            System.out.println(pokemons[n]);
        }
    }
}
