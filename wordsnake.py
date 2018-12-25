max_chain = []
pokemons = ["audino","bagon","baltoy","banette","bidoof","braviary","bronzor",
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
            "wingull","yamask"]

def dfs(start_node):
    cur_chain = []
    stack = []
    stack.insert(0, start_node)
    while stack != []:
        node = stack[0]
        if node in cur_chain:
            if cur_chain[-1] == node:
                stack.pop(0)
                cur_chain.remove(node)
            else:
                stack.pop(0)
        else:
            cur_chain.append(node)
            if node.edges_out != []:
                for out_node in node.edges_out:
                    stack.insert(0, out_node)
            else:
                reset_max(cur_chain)
                stack.pop(0)
                cur_chain.remove(node)

def reset_max(new_chain):
    if len(new_chain) > len(max_chain):
        while len(max_chain) > 0:
            max_chain.pop()
        max_chain.extend(new_chain)

class Node:
    word = ""
    edges_in = []
    edges_out = []

nodes = {}

for pokemon in pokemons:
    pkmn = Node()
    pkmn.word = pokemon
    nodes[pokemon] = pkmn

for pokemon in pokemons:
    first_char = pokemon[0]
    last_char = pokemon[-1]
    # get all nodes pointing at me
    nodes[pokemon].edges_in = [nodes[x] for x in pokemons
                               if x[-1] == first_char and x != pokemon]
    nodes[pokemon].edges_out = [nodes[x] for x in pokemons
                                if x[0] == last_char and x != pokemon]

# start_nodes = [nodes[x] for x in nodes if nodes[x].edges_in == []]
start_nodes = [nodes['machamp']]

for start_node in start_nodes:
    print "startnode: ", start_node.word
    dfs(start_node)

for x in max_chain:
    print "    node: ", x.word
