# Algoritm from zhengyang zhao: https://github.com/zhezha/Pokemons

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
    while len(stack) > 0:
        node = stack[0]
        if node in cur_chain:
            if cur_chain[-1] == node:
                stack.pop(0)
                cur_chain.remove(node)
            else:
                stack.pop(0)
        else:
            cur_chain.append(node)
            if nodes[node].edges_out != []:
                for out_node in nodes[node].edges_out:
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

def id_for(name):
    for node in nodes:
        if node.word == name:
            return node.id

class Node:
    id = 0
    word = ""
    edges_in = []
    edges_out = []

nodes = []
gid = 0

for pokemon in pokemons:
    pkmn = Node()
    pkmn.id = gid
    pkmn.word = pokemon
    nodes.append(pkmn)
    gid = gid+1

gid = 0
for pokemon in pokemons:
    first_char = pokemon[0]
    last_char = pokemon[-1]
    # get all nodes pointing at me
    nodes[gid].edges_in = [id_for(x) for x in pokemons
                           if x[-1] == first_char and x != pokemon]
    nodes[gid].edges_out = [id_for(x) for x in pokemons
                            if x[0] == last_char and x != pokemon]
    gid = gid+1

start_nodes = [x.id for x in nodes if x.edges_in == []]
# start_nodes = [id_for('machamp')]

for start_node in start_nodes:
    dfs(start_node)

for x in max_chain:
    print nodes[x].word
