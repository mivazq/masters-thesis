# Executing the setup file
exec(open("/home/mivazq/data/transactions_ecuador/3_mivazq/Masters_Thesis/setup.py").read())

# import csv file intermediate_transactions.csv
df = pd.read_csv(pathCle+"output/intermediate_transactions.csv")


# random.seed(2)

# G = nx.DiGraph()

# G.add_node(1, color='lightblue', sector='1')
# G.add_node(2, color='lightblue', sector='1')
# G.add_node(3, color='lightblue', sector='1')
# G.add_node(4, color='red', sector='2')
# G.add_node(5, color='red', sector='2')
# G.add_node(6, color='red', sector='2')
# G.add_node(7, color='red', sector='2')
# G.add_node(8, color='red', sector='2')
# G.add_node(9, color='green', sector='3')
# G.add_node(10, color='green', sector='3')
# G.add_node(11, color='green', sector='3')
# G.add_node(12, color='green', sector='3')

# # add random edges
# for i in range(15):
#     u = random.choice(range(12)) + 1
#     v = random.choice(range(12)) + 1
#     if u!=v:
#         G.add_edge(u, v)

# pos = nx.circular_layout(G)
# colors = [color[1] for color in G.nodes(data="color")]
# nx.draw(G, pos=pos, with_labels=True, node_color=colors)
# # arrows point in the direction of sale

# # UPSTREAM-RELEVANT (>50%) SECTORS FOR:
#   # SECTOR BLUE: none
#   # SECTOR RED: GREEN
#   # SECTOR GREEN: RED

# # OUTDEGREE of GREEN to RED
#   #  9 : 2/5
#   # 10 : 0/5
#   # 11 : 1/5
#   # 12 : 0/5

# # OUTDEGREE of RED to GREEN
#   # 4 : 1/4
#   # 5 : 1/4
#   # 6 : 0/4
#   # 7 : 2/4
#   # 8 : 0/4


# print(nx.out_degree_centrality(G))