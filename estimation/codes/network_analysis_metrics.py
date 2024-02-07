# Executing the setup file
exec(open("/home/mivazq/data/transactions_ecuador/3_mivazq/Masters_Thesis/setup.py").read())

# load data
for year in range(2008, 2012):
    print("Storing gexf file for year", year)
    nx.read_gexf(G_all_years[year], pathEst+"input/domestic_network_"+str(year)+".gexf")

