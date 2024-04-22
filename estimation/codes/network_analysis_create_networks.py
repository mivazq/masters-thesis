# Executing the setup file
exec(open("/home/mivazq/data/transactions_ecuador/3_mivazq/Masters_Thesis/setup.py").read())

# load data
df_transactions = pd.read_csv(pathCle+"output/intermediate_transactions.csv")
df_firm_info = pd.read_csv(pathCle+"output/firm_info.csv", 
                           usecols=["id_sri", "soe", "province", "region_geo", "isic_section", "isic_division"])
df_tax_filings = pd.read_csv(pathCle+"output/tax_filings.csv")

# retrieve IDs of tax filings firms (set used for markup estimation)
est_IDs = df_tax_filings['id_sri'].unique()
est_IDs.sort()

# create dictionary to contain the node attributes for all four years (2008-2011)
node_attr_all_years = {}

# merge firm info into transactions to retrieve sector
for year in range(2008, 2012):
    # subset data for year
    df_year = df_transactions[df_transactions['year'] == year]
    # extract unique IDs and sort them
    IDs = np.concatenate((df_year['id_seller'].unique(), df_year['id_buyer'].unique()), axis=0)
    IDs = np.unique(IDs)
    IDs.sort()
    # create pandas dataframe with IDs as column
    node_attr_all_years[year] = pd.DataFrame(IDs, columns=['id_sri'])
    # add attribute of whether the firm ID ever appears in the estimation set
    node_attr_all_years[year]['est'] = np.where(node_attr_all_years[year]['id_sri'].isin(est_IDs), 1, 0)
    # merge other firm info
    node_attr_all_years[year] = pd.merge(node_attr_all_years[year], df_firm_info, on='id_sri', how='left')

# create dictionary to contain the graphs for all four years (2008-2011)
G_all_years = {}

# create a graph for each year
for year in range(2008, 2012):
    print("Creating graph for year", year)
    # subset data
    df_year = df_transactions[df_transactions['year'] == year]
    # create graph
    G = nx.DiGraph()
    # iterate over all transactions in the year
    for i in tqdm(range(len(df_year))):
        # create nodes for each buyer and seller
        u = df_year.iloc[i]['id_seller']
        v = df_year.iloc[i]['id_buyer']
        # create edges between buyer and seller
        G.add_edge(u, v, value=df_year.iloc[i]['transaction_value'], volume=df_year.iloc[i]['transaction_volume'])
    # assign graph to right year in dictionary
    G_all_years[year] = G

# iterate over nodes adding attributes
for year in range(2008, 2012):
    print("Adding attributes to nodes for year", year)
    G = G_all_years[year]
    for node in tqdm(G.nodes()):
        i = node_attr_all_years[year][node_attr_all_years[year]['id_sri'] == node].index[0]
        G.nodes[node]['est']      = node_attr_all_years[year].iloc[i]['est']
        G.nodes[node]['soe']      = node_attr_all_years[year].iloc[i]['soe']
        G.nodes[node]['province'] = node_attr_all_years[year].iloc[i]['province']
        G.nodes[node]['region']   = node_attr_all_years[year].iloc[i]['region_geo']
        G.nodes[node]['section']  = node_attr_all_years[year].iloc[i]['isic_section']
        G.nodes[node]['division'] = node_attr_all_years[year].iloc[i]['isic_division']

# output graphs to gexf files for faster read later
for year in range(2008, 2012):
    print("Storing gexf file for year", year)
    nx.write_gexf(G_all_years[year], pathEst+"input/domestic_network_"+str(year)+".gexf")

# create new graphs where only firms that ever appear in the estimation set are included
G_est_all_years = {}

# create a graph for each year
for year in range(2008, 2012):
    print("Creating graph for year", year)
    G = G_all_years[year].copy()
    remove_nodes_list = [node for node, data in G.nodes(data=True) if data['est'] == 0]
    G.remove_nodes_from(remove_nodes_list)
    G_est_all_years[year] = G

# output graphs to gexf files for faster read later
for year in range(2008, 2012):
    print("Storing gexf file for year", year)
    nx.write_gexf(G_est_all_years[year], pathEst+"input/domestic_est_network_"+str(year)+".gexf")

