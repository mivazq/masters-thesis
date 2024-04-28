# Executing the setup file
exec(open("/home/mivazq/data/transactions_ecuador/3_mivazq/Masters_Thesis/setup.py").read())

df_tax_filings = pd.read_csv(pathCle+"output/tax_filings.csv")
df_firm_info = pd.read_csv(pathCle+"output/firm_info.csv", 
                           usecols=["id_sri", "soe", "province", "region_geo", "isic_section", "isic_division"])
panel = pd.merge(df_tax_filings, df_firm_info, on='id_sri', how='left')

# load data
G_all_years = {} 
#for year in range(2008, 2009):
for year in range(2008, 2012):
    print("Loading gexf file for year", year)
    #G_all_years[year] = nx.read_gexf(pathEst+"input/domestic_network_"+str(year)+".gexf")
    G_all_years[year] = nx.read_gexf(pathEst+"input/domestic_est_network_"+str(year)+".gexf")

# create lagged 'active' variable
panel['lag_active'] = panel.groupby('id_sri')['active'].shift(1)
panel.fillna({'lag_active':0}, inplace=True) # set nan values to 0
panel['use_1st'] = np.where(panel['active']==1, 1, 0) # Active in current period
panel['use_2nd'] = np.where((panel['active']==1) & (panel['lag_active']==1), 1, 0) # Active in current and previous period

# create table counting 'use_1sd' and 'use_2nd' by isic_division
ind_count = panel.groupby('isic_division')['use_1st'].sum().reset_index()

# get names of all isic_divisions whose count is less than 100 as a list
exclusions = list(ind_count[ind_count['use_1st']<100]['isic_division'].unique())

# compute network metrics for all observations besides those in excluded isic_divisions
metrics = panel[['id_sri', 'year', 'isic_division']]
#metrics = metrics[~metrics['isic_division'].isin(exclusions)]
metrics = metrics[metrics['year']==2008]
metrics.reset_index(drop=True, inplace=True)

# store nx.in_degree_centrality as column in metrics
G = G_all_years[2008]
test  = nx.betweenness_centrality(G, k=1000, seed=1)
test2 = nx.betweenness_centrality(G, k=1000, seed=3)
test3 = nx.betweenness_centrality(G, k=5000, seed=3)
np.corrcoef(np.array(list(test.values())), np.array(list(test2.values())))

# create network measures dataframe 
cent = pd.DataFrame()
cent['betweenness_centrality_test'] = pd.DataFrame.from_dict(test, orient='index')
cent['betweenness_centrality_test2'] = pd.DataFrame.from_dict(test2, orient='index')
cent['betweenness_centrality_test3'] = pd.DataFrame.from_dict(test3, orient='index')
cent['id_sri'] = pd.to_numeric(cent.index)
cent.astype({'id_sri': 'int64'}).dtypes

# merge cent with metrics and export
metrics = pd.merge(metrics, cent, on='id_sri', how='left')
metrics.to_csv(pathEst+"output/btwn.csv", index=False)





import graphblas_algorithms as ga
# Explicitly convert to ga.Graph
G2 = ga.Graph.from_networkx(G)
nx.in_degree_centrality(G)
ga.in_degree_centrality(G2)
nx.pagerank(G)
ga.pagerank(G2)
G2.id_to_key

# Create a dictionary to convert between the node IDs used by NetworkX and GraphBLAS
idx, vals = ga.in_degree_centrality(G2).to_coo()

# Note that idx only contains the IDs of the nodes with non-zero in-degree centrality
# We can use the idx array to look up the node IDs in the original graph
# We can use the vals array to look up the in-degree centrality values
in_degree_dict = {}
node_ids = [G2.id_to_key[i] for i in idx]
for node_id, in_degree in zip(node_ids, vals):
    in_degree_dict[node_id] = in_degree

# Create a dictionary to convert between the node IDs used by NetworkX and GraphBLAS
idx, vals = ga.pagerank(G2).to_coo()


equiv = dict(zip(G2.id_to_key.values(), G2.id_to_key.keys()))



dt_est = pd.read_csv(pathEst+"output/markups_2008_alt.csv")
dt_est.rename(columns={'id': 'id_sri'}, inplace=True)
# merge metrics with dt_est
metrics = pd.merge(metrics, dt_est, on='id_sri', how='left')
# transform network metrics to be deviations from isic_division mean
# compute mean of each isic_division
mean_division = metrics.groupby('isic_division').mean().reset_index()
mean_division = mean_division[['isic_division', 'in_degree_centrality', 'out_degree_centrality']]
# merge mean_division with metrics
metrics = pd.merge(metrics, mean_division, on='isic_division', how='left')
# create deviation variables
metrics['in_degree_centrality_dev'] = metrics['in_degree_centrality_x'] - metrics['in_degree_centrality_y']
metrics['out_degree_centrality_dev'] = metrics['out_degree_centrality_x'] - metrics['out_degree_centrality_y']

