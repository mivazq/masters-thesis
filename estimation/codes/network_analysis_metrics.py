# Executing the setup file
exec(open("/home/mivazq/data/transactions_ecuador/3_mivazq/Masters_Thesis/setup.py").read())

df_tax_filings = pd.read_csv(pathCle+"output/tax_filings.csv")
df_firm_info = pd.read_csv(pathCle+"output/firm_info.csv", 
                           usecols=["id_sri", "soe", "province", "region_geo", "isic_section", "isic_division"])
panel = pd.merge(df_tax_filings, df_firm_info, on='id_sri', how='left')

# load data
G_all_years = {} 
for year in range(2008, 2009):
#for year in range(2008, 2012):
    print("Loading gexf file for year", year)
    G_all_years[year] = nx.read_gexf(pathEst+"input/domestic_network_"+str(year)+".gexf")

# iterate over nodes adding attributes
for year in range(2008, 2009):
#for year in range(2008, 2012):
    print("Adding attributes to nodes for year", year)
    G = G_all_years[year]
    for node in tqdm(G.nodes()):
        G.nodes[node]['est'] = 0 if (G.nodes[node]['section'] 
                                     not in ("A", "B", "D", "F", "G") 
                                     and G.nodes[node]['est'] == 1) else G.nodes[node]['est']

# compute network metrics
metrics = panel[panel['isic_section'].isin(["A", "B", "D", "F", "G"])]
metrics = metrics[~metrics['isic_division'].isin(["D16", "D30"])]
metrics = metrics[['id_sri', 'year', 'isic_division']]
metrics = metrics[metrics['year']==2008]

# store nx.in_degree_centrality as column in metrics
G = G_all_years[2008]
indeg_cent = pd.Series(nx.in_degree_centrality(G))
nx.set_node_attributes(G, nx.in_degree_centrality(G), 'in_degree_centrality')
nx.set_node_attributes(G, nx.out_degree_centrality(G), 'out_degree_centrality')

# extract 'in_degree_centrality' and 'out_degree_centrality' from G and store it in metrics
cent = pd.DataFrame()
cent['in_degree_centrality'] = pd.DataFrame.from_dict(nx.get_node_attributes(G, 'in_degree_centrality'), orient='index')
cent['out_degree_centrality'] = pd.DataFrame.from_dict(nx.get_node_attributes(G, 'out_degree_centrality'), orient='index')
cent['id_sri'] = pd.to_numeric(cent.index)
cent.astype({'id_sri': 'int64'}).dtypes

# merge cent with metrics
metrics = pd.merge(metrics, cent, on='id_sri', how='left')
# compare type of metrics['id_sri'] and cent.index
type(metrics['id_sri'])
type(cent.index)

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

