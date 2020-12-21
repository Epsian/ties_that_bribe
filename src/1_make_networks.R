# to make networsk for time 1 and time 2 from raw data and add metrics

# setup ####

## packages ####

library(statnet)
library(keyplayer)
# will need igraph
# will need intergraph

## in/out ####

# in
## edgelist loc
.e_loc = "./data/raw/criminaledges.csv"
## t1 attributes loc
.t1e_loc = "./data/raw/lcT1 attribs.csv"
## t2 attributes loc
.t2e_loc = "./data/raw/lcT2 attribs.csv"

# out
## t1 network
.t1_net_out = "./data/t1.l.g.rda"
.t1_data_out = "./data/t1_data.csv"
## t2 network
.t2_net_out = "./data/t2.l.g.rda"
.t2_data_out = "./data/t2_data.csv"

## load data ####

# load edgelist
e = read.csv(.e_loc, header=TRUE, stringsAsFactors = FALSE)
# load attributes
t1 = read.csv(.t1e_loc, header=TRUE, stringsAsFactors = FALSE)[, c('Female', 'Full.Name', 'Criminal', 'Law.Enforcement', 'Politician')]
t2 = read.csv(.t2e_loc, header=TRUE, stringsAsFactors = FALSE)[, c('Female', 'Full.Name', 'Criminal', 'Law.Enforcement', 'Politician')]

# create networks ####

# split edgelist
e1 <- subset(e, time == 1)
e2 <- subset(e, time == 2)

# make networks
g1 <- network(e1, directed=FALSE, matrix.type="edgelist")
g2 <- network(e2, directed=FALSE, matrix.type="edgelist")

# Get the largest Component
t1.l.g <- g1
t1.l.g%v%"lc" <- component.largest(t1.l.g)
t1.l.g <- delete.vertices(t1.l.g, which(t1.l.g%v%"lc"==FALSE))

t2.l.g <- g2
t2.l.g%v%"lc" <- component.largest(t2.l.g)
t2.l.g <- delete.vertices(t2.l.g, which(t2.l.g%v%"lc"==FALSE))

# Check if Atributes and Edges Match, make network

table(t1.l.g%v%"vertex.names"==t1$Full.Name)
t1 <- t1[match(t1.l.g%v%"vertex.names", t1$Full.Name),]
table(t1.l.g%v%"vertex.names" == t1$Full.Name)

t1.l.g <- network(t1.l.g, directed=FALSE, matrix.type="edgelist", vertex.attr=t1)

table(t2.l.g%v%"vertex.names"==t2$Full.Name)
t2 <- t2[match(t2.l.g%v%"vertex.names", t2$Full.Name),]
table(t2.l.g%v%"vertex.names" == t2$Full.Name)

t2.l.g <- network(t2.l.g, directed=FALSE, matrix.type="edgelist", vertex.attr=t2)

# Cleanup
rm(e, e1, e2, t1, t2, g1, g2)

# metrics ####

# degree
t1.l.g%v%"degree" <- degree(t1.l.g, gmode="graph")
t2.l.g%v%"degree" <- degree(t2.l.g, gmode="graph")

# kcore
t1.l.g%v%"kcore" <- kcores(t1.l.g, mode="graph", diag= FALSE)
t2.l.g%v%"kcore" <- kcores(t2.l.g, mode="graph", diag= FALSE)

# geodesic distance
t1.l.g%v%"mean.distance" <- (colSums(geodist(t1.l.g)$gdist))/(nrow(as.sociomatrix(t1.l.g)) - 1)
t2.l.g%v%"mean.distance" <- (colSums(geodist(t2.l.g)$gdist))/(nrow(as.sociomatrix(t2.l.g)) - 1)

# closeness
t1.l.g%v%"closeness" = closeness(t1.l.g, gmode = "graph")
t2.l.g%v%"closeness" = closeness(t2.l.g, gmode = "graph")

# fragmentation
t1.l.g%v%"fragment" = as.numeric(fragment(as.matrix.network.adjacency(t1.l.g)))
t2.l.g%v%"fragment" = as.numeric(fragment(as.matrix.network.adjacency(t2.l.g)))

# eigenvector centrality
t1.l.g%v%"evc" = evcent(t1.l.g, gmode = "graph", rescale = FALSE)
t2.l.g%v%"evc" = evcent(t2.l.g, gmode = "graph", rescale = FALSE)

# max cohesion
t1.l.g%v%"max_cohesion" = igraph::max_cohesion(igraph::cohesive_blocks(intergraph::asIgraph(t1.l.g)))
t2.l.g%v%"max_cohesion" = igraph::max_cohesion(igraph::cohesive_blocks(intergraph::asIgraph(t2.l.g)))

# nestedness
t1.l.g%v%"nestedness" = as.integer(unname(table(unlist(igraph::blocks((igraph::cohesive_blocks(intergraph::asIgraph(t1.l.g))))))))
t2.l.g%v%"nestedness" = as.integer(unname(table(unlist(igraph::blocks((igraph::cohesive_blocks(intergraph::asIgraph(t2.l.g))))))))

# raw betweenness
t1.l.g%v%"bet" <- betweenness(t1.l.g, gmode="graph", cmode="undirected")
t2.l.g%v%"bet" <- betweenness(t2.l.g, gmode="graph", cmode="undirected")

# normalized betweenness
t1.l.g%v%"n_bet" = (t1.l.g%v%"bet"/((network.size(t1.l.g)-1)*(network.size(t1.l.g)-2)/2))
t2.l.g%v%"n_bet" = (t2.l.g%v%"bet"/((network.size(t2.l.g)-1)*(network.size(t2.l.g)-2)/2))

# add LEP varaible ####

t1.l.g%v%"lep" = t1.l.g%v%"Law.Enforcement" | t1.l.g%v%"Politician"
t2.l.g%v%"lep" = t2.l.g%v%"Law.Enforcement" | t2.l.g%v%"Politician"

# make dataframes ####

# time 1
t1.data <- data.frame(
  t1.l.g%v%"vertex.names",
  t1.l.g%v%"Law.Enforcement",
  t1.l.g%v%"Politician",
  t1.l.g%v%"lep",
  t1.l.g%v%"degree",
  t1.l.g%v%"kcore",
  t1.l.g%v%"mean.distance",
  t1.l.g%v%"closeness",
  t1.l.g%v%"fragment",
  t1.l.g%v%"evc",
  t1.l.g%v%"max_cohesion",
  t1.l.g%v%"nestedness",
  t1.l.g%v%"bet",
  t1.l.g%v%"n_bet")

t1.data$ID <- 1:nrow(t1.data)
t1.l.g%v%"ID" <- t1.data$ID

# time 2
t2.data <- data.frame(
  t2.l.g%v%"vertex.names",
  t2.l.g%v%"Law.Enforcement",
  t2.l.g%v%"Politician",
  t2.l.g%v%"lep",
  t2.l.g%v%"degree",
  t2.l.g%v%"kcore",
  t2.l.g%v%"mean.distance",
  t2.l.g%v%"closeness",
  t2.l.g%v%"fragment",
  t2.l.g%v%"evc",
  t2.l.g%v%"max_cohesion",
  t2.l.g%v%"nestedness",
  t2.l.g%v%"bet",
  t2.l.g%v%"n_bet")

t2.data$ID <- 1:nrow(t2.data)
t2.l.g%v%"ID" <- t2.data$ID

# rename columns
colnames(t1.data) = c("vertex_name", "law_enforcement", "politician", "lep", "degree", "kcore", "mean_distance", "closeness", "fragment", "evc", "max_cohesion", "nestedness", "bet", "n_bet", "ID")
colnames(t2.data) = c("vertex_name", "law_enforcement", "politician", "lep", "degree", "kcore", "mean_distance", "closeness", "fragment", "evc", "max_cohesion", "nestedness", "bet", "n_bet", "ID")

# re-order columns
t1.data = t1.data[, c("ID", "vertex_name", "law_enforcement", "politician", "lep", "degree", "kcore", "mean_distance", "closeness", "fragment", "evc", "max_cohesion", "nestedness", "bet", "n_bet")]
t2.data = t2.data[, c("ID", "vertex_name", "law_enforcement", "politician", "lep", "degree", "kcore", "mean_distance", "closeness", "fragment", "evc", "max_cohesion", "nestedness", "bet", "n_bet")]

# save out ####
saveRDS(t1.l.g, file = .t1_net_out)
saveRDS(t2.l.g, file = .t2_net_out)

write.csv(t1.data, .t1_data_out, row.names = FALSE)
write.csv(t2.data, .t2_data_out, row.names = FALSE)

