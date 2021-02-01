# To get the network statistics for table 2 and 3

# setup ####

# packages
library(statnet)

# data load 
t1.l.g = readRDS("./data/t1.l.g.rda")
t2.l.g = readRDS("./data/t2.l.g.rda")

# Table 2 ####

# size (node count) + Criminal Edges
t1.l.g

# diameter
max(geodist(t1.l.g)$gdist)

# mean geodesic Distance
mean((colSums(geodist(t1.l.g)$gdist))/(nrow(as.sociomatrix(t1.l.g)) - 1))

# density
gden(t1.l.g, mode = "graph")

# degree centralization
centralization(t1.l.g, degree, mode="graph", diag=FALSE)
sum(max(t1.l.g%v%"degree")-t1.l.g%v%"degree")/((length(t1.l.g%v%"degree")-1) * (length(t1.l.g%v%"degree")-2))

# eigenvector centralization (and figure out how to make my own centralization measure)
centralization(t1.l.g, evcent, mode="graph", diag=FALSE, normalize = TRUE)
sum(max(t1.l.g%v%"evc")-t1.l.g%v%"evc")/evcent(t1.l.g, gmode = "graph", tmaxdev = TRUE)
sum(max(t1.l.g%v%"evc")-t1.l.g%v%"evc")/(sqrt(2)/2 * (length(t1.l.g%v%"vertex.names") - 2))

# closeness centralization
centralization(t1.l.g, closeness, mode="graph", diag=FALSE)
sum(max(t1.l.g%v%"closeness")-t1.l.g%v%"closeness")/closeness(t1.l.g, gmode = "graph", tmaxdev = TRUE)
sum(max(t1.l.g%v%"closeness")-t1.l.g%v%"closeness")/((length(t1.l.g%v%"vertex.names") - 2) * (length(t1.l.g%v%"vertex.names") - 1)/(2 * length(t1.l.g%v%"vertex.names") - 3))

# total nestedness count
t1_blocks = length(igraph::blocks(igraph::cohesive_blocks(intergraph::asIgraph(t1.l.g))))

# Table 3 ####

# size (node count) + Criminal Edges
t2.l.g

# diameter
max(geodist(t2.l.g)$gdist)

# mean geodesic Distance
mean((colSums(geodist(t2.l.g)$gdist))/(nrow(as.sociomatrix(t2.l.g)) - 1))

# density
gden(t2.l.g, mode = "graph")

# degree centralization
centralization(t2.l.g, degree, mode="graph", diag=FALSE)

# eigenvector centralization
centralization(t2.l.g, evcent, mode="graph", diag=FALSE)

# closeness centralization
centralization(t2.l.g, closeness, mode="graph", diag=FALSE)

# betweenness centralization
centralization(t2.l.g, betweenness, mode="graph", diag=FALSE)

# total nestedness count
t2_blocks = length(igraph::blocks(igraph::cohesive_blocks(intergraph::asIgraph(t2.l.g))))

