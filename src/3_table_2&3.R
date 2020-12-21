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

# eigenvector centralization
centralization(t1.l.g, evcent, mode="graph", diag=FALSE)

# closeness centralization
centralization(t1.l.g, closeness, mode="graph", diag=FALSE)

# betweenness centralization
centralization(t1.l.g, betweenness, mode="graph", diag=FALSE)


#??????????????????????????????????????????????????????????????????????????

# max cohesion centralization
sum(max(t1.l.g%v%"max_cohesion")-t1.l.g%v%"max_cohesion")/(length(t1.l.g%v%"max_cohesion")-1) 

# nestedness centralization?
sum(max(t1.l.g%v%"nestedness")-t1.l.g%v%"nestedness")/(length(t1.l.g%v%"nestedness")-1) 

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



#????????????????????????????????????????????????

sum(max(t2.l.g%v%"degree")-t2.l.g%v%"degree")/(length(t2.l.g%v%"degree")-1) / ((length(t2.l.g%v%"degree") - 1) * (length(t2.l.g%v%"degree") - 2))





# max cohesion centralization
sum(max(t2.l.g%v%"max_cohesion")-t2.l.g%v%"max_cohesion")/(length(t2.l.g%v%"max_cohesion")-1) 

# nestedness centralization?
sum(max(t2.l.g%v%"nestedness")-t2.l.g%v%"nestedness")/(length(t2.l.g%v%"nestedness")-1) 
