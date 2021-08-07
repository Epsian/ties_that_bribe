# make nestedness network plot

# setup ####

# load packages
library(statnet)
library(intergraph)
library(visNetwork)
library(htmlwidgets)

# add journal font
library(showtext)
font_add(family = "STIX", regular = "P:/pCloud Sync/Projects/school/QP/state_sims/STIX2Text-Regular.otf")
showtext_auto() 

# load data
t1.l.g = readRDS("./data/t1.l.g.rda")
t2.l.g = readRDS("./data/t2.l.g.rda")

t1.coords = readRDS("./vis/t1.plot.loc.rda")
t2.coords = readRDS("./vis/t2.plot.loc.rda")

# set node colors
crim.col = "#cccccc"
le.col = "#7c7c7c"
pol.col = "#000000"

# meta data for plotting ####

# add in group data
.t1_group = rep("Criminal", length(t1.l.g%v%"Full.Name"))
.t1_group[t1.l.g%v%"Law.Enforcement"] = "Law Enforcement"
.t1_group[t1.l.g%v%"Politician"] = "Politician"
t1.l.g%v%"group" = .t1_group

.t2_group = rep("Criminal", length(t2.l.g%v%"Full.Name"))
.t2_group[t2.l.g%v%"Law.Enforcement"] = "Law Enforcement"
.t2_group[t2.l.g%v%"Politician"] = "Politician"
t2.l.g%v%"group" = .t2_group

# add in size data
.t1_size = log(t1.l.g%v%"nestedness", base = 1.2)
.t1_size[.t1_size == 0] = 1
.t1_size = .t1_size * 3
t1.l.g%v%"size" = .t1_size

.t2_size = log(t2.l.g%v%"nestedness", base = 1.2)
.t2_size[.t2_size == 0] = 1
.t2_size = .t2_size * 3
t2.l.g%v%"size" = .t2_size

# add in colors
.t1_color = rep(crim.col, length(t1.l.g%v%"Full.Name"))
.t1_color[t1.l.g%v%"Law.Enforcement"] = le.col
.t1_color[t1.l.g%v%"Politician"] = pol.col
t1.l.g%v%"color" = .t1_color

.t2_color = rep(crim.col, length(t2.l.g%v%"Full.Name"))
.t2_color[t2.l.g%v%"Law.Enforcement"] = le.col
.t2_color[t2.l.g%v%"Politician"] = pol.col
t2.l.g%v%"color" = .t2_color

# blank labels
t1.l.g%v%"label" = NA
t2.l.g%v%"label" = NA

# static plots for publication ####

## time 1 ####

# set default color of edges as non-state color
t1.l.g%e%"color" = rep(crim.col, length(t1.l.g%v%"vertex.names"))

# get all edges connected to law enforcement
.le_edges = unique(unlist(sapply(as.integer(t1.l.g%v%"ID")[t1.l.g%v%"Law.Enforcement"], function(x) get.edgeIDs(t1.l.g, x))))
# set all those that are connected to law enforcement as le color
set.edge.value(t1.l.g, "color", le.col, .le_edges)

# get all edges connected to politicains
.pol_edges = unique(unlist(sapply(as.integer(t1.l.g%v%"ID")[t1.l.g%v%"Politician"], function(x) get.edgeIDs(t1.l.g, x))))
# set all those that are connected to politicians as politician color
set.edge.value(t1.l.g, "color", pol.col, .pol_edges)

### reorder all the edges ####

# get the edges and re-order
t1.edgelist = as.edgelist(t1.l.g, attrname = "color")
t1.edgelist = rbind(t1.edgelist[t1.edgelist[, 3] == crim.col, ], t1.edgelist[t1.edgelist[, 3] == le.col, ], t1.edgelist[t1.edgelist[, 3] == pol.col, ])

# get all edge IDs and delete all current edges
.all_t1_edges = unique(unlist(sapply(as.integer(t1.l.g%v%"ID"), function(x) get.edgeIDs(t1.l.g, x))))
delete.edges(t1.l.g, .all_t1_edges)

# re-add edges
add.edges(t1.l.g, tail = t1.edgelist[,1], head =  t1.edgelist[,2], names.eval = "color", vals.eval = t1.edgelist[,3])

### plot ####

# plot and save
pdf("./vis/network_plots/t1_network.pdf")
plot.network(t1.l.g,
             vertex.col = t1.l.g%v%"color",
             vertex.cex = t1.l.g%v%"size" / 40,
             vertex.lwd = 0.25,
             vertex.border = "black",
             coord = t1.coords,
             pad = 5,
             edge.lwd = .01,
             edge.col = t1.l.g%e%"color")
dev.off()

## Time 2 ####

# set default color of edges as non-state color
t2.l.g%e%"color" = rep(crim.col, length(t2.l.g%v%"vertex.names"))

# get all edges connected to law enforcement
.le_edges = unique(unlist(sapply(as.integer(t2.l.g%v%"ID")[t2.l.g%v%"Law.Enforcement"], function(x) get.edgeIDs(t2.l.g, x))))
# set all those that are connected to law enforcement as le color
set.edge.value(t2.l.g, "color", le.col, .le_edges)

# get all edges connected to politicains
.pol_edges = unique(unlist(sapply(as.integer(t2.l.g%v%"ID")[t2.l.g%v%"Politician"], function(x) get.edgeIDs(t2.l.g, x))))
# set all those that are connected to politicians as politician color
set.edge.value(t2.l.g, "color", pol.col, .pol_edges)

### reorder all the edges ####

# get the edges and re-order
t2.edgelist = as.edgelist(t2.l.g, attrname = "color")
t2.edgelist = rbind(t2.edgelist[t2.edgelist[, 3] == crim.col, ], t2.edgelist[t2.edgelist[, 3] == le.col, ], t2.edgelist[t2.edgelist[, 3] == pol.col, ])

# get all edge IDs and delete all current edges
.all_t2_edges = unique(unlist(sapply(as.integer(t2.l.g%v%"ID"), function(x) get.edgeIDs(t2.l.g, x))))
delete.edges(t2.l.g, .all_t2_edges)

# re-add edges
add.edges(t2.l.g, tail = t2.edgelist[,1], head =  t2.edgelist[,2], names.eval = "color", vals.eval = t2.edgelist[,3])

### plot ####

# plot and save
pdf("./vis/network_plots/t2_network.pdf")
plot.network(t2.l.g,
             vertex.col = t2.l.g%v%"color",
             vertex.cex = t2.l.g%v%"size" / 40,
             vertex.lwd = 0.25,
             vertex.border = "black",
             coord = t2.coords,
             pad = 5,
             edge.lwd = .01,
             edge.col = t2.l.g%e%"color")
dev.off()

# Interactive plots ####

## transform to vis network data ####

t1.visnet = toVisNetworkData(intergraph::asIgraph(t1.l.g))
t2.visnet = toVisNetworkData(intergraph::asIgraph(t2.l.g))

# add in hover text
t1.visnet[["nodes"]]$title = paste0("<b>Node Metrics</b>", "<br>",
                                    "Degree: ", t1.visnet[["nodes"]]$degree, "<br>",
                                    "Eigenvector: ", round(t1.visnet[["nodes"]]$evc, digits = 3), "<br>",
                                    "Nestedness: ", t1.visnet[["nodes"]]$nestedness, "<br>")
t2.visnet[["nodes"]]$title = paste0("<b>Node Metrics</b>", "<br>",
                                    "Degree: ", t2.visnet[["nodes"]]$degree, "<br>",
                                    "Eigenvector: ", round(t2.visnet[["nodes"]]$evc, digits = 3), "<br>",
                                    "Nestedness: ", t2.visnet[["nodes"]]$nestedness, "<br>")

## set up plots ####

t1.interactive = visNetwork(nodes = t1.visnet$nodes, edges = t1.visnet$edges) %>%
  visEdges(color = list(color = "grey", highlight = "black"), smooth = FALSE) %>%
  visOptions(highlightNearest = TRUE, selectedBy = "group") %>%
  visLayout(randomSeed = 1337) %>%
  visPhysics(solver = "forceAtlas2Based")

saveWidget(t1.interactive, "t1_interactive_network.html")

t2.interactive = visNetwork(nodes = t2.visnet$nodes, edges = t2.visnet$edges) %>%
  visEdges(color = list(color = "grey", highlight = "black"), smooth = FALSE) %>%
  visOptions(highlightNearest = TRUE, selectedBy = "group") %>%
  visLayout(randomSeed = 1337) %>%
  visPhysics(solver = "forceAtlas2Based", timestep = 0.35)

saveWidget(t2.interactive, "t2_interactive_network.html")

# make plots for website ####

# make copies of existing network above
t1.visnet_web = t1.visnet
t2.visnet_web = t2.visnet

# add nicer colors
t1.visnet_web$nodes$color[t1.visnet_web$nodes$group == "Law Enforcement"] = "#362c47"
t1.visnet_web$nodes$color[t1.visnet_web$nodes$group == "Politician"] = "#ff7400"

t2.visnet_web$nodes$color[t2.visnet_web$nodes$group == "Law Enforcement"] = "#362c47"
t2.visnet_web$nodes$color[t2.visnet_web$nodes$group == "Politician"] = "#ff7400"

t1.interactive_web = visNetwork(nodes = t1.visnet_web$nodes, edges = t1.visnet_web$edges) %>%
  visEdges(color = list(color = "grey", highlight = "black"), smooth = FALSE) %>%
  visOptions(highlightNearest = TRUE, selectedBy = "group") %>%
  visLayout(randomSeed = 1337) %>%
  visPhysics(solver = "forceAtlas2Based")
#%>% visLegend(main = "Node Type")

t1.interactive_web$sizingPolicy$browser$fill = TRUE

saveWidget(t1.interactive_web, "t1_interactive_network_web.html")

t2.interactive_web = visNetwork(nodes = t2.visnet_web$nodes, edges = t2.visnet_web$edges) %>%
  visEdges(color = list(color = "grey", highlight = "black"), smooth = FALSE) %>%
  visOptions(highlightNearest = TRUE, selectedBy = "group") %>%
  visLayout(randomSeed = 1337) %>%
  visPhysics(solver = "forceAtlas2Based", timestep = 0.35)
#%>% visLegend(main = "Node Type")

t2.interactive_web$sizingPolicy$browser$fill = TRUE

saveWidget(t2.interactive_web, "t2_interactive_network_web.html")












