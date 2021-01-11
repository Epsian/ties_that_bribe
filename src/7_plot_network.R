# make nestedness network plot

# setup ####

# load packages
library(statnet)
library(intergraph)
library(visNetwork)
library(htmlwidgets)
library(ggraph)
library(tidygraph)

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

# convert to tidygraph
tidy.t1.l.g = tidygraph::as_tbl_graph(intergraph::asIgraph(t1.l.g))
tidy.t2.l.g = tidygraph::as_tbl_graph(intergraph::asIgraph(t2.l.g))

# time 1
ggraph(tidy.t1.l.g, layout = "kk") + 
  geom_node_point(aes(size = size, color = color)) +
  geom_edge_link() + 
  theme_graph()









t1.col = ifelse(t1_data$law_enforcement, le.col, crim.col)
t1.col[t1_data$politician] = "#000000"



t1.plot = gplot(t1.l.g,
                gmode = "graph",
                vertex.col = t1.col,
                vertex.cex = log(t1_data$nestedness) + 1,
                edge.col = "grey"
                #coord = t1.coords
)









ggraph(routes_tidy, layout = "graphopt") + 
  geom_node_point() +
  geom_edge_link(aes(width = weight), alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = label), repel = TRUE) +
  labs(edge_width = "Letters") +
  theme_graph()





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
















