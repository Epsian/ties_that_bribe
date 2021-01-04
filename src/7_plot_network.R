# make nestedness network plot

# setup ####

# load packages
library(statnet)
library(intergraph)
library(visNetwork)
library(htmlwidgets)
library(ggplot2)
library(GGally)

# add journal font
library(showtext)
font_add(family = "STIX", regular = "P:/pCloud Sync/Projects/school/QP/state_sims/STIX2Text-Regular.otf")
showtext_auto() 

# load data
t1.l.g = readRDS("./data/t1.l.g.rda")
t2.l.g = readRDS("./data/t2.l.g.rda")

t1_data = read.csv("./data/t1_data.csv", header = TRUE, stringsAsFactors = FALSE)
t2_data = read.csv("./data/t2_data.csv", header = TRUE, stringsAsFactors = FALSE)

t1.coords = readRDS("./vis/t1.plot.loc.rda")
t2.coords = readRDS("./vis/t2.plot.loc.rda")

# set node colors
crim.col = "#c0c0c0"
le.col = "#4C4C4C"
pol.col = "#000000"

# static plots for publication ####

t1.col = ifelse(t1_data$law_enforcement, le.col, crim.col)
t1.col[t1_data$politician] = "#000000"



t1.plot = gplot(t1.l.g,
                gmode = "graph",
                vertex.col = t1.col,
                vertex.cex = log(t1_data$nestedness) + 1,
                edge.col = "grey"
                #coord = t1.coords
)






# Interactive plots ####

## transform to vis network data ####

t1.visnet = toVisNetworkData(intergraph::asIgraph(t1.l.g))
t2.visnet = toVisNetworkData(intergraph::asIgraph(t2.l.g))

## add in group data
t1.visnet[["nodes"]]$group = "Criminal"
t1.visnet[["nodes"]][t1.visnet[["nodes"]]$Law.Enforcement == TRUE, "group"] = "Law Enforcement"
t1.visnet[["nodes"]][t1.visnet[["nodes"]]$Politician == TRUE, "group"] = "Politician"

t2.visnet[["nodes"]]$group = "Criminal"
t2.visnet[["nodes"]][t2.visnet[["nodes"]]$Law.Enforcement == TRUE, "group"] = "Law Enforcement"
t2.visnet[["nodes"]][t2.visnet[["nodes"]]$Politician == TRUE, "group"] = "Politician"

# add in size data
t1.visnet[["nodes"]]$size = log(t1.visnet[["nodes"]]$nestedness, base = 1.2)
t1.visnet[["nodes"]]$size[t1.visnet[["nodes"]]$size == 0] = 1
t1.visnet[["nodes"]]$size = t1.visnet[["nodes"]]$size * 3

t2.visnet[["nodes"]]$size = log(t2.visnet[["nodes"]]$nestedness, base = 1.2)
t2.visnet[["nodes"]]$size[t2.visnet[["nodes"]]$size == 0] = 1
t2.visnet[["nodes"]]$size = t2.visnet[["nodes"]]$size * 3

# add in colors
t1.visnet[["nodes"]]$color = crim.col
t1.visnet[["nodes"]]$color[t1.visnet[["nodes"]]$Law.Enforcement] = le.col
t1.visnet[["nodes"]]$color[t1.visnet[["nodes"]]$Politician] = pol.col

t2.visnet[["nodes"]]$color = crim.col
t2.visnet[["nodes"]]$color[t2.visnet[["nodes"]]$Law.Enforcement] = le.col
t2.visnet[["nodes"]]$color[t2.visnet[["nodes"]]$Politician] = pol.col

# blank labels
t1.visnet[["nodes"]]$label = NA
t2.visnet[["nodes"]]$label = NA

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
















