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

t1_data = read.csv("./data/t1_data.csv", header = TRUE, stringsAsFactors = FALSE)
t2_data = read.csv("./data/t2_data.csv", header = TRUE, stringsAsFactors = FALSE)

# static plots for publication ####


















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

## set up plots ####

t1.interactive = visNetwork(nodes = t1.visnet$nodes, edges = t1.visnet$edges) %>%
  visGroups(groupname = "Criminal", color = "red", shape = "circle") %>%
  visGroups(groupname = "Law Enforcement", color = "blue", shape = "circle") %>%
  visGroups(groupname = "Politician", color = "orange", shape = "circle") %>%
  visEdges(color = list(color = "grey", highlight = "black"), smooth = FALSE) %>%
  visOptions(highlightNearest = TRUE, selectedBy = "group")

saveWidget(t1.interactive, "t1_interactive_network.html")

t2.interactive = visNetwork(nodes = t2.visnet$nodes, edges = t2.visnet$edges) %>%
  visGroups(groupname = "Criminal", color = "red", shape = "circle") %>%
  visGroups(groupname = "Law Enforcement", color = "blue", shape = "circle") %>%
  visGroups(groupname = "Politician", color = "orange", shape = "circle") %>%
  visEdges(color = list(color = "grey", highlight = "black"), smooth = FALSE) %>%
  visOptions(highlightNearest = TRUE, selectedBy = "group")

saveWidget(t2.interactive, "t2_interactive_network.html")
















