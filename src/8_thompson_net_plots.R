# to plot a 2 step network for thompson and create comparison graphs

# setup ####

# load packages
library(ggplot2)
library(ggthemes)
library(plotly)
library(htmlwidgets)
library(ggpubr)
library(igraph)
library(statnet)

# add journal font
library(showtext)
font_add(family = "STIX", regular = "P:/pCloud Sync/Projects/school/QP/state_sims/STIX2Text-Regular.otf")
showtext_auto() 

# dot colors
crim.col = "#cccccc"
le.col = "#7c7c7c"
pol.col = "#000000"

# static plot sizes
.static_width = 650
.static_height = 650

# load data
t2_data = read.csv("./data/t2_data.csv", header = TRUE, stringsAsFactors = FALSE)
t2.l.g = readRDS("./data/t2.l.g.rda")
t2.coords = readRDS("./vis/t2.plot.loc.rda")

# make classification column
t2_data$`Node Type` = "Non-State"
t2_data[t2_data$law_enforcement == TRUE, "Node Type"] = "Law Enforcement"
t2_data[t2_data$politician == TRUE, "Node Type"] = "Politician"

# make dotplots ####

# make df to hold results
billdf = data.frame("group" = c("Thompson", "Law Enforcement", "Politician", "Non-State"), stringsAsFactors = FALSE)

# degree
billdf$degree = c(t2_data$degree[t2_data$vertex_name == "Thompson, Big Bill"], mean(t2_data$degree[t2_data$law_enforcement]), mean(t2_data$degree[t2_data$politician]), mean(t2_data$degree[!(t2_data$politician | t2_data$law_enforcement)]))

# eigenvectors
billdf$evc = c(t2_data$evc[t2_data$vertex_name == "Thompson, Big Bill"], mean(t2_data$evc[t2_data$law_enforcement]), mean(t2_data$evc[t2_data$politician]), mean(t2_data$evc[!(t2_data$politician | t2_data$law_enforcement)]))

# nestedness
billdf$nestedness = c(t2_data$nestedness[t2_data$vertex_name == "Thompson, Big Bill"], mean(t2_data$nestedness[t2_data$law_enforcement]), mean(t2_data$nestedness[t2_data$politician]), mean(t2_data$nestedness[!(t2_data$politician | t2_data$law_enforcement)]))

# reshape df
bill_melt = reshape2::melt(billdf)

# degree plot
degree_plot = ggplot(bill_melt[bill_melt$variable == "degree" & bill_melt$group != "Thompson",], aes(x=variable, y=value)) +
  geom_segment(aes(x=variable, xend=variable, y=min(value), yend=max(value)), linetype="dashed", size=0.1) +   # Draw dashed lines
  geom_point(aes(col=group), size=5) +   # Draw points
  scale_color_manual(breaks = c("Non-State", "Law Enforcement", "Politician"), values = c(crim.col, le.col, pol.col)) +
  labs(title="Degree", subtitle=NULL, color = "Node Type") + xlab(NULL) + ylab("Value") +
  geom_hline(yintercept = 7, size = 1) + # 7 is bill's degree
  theme_classic(base_size = 12, base_family = "STIX") + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) + coord_flip()

degree_plot

# eigenvector
evc_plot = ggplot(bill_melt[bill_melt$variable == "evc" & bill_melt$group != "Thompson",], aes(x=variable, y=value)) +
  geom_segment(aes(x=variable, xend=variable, y=min(value), yend=0.026289433), linetype="dashed", size=0.1) +   # Draw dashed lines
  geom_point(aes(col=group), size=5) +   # Draw points
  scale_color_manual(breaks = c("Non-State", "Law Enforcement", "Politician"), values = c(crim.col, le.col, pol.col)) +
  labs(title="Eigenvector", subtitle=NULL, color = "Node Type") + xlab(NULL) + ylab("Value") +
  geom_hline(yintercept = 0.026289433, size = 1) + # 0.026289433 is bill's evc
  theme_classic(base_size = 12, base_family = "STIX") + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) + coord_flip()

evc_plot

# nestedness
nest_plot = ggplot(bill_melt[bill_melt$variable == "nestedness" & bill_melt$group != "Thompson",], aes(x=variable, y=value)) +
  geom_segment(aes(x=variable, xend=variable, y=min(value), yend=max(value)), linetype="dashed", size=0.1) +   # Draw dashed lines
  geom_point(aes(col=group), size=5) +   # Draw points
  scale_color_manual(breaks = c("Non-State", "Law Enforcement", "Politician"), values = c(crim.col, le.col, pol.col)) +
  labs(title="Nestedness", subtitle=NULL, color = "Node Type") + xlab(NULL) + ylab("Value") +
  geom_hline(yintercept = 4, size = 1) + # 4 is bill's nestedness
  theme_classic(base_size = 12, base_family = "STIX") + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) + coord_flip()

nest_plot

# combine
multidot = ggarrange(degree_plot, evc_plot, nest_plot, ncol = 1, nrow = 3)
multidot
ggsave("./vis/thompson_plots/dotplot.pdf", multidot, scale = 1, width = 7.18, height = 4)

# make egonet ####
# --------------------------------------------------------------------------------------------------------------------------------------------------------------

# get node ID of thompson
.thompson_id = t2_data[t2_data$vertex_name == "Thompson, Big Bill", "ID"]

# get just nodes connected to thompson
.thompson_neighborhood = c(get.neighborhood(t2.l.g, .thompson_id, "combined"), .thompson_id)

# get a list of all nodes not connected to thompson
.toremove = t2_data$ID
.toremove = t2_data$ID[!(.toremove %in% .thompson_neighborhood)]

# make a network on just thompson
thompson_net = t2.l.g
thompson_net = delete.vertices(thompson_net, .toremove)







thompson_igraph = intergraph::asIgraph(t2.l.g)
thompson_igraph = igraph::set_vertex_attr(thompson_igraph, "name", value = igraph::get.vertex.attribute(thompson_igraph, "ID"))

# are node ids persevered?
igraph::vertex_attr(thompson_igraph, "vertex.names", .thompson_id)
# yes

tblock = cohesive_blocks(thompson_igraph)

blocks(tblock)
igraph::graphs_from_cohesive_blocks(tblock, thompson_igraph)

plot(tblock, thompson_igraph, size = 5)
plot_hierarchy(tblock, thompson_igraph)



# make color vector
.color = rep(crim.col, length(thompson_net%v%"vertex.names"))
.color[thompson_net%v%"Law.Enforcement"] = le.col
.color[thompson_net%v%"Politician"] = pol.col

# made sides vector
.sides = rep(100, length(thompson_net%v%"vertex.names"))
.sides[thompson_net%v%"vertex.names" == "Thompson, Big Bill"] = 4

# plot and save
pdf("./vis/thompson_plots/neighborhood.pdf")
gplot(thompson_net,
      gmode = "graph",
      vertex.col = .color,
      vertex.cex = log(thompson_net%v%"nestedness", base = 5),
      coord = t2.coords[.thompson_neighborhood,],
      pad = 5,
      vertex.sides = .sides,
      edge.col = "dark grey")
dev.off()



svg(filename = "time1_svg.svg",
    width = 10,
    height = 10,
    bg = NA)

t1.plot = gplot(t1.l.g,
                gmode = "graph",
                vertex.col = t1.col,
                edge.col = t1.l.g%e%"color",
                coord = t1.coords
)


