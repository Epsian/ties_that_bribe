# to plot a 2 step network for thompson and create comparison graphs

# setup ####

# load packages
library(ggplot2)
library(ggthemes)
library(plotly)
library(htmlwidgets)
library(ggpubr)
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
man_plot = readRDS("./vis/thompson_plots/man_plot.rda")
t2_cohesive = readRDS("./data/t2_cohesive_blocks.rda")

# make classification column
t2_data$`Node Type` = "Non-State"
t2_data[t2_data$law_enforcement == TRUE, "Node Type"] = "Law Enforcement"
t2_data[t2_data$politician == TRUE, "Node Type"] = "Politician"

# find percentile of Big Bill for metrics ####

## degree ####

# get big bill degree
t2_data[t2_data$vertex_name == "Thompson, Big Bill", "degree"]
# get estiamte
quantile(t2_data$degree, c(0, .25, .50, .70, .75, 1))
# find exact
.degree_percentile = ecdf(t2_data$degree)(7) # bill has a degree of 7
# 73 percentile

## evc ####

# get big bill evc
t2_data[t2_data$vertex_name == "Thompson, Big Bill", "evc"]
# find exact
.evc_percentile = ecdf(t2_data$evc)(0.02628943) # bill has an evc of 0.02628943
# 84 percentile

## nestedness ####

# get big bill nestedness
t2_data[t2_data$vertex_name == "Thompson, Big Bill", "nestedness"]
# find exact
.nestedness_percentile = ecdf(t2_data$nestedness)(4) # bill has a nestedness of 4
# 76 percentile

# make dotplots ####

# make df to hold results
billdf = data.frame("group" = c("Thompson", "Law Enforcement", "Politician", "Non-State"), stringsAsFactors = FALSE)

# degree
billdf$degree = c(t2_data$degree[t2_data$vertex_name == "Thompson, Big Bill"], median(t2_data$degree[t2_data$law_enforcement]), median(t2_data$degree[t2_data$politician]), median(t2_data$degree[!(t2_data$politician | t2_data$law_enforcement)]))

# eigenvectors
billdf$evc = c(t2_data$evc[t2_data$vertex_name == "Thompson, Big Bill"], median(t2_data$evc[t2_data$law_enforcement]), median(t2_data$evc[t2_data$politician]), median(t2_data$evc[!(t2_data$politician | t2_data$law_enforcement)]))

# nestedness
billdf$nestedness = c(t2_data$nestedness[t2_data$vertex_name == "Thompson, Big Bill"], median(t2_data$nestedness[t2_data$law_enforcement]), median(t2_data$nestedness[t2_data$politician]), median(t2_data$nestedness[!(t2_data$politician | t2_data$law_enforcement)]))

# reshape df
bill_melt = reshape2::melt(billdf)

# degree plot
degree_plot = ggplot(bill_melt[bill_melt$variable == "degree" & bill_melt$group != "Thompson",], aes(x=variable, y=value)) +
  geom_segment(aes(x=variable, xend=variable, y=min(value), yend=max(value)), linetype="dashed", size=0.1) +   # Draw dashed lines
  geom_point(aes(col=group), size=5) +   # Draw points
  scale_color_manual(breaks = c("Non-State", "Law Enforcement", "Politician"), values = c(crim.col, le.col, pol.col)) +
  labs(title="Degree", subtitle=NULL, color = "Node Type") + xlab(NULL) + ylab("Median Value") +
  geom_hline(yintercept = 7, size = 1) + # 7 is bill's degree
  #labs(caption = "Thompson's Degree is in the 73rd Percentile") +
  theme_classic(base_size = 12, base_family = "STIX") + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) + coord_flip()

degree_plot

# eigenvector
evc_plot = ggplot(bill_melt[bill_melt$variable == "evc" & bill_melt$group != "Thompson",], aes(x=variable, y=value)) +
  geom_segment(aes(x=variable, xend=variable, y=min(value), yend=0.03), linetype="dashed", size=0.1) +   # Draw dashed lines
  geom_point(aes(col=group), size=5) +   # Draw points
  scale_color_manual(breaks = c("Non-State", "Law Enforcement", "Politician"), values = c(crim.col, le.col, pol.col)) +
  labs(title="Eigenvector", subtitle=NULL, color = "Node Type") + xlab(NULL) + ylab("Median Value") +
  geom_hline(yintercept = 0.026289433, size = 1) + # 0.026289433 is bill's evc
  #labs(caption = "Thompson's Eigenvector is in the 84th Percentile") +
  theme_classic(base_size = 12, base_family = "STIX") + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) + coord_flip()

evc_plot

# nestedness
nest_plot = ggplot(bill_melt[bill_melt$variable == "nestedness" & bill_melt$group != "Thompson",], aes(x=variable, y=value)) +
  geom_segment(aes(x=variable, xend=variable, y=min(value), yend=max(value)), linetype="dashed", size=0.1) +   # Draw dashed lines
  geom_point(aes(col=group), size=5) +   # Draw points
  scale_color_manual(breaks = c("Non-State", "Law Enforcement", "Politician"), values = c(crim.col, le.col, pol.col)) +
  labs(title="Nestedness", subtitle=NULL, color = "Node Type") + xlab(NULL) + ylab("Median Value") +
  geom_hline(yintercept = 4, size = 1) + # 4 is bill's nestedness
  #labs(caption = "Thompson's Nestedness is in the 76th Percentile") +
  theme_classic(base_size = 12, base_family = "STIX") + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) + coord_flip()

nest_plot

# combine
multidot = ggarrange(degree_plot, evc_plot, nest_plot, ncol = 1, nrow = 3)
multidot
ggsave("./vis/thompson_plots/dotplot.pdf", multidot, scale = 1, width = 7.18, height = 4)

# make egonet ####

# get node ID of thompson
.thompson_id = t2_data[t2_data$vertex_name == "Thompson, Big Bill", "ID"]

# get id of capone for later removal
.capone_id = t2_data[t2_data$vertex_name == "Capone, Al", "ID"]

# get just nodes connected to thompson
.thompson_neighborhood = c(get.neighborhood(t2.l.g, .thompson_id, "combined"), .thompson_id)

# get all nodes in a 2step neighborhood from thompson
.thompson_2step = gapply(t2.l.g, c(1,2), 1:937, FUN = function(x) x, mode = "graph", distance = 2)[[.thompson_id]]

# get a list of all nodes not in 2step neighborhood from thompson
.toremove = t2_data$ID
.toremove = t2_data$ID[!(.toremove %in% .thompson_neighborhood)]

# make a network on just thompson
thompson_net = t2.l.g
thompson_net = delete.vertices(thompson_net, .toremove)

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
      coord = man_plot,
      pad = 5,
      vertex.sides = .sides,
      edge.col = "dark grey")
dev.off()

# find nest info to draw on plot ####

# node names plot
gplot(thompson_net,
      gmode = "graph",
      vertex.col = .color,
      vertex.cex = log(thompson_net%v%"nestedness", base = 5),
      label = thompson_net%v%"vertex.names",
      coord = man_plot,
      pad = 5,
      vertex.sides = .sides,
      edge.col = "dark grey")


# get all the blocks (cohesive subgroups) from prohibition
t2_blocks = igraph::blocks(t2_cohesive)

# get node ids for all in thompson ego net
ego_ids = thompson_net%v%"ID"
name_key = c("Capone, Al" = 131, "Crowe, Robert E." = 217, "Jackson, Dan" =  433, "Lundin, Fred" = 534, "Saltis, Polack Joe" = 781, "Tennes, Mont" = 876, "Thompson, Big Bill" = 878, "Torrio, Johnny the Fox" = 883)

# sbuset to just blocks that have nodes from thompson's ego net
ego_blocks = lapply(t2_blocks, function(block){
  
  # test if this block contains any nodes with IDs matching thompson's ego net
  thompson_ego_present = any(block %in% ego_ids)
  
  # if so keep the block, else mark for deletion
  if(thompson_ego_present){return(block)} else {
    return(NULL)
  }
})

# delete all those that are NULL
ego_blocks[sapply(ego_blocks, is.null)] = NULL

# replace numbers with names for easy plotting
string_blocks = lapply(ego_blocks, function(block){
  
  # for every name in thompson ego net, replace thier numbers
  for(i in 1:length(name_key)){
    
    # simplify block into character vector
    block = as.character(block)
    
    # replace numbers with names for neighborhood nodes
    block[block == name_key[i]] = names(name_key[i])
    
  }
  
  # keep only names
  block = block[!grepl("^\\d", block, perl = TRUE)]
  
  # collapse into single strings
  block = paste0(block, collapse = "--")
  
  # return
  return(block)
})

# get a list of unique blocks for drawing
table(unlist(string_blocks))

# split nests by actor for text
capone_blocks = string_blocks[sapply(string_blocks, function(block){"Capone, Al" %in% block})]

# checks ####

# see how many blocks thompson is in
table(sapply(t2_blocks, function(x){878 %in% x}))

# see those nests
thompson_blocks = t2_blocks[sapply(t2_blocks, function(x){878 %in% x})]

# See if thompson ego net is all present in each nest
lapply(thompson_blocks, function(x){
  
  present = name_key %in% x
  names(present) = names(name_key)
  
  return(present)
  
  })

# find the smallest nest

summary(sapply(t2_blocks, length))



