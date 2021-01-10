# to plot a 2 step network for thompson and create comparison graphs

# setup ####

# load packages
library(ggplot2)
library(ggthemes)
library(plotly)
library(htmlwidgets)

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
t1_data = read.csv("./data/t1_data.csv", header = TRUE, stringsAsFactors = FALSE)
t2_data = read.csv("./data/t2_data.csv", header = TRUE, stringsAsFactors = FALSE)

# make classification column
t1_data$`Node Type` = "Criminal"
t1_data[t1_data$law_enforcement == TRUE, "Node Type"] = "Law Enforcement"
t1_data[t1_data$politician == TRUE, "Node Type"] = "Politician"

t2_data$`Node Type` = "Criminal"
t2_data[t2_data$law_enforcement == TRUE, "Node Type"] = "Law Enforcement"
t2_data[t2_data$politician == TRUE, "Node Type"] = "Politician"

# make barcharts ####

# make df to hold results
billdf = data.frame("group" = c("Thompson", "Law Enforcement", "Politician", "Criminal"), stringsAsFactors = FALSE)

# degree
billdf$degree = c(t2_data$degree[t2_data$vertex_name == "Thompson, Big Bill"], mean(t2_data$degree[t2_data$law_enforcement]), mean(t2_data$degree[t2_data$politician]), mean(t2_data$degree[!(t2_data$politician | t2_data$law_enforcement)]))

# eigenvectors
billdf$evc = c(t2_data$evc[t2_data$vertex_name == "Thompson, Big Bill"], mean(t2_data$evc[t2_data$law_enforcement]), mean(t2_data$evc[t2_data$politician]), mean(t2_data$evc[!(t2_data$politician | t2_data$law_enforcement)]))

# nestedness
billdf$nestedness = c(t2_data$nestedness[t2_data$vertex_name == "Thompson, Big Bill"], mean(t2_data$nestedness[t2_data$law_enforcement]), mean(t2_data$nestedness[t2_data$politician]), mean(t2_data$nestedness[!(t2_data$politician | t2_data$law_enforcement)]))

# plot ####

# reshape df
bill_melt = reshape2::melt(billdf)

degree_plot = ggplot(bill_melt[bill_melt$variable == "degree",], aes(x=variable, y=value)) +
  geom_segment(aes(x=variable, xend=variable, y=min(value), yend=max(value)), linetype="dashed", size=0.1) +   # Draw dashed lines
  geom_point(aes(col=group), size=3) +   # Draw points
  scale_color_manual(breaks = c("Criminal", "Law Enforcement", "Politician", "Thompson"), values = c(crim.col, le.col, pol.col, "red")) +
  labs(title="Degree", subtitle=NULL, color = "Node Type") + xlab(NULL) + ylab("Value") +
  theme_classic(base_size = 12, base_family = "STIX") + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) + coord_flip()







evc_plot = ggplot(bill_melt[bill_melt$variable == "evc",], aes(x=variable, y=value)) +
  geom_segment(aes(x=variable, 
                   xend=variable, 
                   y=min(value), 
                   yend=max(value)), 
               linetype="dashed", 
               size=0.1) +   # Draw dashed lines
  geom_point(aes(col=group), size=3) +   # Draw points
  scale_color_manual(breaks = c("Criminal", "Law Enforcement", "Politician", "Thompson"), values = c(crim.col, le.col, pol.col, "red")) +
  labs(title="Degree", subtitle=NULL) + 
  theme_classic(base_size = 12, base_family = "STIX") +
  coord_flip()

nestedness_plot = ggplot(bill_melt[bill_melt$variable == "nestedness",], aes(x=variable, y=value)) +
  geom_segment(aes(x=variable, 
                   xend=variable, 
                   y=min(value), 
                   yend=max(value)), 
               linetype="dashed", 
               size=0.1) +   # Draw dashed lines
  geom_point(aes(col=group), size=3) +   # Draw points
  scale_color_manual(breaks = c("Criminal", "Law Enforcement", "Politician", "Thompson"), values = c(crim.col, le.col, pol.col, "red")) +
  labs(title="Degree", subtitle=NULL) + 
  theme_classic(base_size = 12, base_family = "STIX") +
  coord_flip()



