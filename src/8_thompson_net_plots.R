# to plot a 2 step network for thompson and create comparison graphs

# setup ####

# load packages
library(ggplot2)
library(ggthemes)
library(plotly)
library(htmlwidgets)
library(ggpubr)

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
t1_data$`Node Type` = "Non-State"
t1_data[t1_data$law_enforcement == TRUE, "Node Type"] = "Law Enforcement"
t1_data[t1_data$politician == TRUE, "Node Type"] = "Politician"

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




