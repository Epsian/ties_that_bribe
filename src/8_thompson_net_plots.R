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
billdf = data.frame("group" = c("bill", "all", "lep", "le", "pol", "crim"), stringsAsFactors = FALSE)

# degree
billdf$degree = c(t2_data$degree[t2_data$vertex_name == "Thompson, Big Bill"], mean(t2_data$degree), mean(t2_data$degree[t2_data$lep]), mean(t2_data$degree[t2_data$law_enforcement]), mean(t2_data$degree[t2_data$politician]), mean(t2_data$degree[!(t2_data$politician | t2_data$law_enforcement)]))

# eigenvectors
billdf$evc = c(t2_data$evc[t2_data$vertex_name == "Thompson, Big Bill"], mean(t2_data$evc), mean(t2_data$evc[t2_data$lep]), mean(t2_data$evc[t2_data$law_enforcement]), mean(t2_data$evc[t2_data$politician]), mean(t2_data$evc[!(t2_data$politician | t2_data$law_enforcement)]))

# nestedness
billdf$nestedness = c(t2_data$nestedness[t2_data$vertex_name == "Thompson, Big Bill"], mean(t2_data$nestedness), mean(t2_data$nestedness[t2_data$lep]), mean(t2_data$nestedness[t2_data$law_enforcement]), mean(t2_data$nestedness[t2_data$politician]), mean(t2_data$nestedness[!(t2_data$politician | t2_data$law_enforcement)]))

# plot
ggplot(billdf) +
  geom_dotplot(aes(x = group, y = degree)) +
  ylim(0, 1)



dotchart(as.matrix(billdf[,2:3]), labels = billdf$group)


ggplot(billdf, aes(x=dose, y=len)) + 
  geom_dotplot(binaxis='y', stackdir='center', fill="#FFAAD4")

test = reshape2::melt(billdf)



ggplot(test) +
  geom_dotplot(aes(x = variable, y = value, fill = group))



#x is degree, evc, nestedn
#y is nu,
#color is group


ggplot(test[test$variable == "degree",], aes(x=variable, y=value)) +
  geom_point(aes(col=group), size=3) +   # Draw points
  geom_segment(aes(x=variable, 
                   xend=variable, 
                   y=min(value), 
                   yend=max(value)), 
               linetype="dashed", 
               size=0.1) +   # Draw dashed lines
  labs(title="Dot Plot", 
       subtitle="Make Vs Avg. Mileage", 
       caption="source: mpg") +  
  coord_flip()

ggplot(test[test$variable == "evc",], aes(x=variable, y=value)) +
  geom_point(aes(col=group), size=3) +   # Draw points
  geom_segment(aes(x=variable, 
                   xend=variable, 
                   y=min(value), 
                   yend=max(value)), 
               linetype="dashed", 
               size=0.1) +   # Draw dashed lines
  labs(title="Dot Plot", 
       subtitle="Make Vs Avg. Mileage", 
       caption="source: mpg") +  
  coord_flip()



ggplot(test, aes(x=variable, y=value)) + 
  geom_point(aes(col=group), size=3) +   # Draw points
  geom_segment(aes(x=variable, 
                   xend=variable, 
                   y=min(value), 
                   yend=max(value)), 
               linetype="dashed", 
               size=0.1) +   # Draw dashed lines
  labs(title="Dot Plot", 
       subtitle="Make Vs Avg. Mileage", 
       caption="source: mpg") +  
  coord_flip() +
  facet_grid(variable ~ .)











df <- data.frame(City = c("AMS", "AMS", "AMS", "AMS", "BEL", "BEL", "BEL", "BEL"),
                 Month = c(4, 5, 6, 7, 4, 5, 6, 7),
                 Ratio = c(8, 9, 10, 5, 12, 13, 9, 10))

dp <- ggplot(df) + geom_dotplot(aes(x = Month, y = Ratio, fill = City), position = "dodge") 
dp

ggplot(df) + geom_dotplot(aes(x = Month, Ratio))






















t1_dn_plot = ggplot(t1_data, aes(x = degree, y = nestedness, color = `Node Type`)) +
  geom_jitter(width = 1, height = 1, size = 1) +
  labs(title = "Nestedness by Degree Centrality", subtitle = "1900-1919") +
  theme_classic(base_size = 12, base_family = "STIX") +
  scale_color_manual(breaks = c("Criminal", "Law Enforcement", "Politician"), values = c(crim.col, le.col, pol.col)) +
  stat_smooth(method = lm, se = FALSE, size = .4, linetype = "dotted", fullrange = TRUE) +
  xlim(0, 70) + 
  ylim(0, 35)
