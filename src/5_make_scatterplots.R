# to make scatterplots. 1) degree on X, nestedness on Y, 2) EVC on X nestedness on Y; crim, LE, pol as colors

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
t1_data$`Node Type` = "Non-State"
t1_data[t1_data$law_enforcement == TRUE, "Node Type"] = "Law Enforcement"
t1_data[t1_data$politician == TRUE, "Node Type"] = "Politician"

t2_data$`Node Type` = "Non-State"
t2_data[t2_data$law_enforcement == TRUE, "Node Type"] = "Law Enforcement"
t2_data[t2_data$politician == TRUE, "Node Type"] = "Politician"

# remove capone from time 2
t2_data = t2_data[t2_data$vertex_name != "Capone, Al",]

# t1 ####

## make degree x nesteddness plot ####

# make plot in ggplot
t1_dn_plot = ggplot(t1_data, aes(x = degree, y = nestedness, color = `Node Type`)) +
  geom_jitter(width = 1, height = 1, size = 1) +
  labs(title = "Nestedness by Degree Centrality", subtitle = "1900-1919") +
  theme_classic(base_size = 12, base_family = "STIX") +
  scale_color_manual(breaks = c("Non-State", "Law Enforcement", "Politician"), values = c(crim.col, le.col, pol.col)) +
  stat_smooth(method = lm, se = FALSE, size = .4, linetype = "dotted", fullrange = TRUE) +
  xlim(0, 70) + 
  ylim(0, 35)

# turn into dynamic with plotly
t1_dn_plot_dyn = ggplotly(t1_dn_plot) %>% layout(title = list(text = paste0('Nestedness by Degree Centrality', '<br>', '<sup>', '1900-1919', '</sup>')), xaxis = list(title = "Degree Centrality"), yaxis = list(title = "Nestedness"))

# save static and dynamic plots
orca(t1_dn_plot_dyn, "./vis/scatterplots/t1_dn_plot.pdf", width = .static_width, height = .static_height)
saveWidget(as_widget(t1_dn_plot_dyn), "t1_dn_plot.html")

## make evc x nestedness plot ####

# make plot in ggplot
t1_en_plot = ggplot(t1_data, aes(x = evc, y = nestedness, color = `Node Type`)) +
  geom_jitter(width = 0.015, height = 1, size = 0.5) +
  labs(title = "Nestedness by Eigenvector Centrality", subtitle = "1900-1919") +
  theme_classic(base_size = 12, base_family = "STIX") +
  scale_color_manual(breaks = c("Non-State", "Law Enforcement", "Politician"), values = c(crim.col, le.col, pol.col)) +
  stat_smooth(method = lm, se = FALSE, size = .4, linetype = "dotted", fullrange = TRUE) +
  xlim(0, 0.3) + 
  ylim(0, 25)

# turn into dynamic with plotly
t1_en_plot_dyn = ggplotly(t1_en_plot) %>% layout(title = list(text = paste0('Nestedness by Eigenvector Centrality', '<br>', '<sup>', '1900-1919', '</sup>')), xaxis = list(title = "Eigenvector Centrality"), yaxis = list(title = "Nestedness"))

# save static and dynamic plots
orca(t1_en_plot_dyn, "./vis/scatterplots/t1_en_plot.pdf", width = .static_width, height = .static_height)
saveWidget(as_widget(t1_en_plot_dyn), "t1_en_plot.html")

# t2 ####

## make degree x nesteddness plot ####

# make plot in ggplot
t2_dn_plot = ggplot(t2_data, aes(x = degree, y = nestedness, color = `Node Type`)) +
  geom_jitter(width = 1, height = 1, size = 0.5) +
  labs(title = "Nestedness by Degree Centrality", subtitle = "1900-1919") +
  theme_classic(base_size = 12, base_family = "STIX") +
  scale_color_manual(breaks = c("Non-State", "Law Enforcement", "Politician"), values = c(crim.col, le.col, pol.col)) +
  stat_smooth(method = lm, se = FALSE, size = .4, linetype = "dotted", fullrange = TRUE) +
  xlim(0, 70) + 
  ylim(0, 35)

# turn into dynamic with plotly
t2_dn_plot_dyn = ggplotly(t2_dn_plot) %>% layout(title = list(text = paste0('Nestedness by Degree Centrality', '<br>', '<sup>', '1920-1933', '</sup>')), xaxis = list(title = "Degree Centrality"), yaxis = list(title = "Nestedness"))

# save static and dynamic plots
orca(t2_dn_plot_dyn, "./vis/scatterplots/t2_dn_plot.pdf", width = .static_width, height = .static_height)
saveWidget(as_widget(t2_dn_plot_dyn), "t2_dn_plot.html")

## make evc x nestedness plot ####

# make plot in ggplot
t2_en_plot = ggplot(t2_data, aes(x = evc, y = nestedness, color = `Node Type`)) +
  geom_jitter(width = 0.015, height = 1, size = 0.5) +
  labs(title = "Nestedness by Eigenvector Centrality", subtitle = "1900-1919") +
  theme_classic(base_size = 12, base_family = "STIX") +
  scale_color_manual(breaks = c("Non-State", "Law Enforcement", "Politician"), values = c(crim.col, le.col, pol.col)) +
  stat_smooth(method = lm, se = FALSE, size = .4, linetype = "dotted", fullrange = TRUE) +
  xlim(0, 0.3) + 
  ylim(0, 25)

# turn into dynamic with plotly
t2_en_plot_dyn = ggplotly(t2_en_plot) %>% layout(title = list(text = paste0('Nestedness by Eigenvector Centrality', '<br>', '<sup>', '1920-1933', '</sup>')), xaxis = list(title = "Eigenvector Centrality"), yaxis = list(title = "Nestedness"))

# save static and dynamic plots
orca(t2_en_plot_dyn, "./vis/scatterplots/t2_en_plot.pdf", width = .static_width, height = .static_height)
saveWidget(as_widget(t2_en_plot_dyn), "t2_en_plot.html")


