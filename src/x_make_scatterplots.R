# to make scatterplots. 1) degree on X, nestedness on Y, 2) EVC on X nestedness on Y; crim, LE, pol as colors

# setup ####

# load packages
library(ggplot2)

# load data
t1_data = read.csv("./data/t1_data.csv", header = TRUE, stringsAsFactors = FALSE)
t2_data = read.csv("./data/t2_data.csv", header = TRUE, stringsAsFactors = FALSE)

# make classification column
t1_data$class = "Criminal"
t1_data[t1_data$law_enforcement == TRUE, "class"] = "Law Enforcement"
t1_data[t1_data$politician == TRUE, "class"] = "Politician"

t2_data$class = "Criminal"
t2_data[t2_data$law_enforcement == TRUE, "class"] = "Law Enforcement"
t2_data[t2_data$politician == TRUE, "class"] = "Politician"

# remove capone from time 2
t2_data = t2_data[t2_data$vertex_name != "Capone, Al",]

# t1 ####

## make degree x nesteddness plot ####

t1_dn_plot = ggplot(t1_data, aes(degree, nestedness)) + geom_jitter(aes(color = class), width = 1, height = 1) + labs(title = "Nestedness by Degree Centrality", subtitle = "1900-1919")
t1_dn_plot + theme_classic()

## make evc x nestedness plot ####

t1_en_plot = ggplot(t1_data, aes(evc, nestedness)) + geom_jitter(aes(color = class), width = 0.015, height = 1) + labs(title = "Nestedness by Eigenvector Centrality", subtitle = "1900-1919")
t1_en_plot + theme_classic()

# t2 ####

## make degree x nesteddness plot ####

t2_dn_plot = ggplot(t2_data, aes(degree, nestedness)) + geom_jitter(aes(color = class), width = 1, height = 1) + labs(title = "Nestedness by Degree Centrality", subtitle = "1920-1933")
t2_dn_plot + theme_classic()

## make evc x nestedness plot ####

t2_en_plot = ggplot(t2_data, aes(evc, nestedness)) + geom_jitter(aes(color = class), width = 0.015, height = 1) + labs(title = "Nestedness by Eigenvector Centrality", subtitle = "1920-1933")
t2_en_plot + theme_classic()

