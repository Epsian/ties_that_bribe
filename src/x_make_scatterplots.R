# to make scatterplots. 1) degree on X, nestedness on Y, 2) EVC on X nestedness on Y; crim, LE, pol as colors

# setup ####

# load packages
library(plotly)

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

t1_dn_plot = plot_ly(data = t1_data, x = ~degree, y = ~nestedness, color = ~class, type = "scatter", mode = "markers")
t1_dn_plot

## make evc x nestedness plot ####

t1_en_plot = plot_ly(data = t1_data, x = ~evc, y = ~nestedness, color = ~class, type = "scatter", mode = "markers")
t1_en_plot

# t2 ####

## make degree x nesteddness plot ####

t2_dn_plot = plot_ly(data = t2_data, x = ~degree, y = ~nestedness, color = ~class, type = "scatter", mode = "markers")
t2_dn_plot

## make evc x nestedness plot ####

t2_en_plot = plot_ly(data = t2_data, x = ~evc, y = ~nestedness, color = ~class, type = "scatter", mode = "markers")
t2_en_plot


# working on regression lines ---------------------------------------------------

# with single fit line

#fit = lm(nestedness ~ evc, data = t1_data)
#
#t1_data %>%
#  plot_ly(x = ~evc) %>%
#  add_markers(y = ~nestedness) %>%
#  add_lines(x = ~evc, y = fitted(fit))
