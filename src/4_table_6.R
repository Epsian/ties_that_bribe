# to make the nodel level table

# setup ####

# packages
library(statnet)

# data load

## networks
t1.l.g = readRDS("./data/t1.l.g.rda")
t2.l.g = readRDS("./data/t2.l.g.rda")

## data csvs
t1_data = read.csv("./data/t1_data.csv", header = TRUE, stringsAsFactors = FALSE)
t2_data = read.csv("./data/t2_data.csv", header = TRUE, stringsAsFactors = FALSE)

# make classification column
t1_data$class = "Criminal"
t1_data[t1_data$law_enforcement == TRUE, "class"] = "Law Enforcement"
t1_data[t1_data$politician == TRUE, "class"] = "Politician"

t2_data$class = "Criminal"
t2_data[t2_data$law_enforcement == TRUE, "class"] = "Law Enforcement"
t2_data[t2_data$politician == TRUE, "class"] = "Politician"

# node and edge counts ####

## Time 1 ####

### node counts ####

# counts
table(t1_data$class)
# percentages
(table(t1_data$class)/nrow(t1_data))*100

### edges ####

# t1 law enforcement
t1_le_edges = unlist(lapply(t1_data$ID[t1_data$law_enforcement == TRUE], get.edgeIDs, x=t1.l.g))
t1_le_edges = t1_le_edges[!duplicated(t1_le_edges)]
length(t1_le_edges) # how many edges
(length(t1_le_edges) / length(t1.l.g%e%"na")) * 100 # percentage of edges


# t1 politician
t1_pol_edges = unlist(lapply(t1_data$ID[t1_data$politician == TRUE], get.edgeIDs, x=t1.l.g))
t1_pol_edges = t1_pol_edges[!duplicated(t1_pol_edges)]
length(t1_pol_edges)
(length(t1_pol_edges) / length(t1.l.g%e%"na")) * 100

# t1 non-state
t1_ns_edges = unlist(lapply(t1_data$ID[t1_data$politician == FALSE & t1_data$law_enforcement == FALSE], get.edgeIDs, x=t1.l.g))
t1_ns_edges = t1_ns_edges[!duplicated(t1_ns_edges)]
length(t1_ns_edges)
(length(t1_ns_edges) / length(t1.l.g%e%"na")) * 100

## Time 2 ####

### node counts ####

# counts
table(t2_data$class)
# percentages
(table(t2_data$class)/nrow(t2_data))*100

### edges ####

# t2 law enforcement
t2_le_edges = unlist(lapply(t2_data$ID[t2_data$law_enforcement == TRUE], get.edgeIDs, x=t2.l.g))
t2_le_edges = t2_le_edges[!duplicated(t2_le_edges)]
length(t2_le_edges)
(length(t2_le_edges) / length(t2.l.g%e%"na")) * 100

# t2 politician
t2_pol_edges = unlist(lapply(t2_data$ID[t2_data$politician == TRUE], get.edgeIDs, x=t2.l.g))
t2_pol_edges = t2_pol_edges[!duplicated(t2_pol_edges)]
length(t2_pol_edges)
(length(t2_pol_edges) / length(t2.l.g%e%"na")) * 100

# t2 non-state
t2_ns_edges = unlist(lapply(t2_data$ID[t2_data$politician == FALSE & t2_data$law_enforcement == FALSE], get.edgeIDs, x=t2.l.g))
t2_ns_edges = t2_ns_edges[!duplicated(t2_ns_edges)]
length(t2_ns_edges)
(length(t2_ns_edges) / length(t2.l.g%e%"na")) * 100

# group means ####

## Time 1 ####

### degree ####
mean(t1_data[t1_data$'law_enforcement', 'degree'])
mean(t1_data[t1_data$'politician', 'degree'])
mean(t1_data[!(t1_data$'law_enforcement' | t1_data$'politician'), 'degree'])

sd(t1_data[t1_data$'law_enforcement', 'degree'])
sd(t1_data[t1_data$'politician', 'degree'])
sd(t1_data[!(t1_data$'law_enforcement' | t1_data$'politician'), 'degree'])

### eigenvector ####
mean(t1_data[t1_data$'law_enforcement', 'evc'])
mean(t1_data[t1_data$'politician', 'evc'])
mean(t1_data[!(t1_data$'law_enforcement' | t1_data$'politician'), 'evc'])

sd(t1_data[t1_data$'law_enforcement', 'evc'])
sd(t1_data[t1_data$'politician', 'evc'])
sd(t1_data[!(t1_data$'law_enforcement' | t1_data$'politician'), 'evc'])

### nestedness ####
mean(t1_data[t1_data$'law_enforcement', 'nestedness'])
mean(t1_data[t1_data$'politician', 'nestedness'])
mean(t1_data[!(t1_data$'law_enforcement' | t1_data$'politician'), 'nestedness'])

sd(t1_data[t1_data$'law_enforcement', 'nestedness'])
sd(t1_data[t1_data$'politician', 'nestedness'])
sd(t1_data[!(t1_data$'law_enforcement' | t1_data$'politician'), 'nestedness'])

## Time 2 ####

### degree ####
mean(t2_data[t2_data$'law_enforcement', 'degree'])
mean(t2_data[t2_data$'politician', 'degree'])
mean(t2_data[!(t2_data$'law_enforcement' | t2_data$'politician'), 'degree'])

sd(t2_data[t2_data$'law_enforcement', 'degree'])
sd(t2_data[t2_data$'politician', 'degree'])
sd(t2_data[!(t2_data$'law_enforcement' | t2_data$'politician'), 'degree'])

### eigenvector ####
mean(t2_data[t2_data$'law_enforcement', 'evc'])
mean(t2_data[t2_data$'politician', 'evc'])
mean(t2_data[!(t2_data$'law_enforcement' | t2_data$'politician'), 'evc'])

sd(t2_data[t2_data$'law_enforcement', 'evc'])
sd(t2_data[t2_data$'politician', 'evc'])
sd(t2_data[!(t2_data$'law_enforcement' | t2_data$'politician'), 'evc'])

### nestedness ####
mean(t2_data[t2_data$'law_enforcement', 'nestedness'])
mean(t2_data[t2_data$'politician', 'nestedness'])
mean(t2_data[!(t2_data$'law_enforcement' | t2_data$'politician'), 'nestedness'])

sd(t2_data[t2_data$'law_enforcement', 'nestedness'])
sd(t2_data[t2_data$'politician', 'nestedness'])
sd(t2_data[!(t2_data$'law_enforcement' | t2_data$'politician'), 'nestedness'])



# Pre- revision material -----------------------------------------------




#### P - Edge Counts ####
#### LE - NLI ####

# Average degrees
summary(t1_data[t1_data$'law_enforcement', 'degree']) # Cops
summary(t1_data[!t1_data$'law_enforcement', 'degree']) # not cops

summary(t2_data[t2_data$'law_enforcement', 'degree']) # Cops
summary(t2_data[!t2_data$'law_enforcement', 'degree']) # not cops

# Mean Geodesic
# T1
summary(t1.data$`Mean Geodis`[t1.data$`Law.Enforcement` == TRUE])
summary(t1.data$`Mean Geodis`[t1.data$`Law.Enforcement` == FALSE])

t.test(t1.data$`Mean Geodis`[t1.data$`Law.Enforcement`==TRUE], t1.data$`Mean Geodis`[t1.data$`Law.Enforcement`==FALSE])

#T2
summary(t2.data$`Mean Geodis`[t2.data$`Law.Enforcement` == TRUE])
summary(t2.data$`Mean Geodis`[t2.data$`Law.Enforcement` == FALSE])


# Normalized Betweenness
# T1
((267-1)*(267-2)/2) # = 35245
t1.data$N.Betweenness <- (t1.data$Betweenness/35245)
summary(t1.data$N.Betweenness[t1.data$`Law.Enforcement` == TRUE])
summary(t1.data$N.Betweenness[t1.data$`Law.Enforcement` == FALSE])


# T2
((937-1)*(937-2)/2) # = 437580
t2.data$N.Betweenness <- (t2.data$Betweenness/437580)
summary(t2.data$N.Betweenness[t2.data$`Law.Enforcement`==TRUE])
summary(t2.data$N.Betweenness[t2.data$`Law.Enforcement`==FALSE])


# Eigenvector Centrality
# T1
t1.data$EVC <- evcent(t1.l.g, gmode = "graph", rescale = FALSE)
summary(t1.data$EVC)
summary(t1.data$EVC[t1.data$`Law.Enforcement` == TRUE])
summary(t1.data$EVC[t1.data$`Law.Enforcement` == FALSE])


# T2
t2.data$EVC <- evcent(t2.l.g, gmode = "graph", rescale = FALSE)
summary(t2.data$EVC)
summary(t2.data$EVC[t2.data$`Law.Enforcement` == TRUE])
summary(t2.data$EVC[t2.data$`Law.Enforcement` == FALSE])


# Closeness
# T1
t1.data$Closeness <- closeness(t1.l.g, gmode = "graph")
t1.data$N.Closenes <- (t1.data$Closeness / (nrow(t1.data) - 1))
summary(t1.data$N.Closenes[t1.data$`Law.Enforcement` == TRUE])
summary(t1.data$N.Closenes[t1.data$`Law.Enforcement` == FALSE])


# T2
t2.data$Closeness <- closeness(t2.l.g, gmode = "graph")
t2.data$N.Closenes <- (t2.data$Closeness / (nrow(t2.data) - 1))
summary(t2.data$N.Closenes[t2.data$`Law.Enforcement` == TRUE])
summary(t2.data$N.Closenes[t2.data$`Law.Enforcement` == FALSE])


#### LE - Get Edge Counts ####



t1.crim.edges <- lapply(t1.data$ID[t1.data$`Law.Enforcement` == FALSE], get.edgeIDs, x=t1.l.g)
t1.crim.edges <- unlist(t1.crim.edges)
t1.crim.edges[duplicated(t1.crim.edges)] <- NA
t1.crim.edges <- t1.crim.edges[complete.cases(t1.crim.edges)]

length(t1.crim.edges)

t1.all.edges <- lapply(t1.data$ID, get.edgeIDs, x=t1.l.g)
t1.all.edges <- unlist(t1.all.edges)
t1.all.edges[duplicated(t1.all.edges)] <- NA
t1.all.edges <- t1.all.edges[complete.cases(t1.all.edges)]

length(t1.all.edges)

# T2
t2.pol.edges <- lapply(t2.data$ID[t2.data$`Law.Enforcement` == TRUE], get.edgeIDs, x=t2.l.g)
t2.pol.edges <- unlist(t2.pol.edges)
t2.pol.edges[duplicated(t2.pol.edges)] <- NA
t2.pol.edges <- t2.pol.edges[complete.cases(t2.pol.edges)]

length(t2.pol.edges)

t2.crim.edges <- lapply(t2.data$ID[t2.data$`Law.Enforcement` == FALSE], get.edgeIDs, x=t2.l.g)
t2.crim.edges <- unlist(t2.crim.edges)
t2.crim.edges[duplicated(t2.crim.edges)] <- NA
t2.crim.edges <- t2.crim.edges[complete.cases(t2.crim.edges)]

length(t2.crim.edges)

t2.all.edges <- lapply(t2.data$ID, get.edgeIDs, x=t2.l.g)
t2.all.edges <- unlist(t2.all.edges)
t2.all.edges[duplicated(t2.all.edges)] <- NA
t2.all.edges <- t2.all.edges[complete.cases(t2.all.edges)]

length(t2.all.edges)

#### LE - Cross Time Comparisons ####

# Mean Degree
t.test(t1.data$Degree[t1.data$`Law.Enforcement` == TRUE], t2.data$Degree[t2.data$`Law.Enforcement` == TRUE])
t.test(t1.data$Degree[t1.data$`Law.Enforcement` == FALSE], t2.data$Degree[t2.data$`Law.Enforcement` == FALSE])

# Mean Geodesic
t.test(t1.data$'Mean Geodis'[t1.data$`Law.Enforcement` == TRUE], t2.data$'Mean Geodis'[t2.data$`Law.Enforcement` == TRUE])
t.test(t1.data$'Mean Geodis'[t1.data$`Law.Enforcement` == FALSE], t2.data$'Mean Geodis'[t2.data$`Law.Enforcement` == FALSE])

# Average Betweenness
t.test(t1.data$N.Betweenness[t1.data$`Law.Enforcement` == TRUE], t2.data$N.Betweenness[t2.data$`Law.Enforcement` == TRUE])
t.test(t1.data$N.Betweenness[t1.data$`Law.Enforcement` == FALSE], t2.data$N.Betweenness[t2.data$`Law.Enforcement` == FALSE])

# EVC
t.test(t1.data$EVC[t1.data$`Law.Enforcement` == TRUE], t2.data$EVC[t2.data$`Law.Enforcement` == TRUE])
t.test(t1.data$EVC[t1.data$`Law.Enforcement` == FALSE], t2.data$EVC[t2.data$`Law.Enforcement` == FALSE])

# Closeness
t.test(t1.data$N.Closenes[t1.data$`Law.Enforcement` == TRUE], t2.data$N.Closenes[t2.data$`Law.Enforcement` == TRUE])
t.test(t1.data$N.Closenes[t1.data$`Law.Enforcement` == FALSE], t2.data$N.Closenes[t2.data$`Law.Enforcement` == FALSE])

#### LE - Dyad Counts ####

# cops to Cops T1
t1.cc <- lapply(t1.data$ID[t1.data$`Law.Enforcement` == TRUE], get.edgeIDs, x=t1.l.g)
t1.cc <- unlist(t1.cc)
length(t1.cc[duplicated(t1.cc)])

# Cops to Cops t2
t2.cc <- lapply(t2.data$ID[t2.data$`Law.Enforcement` == TRUE], get.edgeIDs, x=t2.l.g)
t2.cc <- unlist(t2.cc)
length(t2.cc[duplicated(t2.cc)])

# Cops to non Cops T1
table(t1.pol.edges %in% t1.crim.edges)
table(t1.crim.edges %in% t1.pol.edges)

# Cops to non Cops T2
table(t2.pol.edges %in% t2.crim.edges)
table(t2.crim.edges %in% t2.pol.edges)

#NLE to NLE T1
t1.nle <- lapply(t1.data$ID[t1.data$`Law.Enforcement` == FALSE], get.edgeIDs, x=t1.l.g)
t1.nle <- unlist(t1.nle)
length(t1.nle[duplicated(t1.nle)])

#NLE to NLE T2
t2.nle <- lapply(t2.data$ID[t2.data$`Law.Enforcement` == FALSE], get.edgeIDs, x=t2.l.g)
t2.nle <- unlist(t2.nle)
length(t2.nle[duplicated(t2.nle)])

#### LE Clean Up ####

rm(t1.cc, t1.crim.edges, t1.nle, t1.pol.edges, t1.all.edges, t2.cc, t2.crim.edges, t2.nle, t2.pol.edges, t2.all.edges)

#### P - NLI ####

# Mean Degree
summary(t1.data$Degree[t1.data$Politician == TRUE])
summary(t1.data$Degree[t1.data$Politician == FALSE])

summary(t2.data$Degree[t2.data$Politician == TRUE])
summary(t2.data$Degree[t2.data$Politician == FALSE])

t.test(t1.data$Degree[t1.data$`Politician`==TRUE], t1.data$Degree[t1.data$`Politician`==FALSE])
t.test(t2.data$Degree[t2.data$`Politician`==TRUE], t2.data$Degree[t2.data$`Politician`==FALSE])

# Mean Geodesic
# T1
summary(t1.data$`Mean Geodis`[t1.data$`Politician` == TRUE])
summary(t1.data$`Mean Geodis`[t1.data$`Politician` == FALSE])

t.test(t1.data$`Mean Geodis`[t1.data$`Politician`==TRUE], t1.data$`Mean Geodis`[t1.data$`Politician`==FALSE])

#T2
summary(t2.data$`Mean Geodis`[t2.data$`Politician` == TRUE])
summary(t2.data$`Mean Geodis`[t2.data$`Politician` == FALSE])

t.test(t2.data$`Mean Geodis`[t2.data$`Politician`==TRUE], t2.data$`Mean Geodis`[t2.data$`Politician`==FALSE])

# Normalized Betweenness
# T1
((267-1)*(267-2)/2) # = 35245
t1.data$N.Betweenness <- (t1.data$Betweenness/35245)
summary(t1.data$N.Betweenness[t1.data$`Politician` == TRUE])
summary(t1.data$N.Betweenness[t1.data$`Politician` == FALSE])

t.test(t1.data$N.Betweenness[t1.data$`Politician`==TRUE], t1.data$N.Betweenness[t1.data$`Politician`==FALSE])

# T2
((937-1)*(937-2)/2) # = 437580
t2.data$N.Betweenness <- (t2.data$Betweenness/437580)
summary(t2.data$N.Betweenness[t2.data$`Politician`==TRUE])
summary(t2.data$N.Betweenness[t2.data$`Politician`==FALSE])

t.test(t2.data$N.Betweenness[t2.data$`Politician`==TRUE], t2.data$N.Betweenness[t2.data$`Politician`==FALSE])

# Eigenvector Centrality
# T1
t1.data$EVC <- evcent(t1.l.g, gmode = "graph", rescale = FALSE)
summary(t1.data$EVC)
summary(t1.data$EVC[t1.data$`Politician` == TRUE])
summary(t1.data$EVC[t1.data$`Politician` == FALSE])

t.test(t1.data$EVC[t1.data$`Politician` == TRUE], t1.data$EVC[t1.data$`Politician` == FALSE])

# T2
t2.data$EVC <- evcent(t2.l.g, gmode = "graph", rescale = FALSE)
summary(t2.data$EVC)
summary(t2.data$EVC[t2.data$`Politician` == TRUE])
summary(t2.data$EVC[t2.data$`Politician` == FALSE])

t.test(t2.data$EVC[t2.data$`Politician` == TRUE], t2.data$EVC[t2.data$`Politician` == FALSE])

# Closeness
# T1
t1.data$Closeness <- closeness(t1.l.g, gmode = "graph")
t1.data$N.Closenes <- (t1.data$Closeness / (nrow(t1.data) - 1))
summary(t1.data$N.Closenes[t1.data$`Politician` == TRUE])
summary(t1.data$N.Closenes[t1.data$`Politician` == FALSE])

t.test(t1.data$N.Closenes[t1.data$`Politician` == TRUE], t1.data$N.Closenes[t1.data$`Politician` == FALSE])

# T2
t2.data$Closeness <- closeness(t2.l.g, gmode = "graph")
t2.data$N.Closenes <- (t2.data$Closeness / (nrow(t2.data) - 1))
summary(t2.data$N.Closenes[t2.data$`Politician` == TRUE])
summary(t2.data$N.Closenes[t2.data$`Politician` == FALSE])

t.test(t2.data$N.Closenes[t2.data$`Politician` == TRUE], t2.data$N.Closenes[t2.data$`Politician` == FALSE])

#### P - Edge Counts ####

# T1
t1.politician.edges <- lapply(t1.data$ID[t1.data$`Politician` == TRUE], get.edgeIDs, x=t1.l.g)
t1.politician.edges <- unlist(t1.politician.edges)
t1.politician.edges[duplicated(t1.politician.edges)] <- NA
t1.politician.edges <- t1.politician.edges[complete.cases(t1.politician.edges)]

length(t1.politician.edges)

t1.non.politician.edges <- lapply(t1.data$ID[t1.data$`Politician` == FALSE], get.edgeIDs, x=t1.l.g)
t1.non.politician.edges <- unlist(t1.non.politician.edges)
t1.non.politician.edges[duplicated(t1.non.politician.edges)] <- NA
t1.non.politician.edges <- t1.non.politician.edges[complete.cases(t1.non.politician.edges)]

length(t1.non.politician.edges)

t1.all.edges <- lapply(t1.data$ID, get.edgeIDs, x=t1.l.g)
t1.all.edges <- unlist(t1.all.edges)
t1.all.edges[duplicated(t1.all.edges)] <- NA
t1.all.edges <- t1.all.edges[complete.cases(t1.all.edges)]

length(t1.all.edges)

# T2
t2.politician.edges <- lapply(t2.data$ID[t2.data$`Politician` == TRUE], get.edgeIDs, x=t2.l.g)
t2.politician.edges <- unlist(t2.politician.edges)
t2.politician.edges[duplicated(t2.politician.edges)] <- NA
t2.politician.edges <- t2.politician.edges[complete.cases(t2.politician.edges)]

length(t2.politician.edges)

t2.non.politician.edges <- lapply(t2.data$ID[t2.data$`Politician` == FALSE], get.edgeIDs, x=t2.l.g)
t2.non.politician.edges <- unlist(t2.non.politician.edges)
t2.non.politician.edges[duplicated(t2.non.politician.edges)] <- NA
t2.non.politician.edges <- t2.non.politician.edges[complete.cases(t2.non.politician.edges)]

length(t2.non.politician.edges)

t2.all.edges <- lapply(t2.data$ID, get.edgeIDs, x=t2.l.g)
t2.all.edges <- unlist(t2.all.edges)
t2.all.edges[duplicated(t2.all.edges)] <- NA
t2.all.edges <- t2.all.edges[complete.cases(t2.all.edges)]

length(t2.all.edges)

#### P - Cross Time Comparisons ####

# Mean Degree
t.test(t1.data$Degree[t1.data$`Politician` == TRUE], t2.data$Degree[t2.data$`Politician` == TRUE])
t.test(t1.data$Degree[t1.data$`Politician` == FALSE], t2.data$Degree[t2.data$`Politician` == FALSE])

# Mean Geodesic
t.test(t1.data$'Mean Geodis'[t1.data$`Politician` == TRUE], t2.data$'Mean Geodis'[t2.data$`Politician` == TRUE])
t.test(t1.data$'Mean Geodis'[t1.data$`Politician` == FALSE], t2.data$'Mean Geodis'[t2.data$`Politician` == FALSE])

# Average Betweenness
t.test(t1.data$N.Betweenness[t1.data$`Politician` == TRUE], t2.data$N.Betweenness[t2.data$`Politician` == TRUE])
t.test(t1.data$N.Betweenness[t1.data$`Politician` == FALSE], t2.data$N.Betweenness[t2.data$`Politician` == FALSE])

# EVC
t.test(t1.data$EVC[t1.data$`Politician` == TRUE], t2.data$EVC[t2.data$`Politician` == TRUE])
t.test(t1.data$EVC[t1.data$`Politician` == FALSE], t2.data$EVC[t2.data$`Politician` == FALSE])

# Closeness
t.test(t1.data$N.Closenes[t1.data$`Politician` == TRUE], t2.data$N.Closenes[t2.data$`Politician` == TRUE])
t.test(t1.data$N.Closenes[t1.data$`Politician` == FALSE], t2.data$N.Closenes[t2.data$`Politician` == FALSE])

#### P - Dyad Counts ####

# Pols to Pols T1
t1.cc <- lapply(t1.data$ID[t1.data$`Politician` == TRUE], get.edgeIDs, x=t1.l.g)
t1.cc <- unlist(t1.cc)
length(t1.cc[duplicated(t1.cc)])

# Pols to Pols t2
t2.cc <- lapply(t2.data$ID[t2.data$`Politician` == TRUE], get.edgeIDs, x=t2.l.g)
t2.cc <- unlist(t2.cc)
length(t2.cc[duplicated(t2.cc)])

# Pols to non Pols T1
table(t1.politician.edges %in% t1.non.politician.edges)
table(t1.non.politician.edges %in% t1.politician.edges)

# Pols to non Pols T2
table(t2.politician.edges %in% t2.non.politician.edges)
table(t2.non.politician.edges %in% t2.politician.edges)

#NPol to NPol T1
t1.nle <- lapply(t1.data$ID[t1.data$`Politician` == FALSE], get.edgeIDs, x=t1.l.g)
t1.nle <- unlist(t1.nle)
length(t1.nle[duplicated(t1.nle)])

#NPol to NPol T2
t2.nle <- lapply(t2.data$ID[t2.data$`Politician` == FALSE], get.edgeIDs, x=t2.l.g)
t2.nle <- unlist(t2.nle)
length(t2.nle[duplicated(t2.nle)])

#### P - Cleanup ####
rm(t1.politician.edges, t1.non.politician.edges, t1.all.edges, t2.politician.edges, t2.non.politician.edges, t2.all.edges, t1.cc, t2.cc, t1.nle, t2.nle)