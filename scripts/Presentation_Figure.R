library(tidyverse); library(testthat); library(vegan); library(msm); library(NbClust)
library(ggplot2); library(cluster); library(mclust); library(indicspecies); library(gganimate)

# Cover data (wide format)
cover <- read.csv("data/comdata_wideformat.csv", 
                  header = TRUE, 
                  stringsAsFactors = FALSE) %>%
  select(-X)
group_percents = cover[,8:19]
cover = cover[,-(8:19)]
comp.attr <- cover[,1:13]
comp.cover <- cover[,14:ncol(cover)]

# Treatment labels and group structure
trtlabels <- read.csv("data/trtlabels.csv",
                      header = TRUE,
                      stringsAsFactors = FALSE)

# Species treatments
waps.specs <- read.csv("data/WAPSspecies.csv",
                       header = TRUE,
                       stringsAsFactors = FALSE)

# Climate data
climate <- read.csv("data/cimis_monthly.csv",
                    header = TRUE,
                    stringsAsFactors = FALSE)

selected.trts <-c("annuals", "WAPS", "Natives", "natives+WAPS+annuals", "annuals+natives", "WAPS+annuals", "WAPS+natives")
selected.clips <- c("none")
selected.fert <- c("none")
selected.water <- c("control", "wet", "latewet", "dry") 

comp.subset = comp.cover[comp.attr$water %in% selected.water &
                           comp.attr$sp.trt %in% selected.trts &
                           comp.attr$clip %in% selected.clips &
                           comp.attr$fertilization %in% selected.fert,]

print("Total Cover in Dataset:")
specsbycover = sort(round(colSums(comp.subset) / sum(comp.subset),2), decreasing = TRUE)
# specsbycover

print("Plot Occupancy in Dataset:")
specsbyfreq = sort(round(colSums(decostand(comp.subset, method = "pa")) / nrow(comp.subset), 2), decreasing = TRUE)
# specsbyfreq

# What treatments and years should be selected?
selected.trts <-c("annuals", "WAPS", "Natives", "natives+WAPS+annuals", "annuals+natives", "WAPS+annuals", "WAPS+natives")
selected.clips <- c("none")
selected.fert <- c("none")
selected.water <- c("control") 
selected.years <- c(2008:2018)

# To include all treatments:
# selected.water <- c("control", "wet", "latewet", "dry") 

# Subsetting cover matrix
ord.mat <- comp.cover[comp.attr$water %in% selected.water &
                        comp.attr$sp.trt %in% selected.trts &
                        comp.attr$clip %in% selected.clips &
                        comp.attr$fertilization %in% selected.fert &
                        comp.attr$year %in% selected.years,]

# Subsetting attribute matrix
ord.att <- comp.attr[comp.attr$water %in% selected.water &
                       comp.attr$sp.trt %in% selected.trts &
                       comp.attr$clip %in% selected.clips &
                       comp.attr$fertilization %in% selected.fert &
                       comp.attr$year %in% selected.years,]

# Checking that these datasets are the same size
expect_true(nrow(ord.att) == nrow(ord.mat))

# Filtering species
totalcov = sum(ord.mat)
toselect = waps.specs$specname[waps.specs$group %in% c("annual", "natives", "waps")]
ord.mat = ord.mat %>% select(toselect)
cat("Fraction of total cover contained in subset =", sum(ord.mat) / totalcov)

# A number of rows have  no recorded live plants (mostly dry treatments?)
ord.att <- ord.att[rowSums(ord.mat) != 0, ]
ord.mat <- ord.mat[rowSums(ord.mat) != 0, ]

spec_groups <- read.csv("data/species_groups.csv") %>%
  rename("species" = "Species")

# Visualizing a certain community
data.subset <- bind_cols(ord.att, decostand(ord.mat, method = "total")) %>%
  filter(sp.trt == "natives+WAPS+annuals" & plot == 198) %>%
  select(year, toselect) %>%
  gather(key = "species", val = "cover", -year) %>%
  left_join(spec_groups, by = "species") %>%
  filter(!is.na(Cluster))

data.subset$cover[50] = .50

data.subset %>%
  ggplot(aes(x = year,
             y = cover,
             fill = factor(species),
             color = factor(species),
             group = species)) +
  geom_point(color = "black", pch = 21, size = 4) + 
  geom_line() +
  theme_classic() +  
  scale_x_continuous(breaks = seq(2008, 2018, by = 2)) +
  ylab("Relative Abundance") +
  xlab("Year") +
  ylim(0, 1) +
  labs(fill = "Species", color = "Species")

data.subset %>%
  ggplot(aes(x = year,
             y = cover,
             fill = factor(Cluster),
             color = factor(Cluster),
             group = species)) +
  geom_point(color = "black", pch = 21, size = 4) +
  geom_line() +
  theme_classic() +  
  scale_x_continuous(breaks = seq(2008, 2018, by = 2)) +
  ylab("Relative Abundance") +
  xlab("Year") +
  ylim(0, 1) +
  labs(fill = "Group", color = "Group")

ggplot(data = NULL, 
       aes(x = seq(2008, 2018),
           y = rep(1, 11),
           fill = as.factor(c(2,2,3,3,1,1,4,4,4,4,4)),
           shape = as.factor(c(2,2,3,3,1,1,4,4,4,4,4)))) +
  geom_line(aes(x = c(2008, 2018), y = c(1,1), fill = NULL, shape = NULL)) + 
  geom_point(size = 6) +
  scale_shape_manual(values = c(21, 22, 23, 24)) +
  theme_void() +
  labs(fill = "Group", shape = "Group")

data.subset %>%
  ggplot(aes(x = year,
             y = cover,
             fill = factor(Group, levels = c("Native", "Naturalized","Invasive")),
             color = factor(Group, levels = c("Native", "Naturalized","Invasive")),
             group = as.factor(species))) +
  geom_point(color = "black", pch = 21, size = 4) + 
  geom_line() +
  theme_classic() +  
  scale_x_continuous(breaks = seq(2008, 2018, by = 2)) +
  ylab("Relative Abundance") +
  xlab("Year") +
  ylim(0, 1) +
  labs(fill = "Group", color = "Group")

ggplot(data = NULL, 
       aes(x = seq(2008, 2018),
           y = rep(1, 11),
           fill = as.factor(c(2,2,3,3,1,1,2,2,2,2,2)),
           shape = as.factor(c(2,2,3,3,1,1,2,2,2,2,2)))) +
  geom_line() + 
  geom_point(size = 6) +
  scale_shape_manual(values = c(21, 22, 23)) +
  theme_void() +
  labs(fill = "Group", shape = "Group")
