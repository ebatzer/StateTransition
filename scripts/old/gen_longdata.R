# Cleaning up WAPS data
library(dplyr);library(tidyr)
setwd("C:/Users/ebatz/Box Sync/Eviner lab shared/Evan/Research Projects/WAPS Data")

waps.comp <- read.csv("C:/Users/ebatz/Box Sync/Eviner lab shared/Evan/Research Projects/WAPS Data/WAPSthrough2017.csv",
          header = T,
          stringsAsFactors = F)

long.comp <- gather(waps.comp, key = "species", value = "cover", -c(1:7))
spec <- gsub("X\\d\\d\\.*", "", long.comp$species)
yr <- gsub('\\.', "", long.comp$species)
yr <- gsub("X", "20", yr)
yr <- gsub("[[:alpha:]]+0?", "", yr)

long.comp$species <- spec
long.comp <- cbind(long.comp, year = as.numeric(yr))
names(long.comp) <- c("plot", "block", "subblock",
                      "water", "spcomp", "fertilization",
                      "clipping", "species", "cover", "year")

write.csv("WAPS_comp_long.csv", x = long.comp)
