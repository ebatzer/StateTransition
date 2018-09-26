################################################################################
# Updating existing datasheet with 2018 data:
################################################################################

# Takes data Valerie added to dropbox in Summer 2018 ("2018 Data - To Add)
# Cleans up species naming according to previous conventions
# Merges together and write new dataframe

################################################################################
# Reading in Datasets

# Species composition
toadd <- read.csv("./data/2018 Data - To Add.csv",
                      header = TRUE,
                      stringsAsFactors = FALSE)

waps.long <- read.csv("./data/comdata_longformat_2017.csv",
                      header = TRUE,
                      stringsAsFactors = FALSE)

waps.wide <- read.csv("./data/comdata_wideformat_2017.csv",
                      header = TRUE,
                      stringsAsFactors = FALSE)

# Treatment labels and group structure
trtlabels <- read.csv("./data/trtlabels.csv",
                      header = TRUE,
                      stringsAsFactors = FALSE)

# Species labels
waps.specs <- read.csv("./data/WAPSspecies.csv", 
                       stringsAsFactors = F)

# Gathers dataset to long format before cleaning up, creates species column
long.comp <- gather(toadd, key = "species", value = "cover", -c(1:8))
View(long.comp)

# Removes year labels from species column
spec <- gsub("X\\d\\d\\.*", "", long.comp$species)

# Reassigns species variable
long.comp$species <- spec

# Converts to lowercase
long.comp$species <- tolower(long.comp$species)

# Creates a new year variable
long.comp$year = rep(2018, nrow(long.comp))

# Removes other.treatment column
long.comp = long.comp %>% select(-Other.treatment..)

# Lowercase names for dataset
names(long.comp) <- c("plot", "block", "subblock",
                      "water", "spcomp", "fertilization",
                      "clipping", "species", "cover", "year")

# Assigns all NA cover values to zero
long.comp$cover[is.na(long.comp$cover)] <- 0

# Reassigns species names to standardized names in the waps.specs dataset
for(i in waps.specs$label){
  long.comp$species <- gsub(paste(i, "$", sep = ""), waps.specs$specname[waps.specs$label == i], long.comp$species)
}

# Removes spaces from species names
long.comp$spcomp <- gsub("[[:space:]]\\+[[:space:]]*", " \\+", long.comp$spcomp)

# Removes whitespace from treatment names
long.comp$spcomp <- gsub("[[:space:]]", "", long.comp$spcomp)
long.comp$water <- gsub("[[:space:]]", "", long.comp$water)

# Assigns all NAs to zero, merges duplicated species names
long.comp$cover <- as.numeric(long.comp$cover)
long.comp$cover[is.na(long.comp$cover)] <- 0
long.comp <- long.comp %>% group_by(plot, block, subblock, water, fertilization, species, year) %>% summarise(cover = sum(cover))

# Merges with 2017-previous data
long.comp = bind_rows(waps.long, long.comp)

# Writing csv of long format cover data
write.csv(x = long.comp, file = "../data/comdata_longformat.csv", sep =",")

# Spreads dataset to wide format
comp.wide <- spread(long.comp, key = "species", value = "cover")
comp.attr <- select(comp.wide, one_of(c("year", "plot", "block", "subblock", "water", "fertilization", "year",
                                        waps.specs$specname[waps.specs$type == "class"])))
comp.wide <- ungroup(comp.wide) %>%  select(one_of(waps.specs$specname[waps.specs$type != "class"]))
comp.wide <- apply(comp.wide, 2, as.numeric)
comp.wide[is.na(comp.wide)] <- 0

# Merges with treatment labels
comp.attr <- left_join(comp.attr, trtlabels)
comp.attr$sp.trt <- gsub("[[:space:]]", "", comp.attr$sp.trt)

# Binding attribute columns with abundance matrix and writing new CSV
wide.comp <- bind_cols(data.frame(comp.attr), data.frame(comp.wide))

# Writing full wide csv
write.csv(x = wide.comp, file = "./data/comdata_wideformat.csv")
unique(wide.comp$year)
