##### this R-script was used to preprocess the raw data from TRY database
##### the result of this script is a dataset containing the means, standard deviations and growth forms 
##### of all valid observations for the six traits leaf area, growth height, specific leaf area, 
##### leaf nitrogen concentration, seed mass and stem specific density in their respective standard unit
##### further details: https://www.try-db.org/TryWeb/TRY_Data_Release_Notes.pdf and https://www.try-db.org/de/TabDetails.php


### load libraries
library(dplyr)
require(data.table)


### set paths to data files
workdir <- ""
path_gf <- ""


### read data
setwd(workdir)

## TRY growth form data
gf <- fread(path_gf, header = T, sep = ",", fill = TRUE, dec = ".", quote = "", 
            data.table = T, select = c("AccSpeciesName", "PlantGrowthForm"))
head(gf)

# remove missing growth form data
gf <- gf[gf$PlantGrowthForm != "",]

# remove double entries of species name
gf <- gf %>% distinct(AccSpeciesName, .keep_all = TRUE)
dim(gf)

## plant functional trait data
trydat <- fread("try.txt", header = T, sep = "\t", dec = ".", quote = "", 
                data.table = T, select = c("DataID", "AccSpeciesName", "StdValue", "UnitName", "TraitID", "TraitName", "ErrorRisk"))
head(trydat)
dim(trydat)

### preprocessing

## remove those entries with high uncertainty (as explained in release notes)
trydat <- trydat[!(trydat$ErrorRisk >= 4), ]

## remove TraitID = NA indicating missing observations
trydat <- subset(trydat, !is.na(trydat$TraitID))

## group by AccSpeciesName (species name) and Trait ID (plant functional trait), compute mean and SD
agg <- trydat %>%
  group_by(AccSpeciesName, TraitID) %>%
  summarize(mean = mean(StdValue, na.rm = TRUE),
            stddev = sd(StdValue, na.rm = TRUE))

## convert to dataframe
agg <- as.data.frame(agg)

## convert from long to wide format to create new columns for each value-trait combination
agg_wide <- pivot_wider(agg, names_from = TraitID, values_from = c("mean", "stddev"))

## convert to dataframe
agg_wide <- as.data.frame(agg_wide)

### join with table containing growth form by "AccSpeciesName"
agg_wide_full <- left_join(agg_wide, gf, by = "AccSpeciesName")

# write table, will be reused in script "3_join_GBIF_TRY.R"
fwrite(agg_wide_full, file = "TRY_final.txt", col.names = TRUE)



