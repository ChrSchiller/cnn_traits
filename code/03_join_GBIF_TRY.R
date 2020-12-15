##### this R-script was used to join the image dataset with the trait dataset
##### the result of this script is the full dataset containing the means, standard deviations, growth forms,
##### bioclimatic data, geolocations, download links and species names
##### of all valid observations for the six traits leaf area, growth height, specific leaf area, 
##### leaf nitrogen content, seed mass and stem specific density in their respective standard unit


### load libraries
require(data.table)
require(dplyr)

### set path to files
workdir = ""

### read data
setwd(workdir)

specs <- fread("species_links.txt", header = TRUE, sep = ",", dec = ".", quote = "", data.table = T)
specs <- as.data.frame(specs)
head(specs)
dim(specs)

traits <- fread("TRY_final.txt", header = T, sep = ",", dec = ".", quote = "", data.table = T)
traits <- as.data.frame(traits)
head(traits)
# traitID's: 
# 4 = SSD = stem specific density, 
# 14 = N = leaf nitrogen concentration, 
# 26 = SM = seed dry mass,
# 3106 = H = plant height, 
# 3110 = LA = leaf area, 
# 3117 = SLA = specific leaf area = 1/LMA


### join image dataset with trait data 

## sort datatable to speed up the following processes
specs <- specs[order(specs$species),]
traits <- traits[order(traits$AccSpeciesName),]

## subset dataframe "links" using the species names from dataframe "traits"
specs <- subset(specs, species %in% traits$AccSpeciesName)

## rename species column in "traits" dataframe to enable join
colnames(traits)[1] <- "species"

## join trait values with download links
specs_full <- left_join(specs, traits, by = "species")
head(specs_full)
dim(specs_full)

# as a reminder:
# traits: 4 = SSD = stem specific density, 14 = N = leaf nitrogen content, 26 = SM = seed dry mass,
# 3106 = H = plant height, 3110 = LA = leaf area, 3117 = SLA = specific leaf area = 1/LMA

# delete some columns that are unnecessary now
specs_full <- specs_full[, -c(1)]
head(specs_full)


##### exclude images without climate data
specs_full <- specs_full[!is.na(specs_full$bio1), ]

##### assign unique identifier for each image
uniqID <- sprintf("%07d", 1:nrow(specs_full))
specs_full <- cbind(specs_full, uniqID)

# write to disk, continue with script "4_download_images.R"
fwrite(specs_full, file = "Dat_full_ready_for_sampling.txt", col.names = TRUE)











