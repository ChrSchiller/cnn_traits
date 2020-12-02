##### this R-script was used to preprocess the raw data from GBIF database 
##### containing plant photographs from iNaturalist
##### additionally, worldclim data is added using the geoloactions of the observations
##### the result of this script is a dataset containing species names 
##### and corresponding download links, climate data and coordinates



### load libraries
require(data.table)
library(dplyr)
require(raster)



### set paths to data files
workdir <- ""

### read data
setwd(workdir)

## dataset containing download links
# "identifier" contains the relevant url for the image
images <- fread("multimedia.txt", header = T, sep = "\t", dec = ".", quote = "", data.table = T, select = c("gbifID", "identifier"))
head(images)

## obtain occurrence data including coordinate and metadata
specs <- fread("occurrence.txt", header = T, sep = "\t", dec = ".", quote = "", data.table = T, 
                 select = c("gbifID", "species", "license", "hasCoordinate", "decimalLatitude", "decimalLongitude", 
                              "coordinateUncertaintyInMeters", "hasGeospatialIssues", "issue"))
head(specs)

### add image URL's to occurrence data by column "gbifID"
specs <- left_join(specs, images, by = "gbifID")
head(specs)

### remove observations with certain issues

## remove observations with known geospatial issues
specs <- as.data.frame(specs)
specs <- specs[(specs$hasGeospatialIssues == FALSE), ]

## remove observations without geolocation
specs <- specs[(specs$hasCoordinate == TRUE), ]

## remove observations with unclear taxonomic identification
specs <- filter(specs, !grepl("TAXON_MATCH_HIGHERRANK", issue))

# remove observations with issue "coordinate uncertainty meters invalid"
specs <- filter(specs, !grepl("COORDINATE_UNCERTAINTY_METERS_INVALID", issue))

# remove observations with issue "PRESUMED_NEGATED_LONGITUDE"
specs <- filter(specs, !grepl("PRESUMED_NEGATED_LONGITUDE", issue))

# remove observations with issue "PRESUMED_NEGATED_LATITUDE"
specs <- filter(specs, !grepl("PRESUMED_NEGATED_LATITUDE", issue))

# remove observations with issue "PRESUMED_SWAPPED_COORDINATE"
specs <- filter(specs, !grepl("PRESUMED_SWAPPED_COORDINATE", issue))

# remove observations with coordinate uncertainty greater than 100 km
specs <- filter(specs, (coordinateUncertaintyInMeters <= 100000))

# some columns are obsolete now and can be removed
specs <- specs[, c(1:3, 5:7, 10)]

### get worldclim data
clim <- getData("worldclim", var = "bio", res = 10)

# use only bio1, bio4, bio7, bio12, bio13, bio14, bio15
clim <- clim[[c(1, 4, 7, 12, 13, 14, 15)]]

# get coordinates for climate data retrieval (geolocations are given in WGS84 crs)
coords <- data.frame(x = specs$decimalLongitude, y = specs$decimalLatitude)

# create spatial points object using those coordinates
points <- SpatialPoints(coords, proj4string= clim@crs)

# get values for point coordinates of observations
values <- extract(clim, points)

# join climate data with observations
specs <- cbind.data.frame(specs, values)

### save to disk, continue with script "3_Join_GBIF_TRY.R"
fwrite(species, file = "species_links.txt", col.names = TRUE)




