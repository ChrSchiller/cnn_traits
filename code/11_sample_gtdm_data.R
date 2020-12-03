##### this script shows the sampling process for the imagery 
##### used for producing the global trait distribution maps
##### input are observation records (e.g. the result of script "3_join_GBIF_TRY.R"), output is a data frame of image url's and metadata


### load packages
library(data.table)
library(raster)
library(sf)
set.seed(12345)

### set paths
workdir <- ""

### set workding directory
setwd(workdir)


### load helper function "stratified"
# function retrieved from https://gist.github.com/mrdwab/6424112
# credits to Ananda Mahto
source("stratified.R")


### read data
dat <- fread("Dat_full_ready_for_sampling.txt", header = TRUE)

## get land mass
land <- rnaturalearth::ne_countries(returnclass = "sp")
plot(land)

# get points including data
coords <- dat[, c(5, 4)] # longitude and latitude
dat <- dat[, c(2, 3, 6:25)] # download links, species name, climate data

# create spatialpointsdataframe
spdf <- SpatialPointsDataFrame(coords = coords, data = dat,
                               proj4string = crs(land))

# remove all the non-land pixels
spdf <- spdf[!is.na(over(spdf, as(land, "SpatialPolygons"))), ]

# convert spatialpointsdataframe
p = st_as_sf(spdf)
poly = st_as_sf(land)
allpoints <- st_intersection(p, poly)

# convert back to dataframe
allpoints_df <- as.data.frame(allpoints)

### stratified sampling, strata = "sovereignt" = countries
length(unique(allpoints_df$sovereignt))
dat_sample <- stratified(allpoints_df, "sovereignt", size = 2500)


# save to disk
fwrite(dat_sample, "GTDM_stratified_by_countries.txt", col.names = TRUE)
# continue with script "5_download_images.R" and the following scripts to download and process the images, and predict the plant functional trait values





