##### this script produces maps of the geographical distribution of the trait datasets
##### input is the corresponding data table, output is the world map with observations


# ### load libraries
library(data.table)
library(sp)
library(sf)
library(rnaturalearth)


### set paths
workdir <- ""
path_data = ""
path_results = ""


### read data
setwd(workdir)

# get land mass
land <- rnaturalearth::ne_countries(returnclass = "sp")

dat <- fread(paste0(workdir, path_data, "metadata.txt"), header = TRUE)

# create spatialpoints object
dat_sp <- SpatialPoints(coords = dat[, c(28, 27)], # select longitude, then latitude
                           proj4string = crs(land))

### plot observations
pdf(paste0(workdir, path_results, "Geographical_distribution.pdf"), width=8, height=4.5)

par(mar = c(0, 0, 2, 0))
plot(land, col = "gray70")
plot(dat_sp, pch = 20, col = "darkgreen", add = TRUE, cex = 0.7)
text(x = 0, y = 95, labels = "Leaf area (LA)", font = 2, cex = 1.9, xpd = NA)

dev.off()


 
