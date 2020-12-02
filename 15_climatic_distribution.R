##### this script produces the climatic distribution plots showing biomes and observations
##### input is the corresponding data table with the metadata, output is the plot


### load libraries
library(data.table)
library(sp)
library(sf)
library(plotbiomes)
library(raster)
library(maptools)


### set paths
workdir <- ""
path_data <- ""
path_results <- ""

### read data
setwd(workdir)

##### read data
dat <- fread(paste0(workdir, path_data, "metadata.txt"), header = TRUE)
head(dat)


dat_sp <- SpatialPoints(dat[, c(17, 31)]) # precipitation in mm and temperature in degree Celsius


####### prepare whittaker biomes

# data is stored in:
Whittaker_biomes_mm <- Whittaker_biomes
Whittaker_biomes_mm$precp_cm <- Whittaker_biomes$precp_cm * 10

### combine desert polygons
desert1 <- SpatialPolygons(list(
  Polygons(list(Polygon(Whittaker_biomes_mm[(Whittaker_biomes_mm$biome_id == 2), c(2, 1)])), ID = "Subtropical desert")
))
desert2 <- SpatialPolygons(list(
  Polygons(list(Polygon(Whittaker_biomes_mm[(Whittaker_biomes_mm$biome_id == 8), c(2, 1)])), ID = "Temperate grassland/desert")
))

# union
desert_union <- raster::union(desert1, desert2)
# dissolve
desert_dissolve <- aggregate(desert_union)

# create new list of polygons
whittaker_poly = SpatialPolygons(list(
  Polygons(list(Polygon(Whittaker_biomes_mm[(Whittaker_biomes_mm$biome_id == 1), c(2, 1)])), ID = "Tropical seasonal forest/savanna"), 
  Polygons(list(Polygon(Whittaker_biomes_mm[(Whittaker_biomes_mm$biome_id == 3), c(2, 1)])), ID = "Temperate rain forest"), 
  Polygons(list(Polygon(Whittaker_biomes_mm[(Whittaker_biomes_mm$biome_id == 4), c(2, 1)])), ID = "Tropical rain forest"), 
  Polygons(list(Polygon(Whittaker_biomes_mm[(Whittaker_biomes_mm$biome_id == 5), c(2, 1)])), ID = "Woodland/shrubland"), 
  Polygons(list(Polygon(Whittaker_biomes_mm[(Whittaker_biomes_mm$biome_id == 6), c(2, 1)])), ID = "Tundra"), 
  Polygons(list(Polygon(Whittaker_biomes_mm[(Whittaker_biomes_mm$biome_id == 7), c(2, 1)])), ID = "Boreal forest"), 
  Polygons(list(Polygon(Whittaker_biomes_mm[(Whittaker_biomes_mm$biome_id == 9), c(2, 1)])), ID = "Temperate seasonal forest")
))

# bind new polygons
whittaker_poly_full <- spRbind(whittaker_poly, desert_dissolve)

# define short labels
labels = c(
  "S/TrSF",
  "TeRF", 
  "TrRF", 
  "TeG", 
  "Tu", 
  "BF", 
  "TeSF", 
  "De")



### plotting
pdf(paste0(workdir, path_results, "Plot_climatic_distribution.pdf"), width=8, height=8)

par(mar = c(4, 4, 3, 1))

plot(1, type="n", xlim = c(0, 6200), ylab = "", xlab = "", ylim = rev(c(-18, 30)))
title(ylab="Mean annual temperatures (°C)", xlab = "Mean annual precipitation (mm)", line=2.5, cex.lab=1.2)
plot(dat_sp, add = TRUE, col = "darkgreen", pch = 20)
plot(whittaker_poly_full, add = TRUE, border = "gray30", lwd = 2)

# add text by hand, since automatic functions don't work nicely
text(x = 300, y = -8, labels = "Tu", font = 2, cex = 1.4)
text(x = 250, y = 18, labels = "De", font = 2, cex = 1.4)
text(x = 1500, y = 23.5, labels = "S/TrSF", font = 2, cex = 1.4)
text(x = 3100, y = 23.5, labels = "TrRF", font = 2, cex = 1.4)
text(x = 750, y = 14, labels = "TeG", font = 2, cex = 1.4)
text(x = 1650, y = 12, labels = "TeSF", font = 2, cex = 1.4)
text(x = 900, y = -1, labels = "BF", font = 2, cex = 1.4)
text(x = 2700, y = 18, labels = "TeRF", font = 2, cex = 1.4)
text(x = 5100, y = 23.5, labels = "Tropical", font = 2, cex = 1.4)
text(x = 4500, y = 12, labels = "Warm Temperate", font = 2, cex = 1.4)
text(x = 3400, y = -1, labels = "Cold Temperate", font = 2, cex = 1.4)
text(x = 2100, y = -10, labels = "Arctic Alpine", font = 2, cex = 1.4)
text(x = 3000, y = -22, labels = "Leaf area (LA)", font = 2, cex = 1.8, xpd = NA)

dev.off()
















