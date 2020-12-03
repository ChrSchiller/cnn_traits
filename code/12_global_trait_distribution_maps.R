##### this script produces the global trait distribution map on the example of leaf area
##### input is a table containing predictions and geolocations
##### outputs are geotif files of the resulting raster files as well as a pdf file


### load libraries
library(data.table)
library(raster)
library(sp)
library(sf)
library(rasterVis)
library(gstat)
library(latticeExtra)
library(viridis)
library(RColorBrewer)



### set paths
workdir <- ""
path_results <- ""


### load data
setwd(workdir)


dat <- fread(paste0(workdir, path_results, "Ensemble_full.txt"), header = TRUE)
head(dat)
nrow(dat)

# get land mass from natural earth project
land <- rnaturalearth::ne_countries(returnclass = "sp")
plot(land)

# get raster grid with extent of land mass sp object
rast <- raster(xmn= -180, ymn= -90, xmx = 180, ymx = 90, resolution = 0.5,
               crs = crs(land))
# set values to 0
rast[] <- 0


# retransform the predicted values to original scale
dat <- as.data.frame(dat)

10^(3.797349 * (log10(3.797349) - log10(3.797349)) + log10(3.797349))

# leaf area
dat$pred_ensemble <- 10^(dat$pred_ensemble * (log10(226971) - log10(3.797349)) + log10(3.797349))

## get points including predictions
# coordinates (longitude and latitude data)
coords <- dat[, c(13, 12)]
## predictions
preds <- dat$pred_ensemble

spdf <- SpatialPointsDataFrame(coords = coords, data = dat,
                               proj4string = crs(land))


# exclude points that do not fall within land mass
spdf2 <- spdf[!is.na(over(spdf, as(land, "SpatialPolygons"))), ]


### inverse-distance weighted interpolation for whole land mass
gs <- gstat(formula=pred_ensemble~1, locations=coordinates(spdf2), data = spdf2, nmax=5, set=list(idp = 0))
nn <- interpolate(rast, gs)

### mask with land mass
nnmsk <- mask(nn, land)
nnmsk@crs <- crs("+proj=longlat +datum=WGS84")

### save raster containing interpolations to disk
writeRaster(nnmsk, paste0(workdir, path_results, "Rast_idw.tif"), format = "GTiff", overwrite = TRUE)

### apply buffer of 100 km radius around ech data point
bff <- buffer(spdf2, width = 100000)

### apply buffer as mask
inv_bff <- mask(nnmsk, bff, inverse = FALSE)
inv_bff@crs <- crs("+proj=longlat +datum=WGS84")

### save results
writeRaster(inv_bff, paste0(workdir, path_results, "Rast_buffered"), format = "GTiff")



### compute quantile range plots

# 90th quantile
quant90 <- raster::rasterize(spdf2, rast, 'pred_ensemble', fun= function(x, ...){quantile(x, .9)}) 


# 10th quantile
quant10 <- raster::rasterize(spdf2, rast, 'pred_ensemble', fun= function(x, ...){quantile(x, .1)}) 


### compute quantile range
rast_range_quant <- quant90 - quant10
rast_range_quant@crs <- crs("+proj=longlat +datum=WGS84")

# save to disk
writeRaster(rast_range_quant, paste0(workdir, path_results, "Rast_quantile_range"), format = "GTiff", overwrite = TRUE)


### levelplots of results

## idw interpolated map

pdf(paste0(workdir, path_results, "Levelplot.pdf"), height = 8, width = 10, paper = 'special')
nlev <- 15
# break points for color scale
my.at <- seq(from = cellStats(inv_bff, "min"),
             to = cellStats(inv_bff, "max"),
             length.out = nlev+1)
# colors
my.cols <- rev(viridis(nlev+5))[4:(nlev+5)]
# levelplot
plot_la <- levelplot(inv_bff, ylim=c(-60, 90), ylab = "", xlab = list("", cex = 1.4), main = list(expression(paste("Leaf area (mm"^"2", ", log10)")), cex = 1.6, font = 2),
                     zscaleLog = 10, #  log-scale due to wide range of distribution
                     scales=list(x=list(cex=1.3),y=list(cex=1.3)), colorkey = list(labels=list(cex=1.3)),
                     col.regions = my.cols, par.settings=list(panel.background=list(col="gray80")), 
                     margin=list(axis = list(col = "black", fontsize = 10)))
# latitudinal distribution only
plot_la$legend$top <- NULL
# add land mass and abline at equator
plot_la <- plot_la + latticeExtra::layer_(sp.polygons(land, col="gray10", fill = "white", lwd=0.5)) # layer under (!) levelplot
plot_la <- plot_la + latticeExtra::layer(sp.polygons(land, col="gray10", fill = "transparent", lwd=0.5)) # plot only borders above (!) levelplot
plot_la <- plot_la + latticeExtra::layer(panel.abline(h = 0, lty = 2, alpha = 0.5, col = "gray40"))
plot_la
dev.off()



## quantile range map

pdf(paste0(workdir, path_results, "Levelplot_quantile_range.pdf"), height = 8, width = 10, paper = 'special')
nlev <- 15
# break points for color scale
my.at <- seq(from = cellStats(rast_range_quant, "min"),
             to = cellStats(rast_range_quant, "max"),
             length.out = nlev+1)
# color scale
my.cols <- colorRampPalette(brewer.pal(9, "GnBu"))(nlev+7)[6:(nlev+7)]

# levelplot
plot_la_range <- levelplot(rast_range_quant, ylim=c(-60, 90), ylab = "", xlab = "", main = list(expression(paste("QR: Leaf area (mm"^"2", ", log10)")), cex = 1.6, font = 2),
                           zscaleLog = 10, #  at = my.at,
                           scales=list(x=list(cex=1.3),y=list(cex=1.3)), colorkey = list(labels=list(cex=1.3)),
                           col.regions = my.cols, par.settings=list(panel.background=list(col="gray80")), 
                           margin=list(axis = list(col = "black", fontsize = 10)))
# latitudinal distribution only
plot_la_range$legend$top <- NULL
# add land mass and abline at equator
plot_la_range <- plot_la_range + latticeExtra::layer_(sp.polygons(land, col="gray10", fill = "white", lwd=0.5)) # layer under (!) levelplot
plot_la_range <- plot_la_range + latticeExtra::layer(sp.polygons(land, col="gray10", fill = "transparent", lwd=0.5)) # plot only borders above (!) levelplot
plot_la_range <- plot_la_range + latticeExtra::layer(panel.abline(h = 0, lty = 2, alpha = 0.5, col = "gray40"))
plot_la_range
dev.off()





