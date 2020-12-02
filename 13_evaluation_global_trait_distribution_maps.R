##### this script compares the global trait distribution maps of several other publications
##### inputs are the data of each publication and trait
##### outputs are the correlation plots


### load libraries
library(data.table)
library(corrplot)
library(raster)
library(sp)
library(sf)
library(rgdal)
library(Hmisc)

### set paths
workdir <- ""
path_data = ""


###### read all datasets ########
setwd(workdir)


########## read my own datasets #############

# la
la_schiller <- raster(paste0(workdir, path_data, "Rast_la.tif"))

# height
height_schiller <- raster(paste0(workdir, path_data, "Rast_gh.tif"))

# sla
sla_schiller <- raster(paste0(workdir, path_data, "Rast_sla.tif"))

# nm
nm_schiller <- raster(paste0(workdir, path_data, "Rast_lnc.tif"))

# sm
sm_schiller <- raster(paste0(workdir, path_data, "Rast_sm.tif"))

# ssd
ssd_schiller <- raster(paste0(workdir, path_data, "Rast_ssd.tif"))




################ van Bodegom et al. (2014) ####################

vanbodegom <- read.table(paste0(workdir, path_data, "output_traits_nozeroswise_lma_ssd_sm_snumlog.txt"))
names(vanbodegom) <- c("ID", "lon", "lat", "lma", "ssd", "sm")

gridded(vanbodegom) = ~lon+lat
crs(vanbodegom) <- crs(sla_schiller)
plot(vanbodegom)
dev.off()

lma_bode <- raster(vanbodegom, layer = 2)
ssd_bode <- raster(vanbodegom, layer = 3)
sm_bode <- raster(vanbodegom, layer = 4)
# conversion of units
lma_bode <- lma_bode^(-1) # now in m2/g
sm_bode <- 10^sm_bode



################# Boonman et al. (2020) ###############

### height
height_bm <- raster(paste0(workdir, path_data, "/Height.tif"))
height_bm <- 10^height_bm


### ssd
ssd_bm <- raster(paste0(workdir, path_data, "/Wood.density.tif"))


### sla
sla_bm <- raster(paste0(workdir, path_data, "/SLA.tif"))
sla_bm <- 10^sla_bm


### nmass
nmass_bm <- raster(paste0(workdir, path_data, "/Nmass.tif"))
nmass_bm <- 10^nmass_bm


################# Moreno-Martínez et al. (2018) #####################

### sla
sla_moreno <- raster(paste0(workdir, path_data, "/SLA_1km_v1.tif"))


### nmass
nmass_moreno <- raster(paste0(workdir, path_data, "/LNC_1km_v1.tif"))



################# Butler et al. (2017) #############################

### sla

sla_butler <- read.csv(paste0(workdir, path_data, "/sla.csv"), sep =",", dec = ".", header = TRUE)
sla_butler <- sla_butler[, c(1:3)]

gridded(sla_butler) = ~lon+lat
crs(sla_butler) <- crs("+proj=longlat +datum=WGS84")
plot(sla_butler)

sla_butler <- raster(sla_butler, layer = 1)
dev.off()


### nmass

nmass_butler <- read.csv(paste0(workdir, path_data, "/lnm.csv"), sep =",", dec = ".", header = TRUE)
nmass_butler <- nmass_butler[, c(1:3)]
gridded(nmass_butler) = ~lon+lat
crs(nmass_butler) <- crs("+proj=longlat +datum=WGS84")
nmass_butler <- raster(nmass_butler, layer = 1)
dev.off()



###################### resampling to same resolution #####################################


# van Bodegom
lma_bode <- resample(lma_bode, sla_schiller, method = "bilinear")
ssd_bode <- resample(ssd_bode, ssd_schiller, method = "bilinear")
sm_bode <- resample(sm_bode, sm_schiller, method = "bilinear")


# Boonman
height_bm <- resample(height_bm, height_schiller, method = "bilinear")
ssd_bm <- resample(ssd_bm, ssd_schiller, method = "bilinear")
sla_bm <- resample(sla_bm, sla_schiller, method = "bilinear")
nmass_bm <- resample(nmass_bm, nm_schiller, method = "bilinear")

# Moreno-Martínez
sla_moreno <- resample(sla_moreno, sla_schiller, method = "bilinear")
# raster cells that are off land mass seem to have negative values -> assign NA
sla_moreno[(sla_moreno < 0)] <- NA

nmass_moreno <- resample(nmass_moreno, nm_schiller, method = "bilinear")
# raster cells that are off land mass seem to have negative values -> assign NA
nmass_moreno[(nmass_moreno < 0)] <- NA

# Butler
sla_butler <- resample(sla_butler, sla_schiller, method = "bilinear")
nmass_butler <- resample(nmass_butler, nm_schiller, method = "bilinear")




######## correlation analysis for sla #####################

dat_sla <- cbind(getValues(sla_schiller), getValues(sla_bm), getValues(lma_bode), getValues(sla_butler), getValues(sla_moreno))
dat_sla <- as.data.frame(dat_sla)
names(dat_sla) <- c("Sc", "Bo", "vB", "Bu",  "MM")


# compute Pearson correlations and p-values
crplt <- rcorr(as.matrix(na.omit(dat_sla)), type = "pearson")
# make sure non-significant values are not shown in corrplot
mycol <- ifelse(c(crplt$P < 0.05), "black", "white")

# plotting
pdf(paste0(workdir, "/Corrplot_sla.pdf"), width = 14, height = 8, paper = "special")
corrplot(crplt$r, method = "circle", col = rev(heat.colors(100)),
         addCoef.col = mycol, tl.col = "black", sig.level = 0.05, tl.cex = 2.2, cl.cex = 2.4, number.cex = 2, cl.align.text = "l", 
         p.mat = crplt$P, insig = "blank")
dev.off()



############## correlation analysis for leaf nitrogen content ###################


dat_nmass <- cbind(getValues(nm_schiller), getValues(nmass_bm), getValues(nmass_butler), getValues(nmass_moreno))
dat_nmass <- as.data.frame(dat_nmass)
names(dat_nmass) <- c("Sc", "Bo", "Bu", "MM")


# compute Pearson correlations and p-values
crplt <- rcorr(as.matrix(na.omit(dat_nmass)), type = "pearson")
# make sure non-significant values are not shown in corrplot
mycol <- ifelse(c(crplt$P < 0.05), "black", "white")

# plotting
pdf(paste0(workdir, "/Corrplot_nm.pdf"), width = 14, height = 8, paper = "special")
corrplot(crplt$r, method = "circle", col = rev(heat.colors(100)),
         addCoef.col = mycol, tl.col = "black", sig.level = 0.05, tl.cex = 2.2, cl.cex = 2.4, number.cex = 2, cl.align.text = "l", 
         p.mat = crplt$P, insig = "blank")
dev.off()



############## correlation analysis for stem specific density ###################

dat_ssd <- cbind(getValues(ssd_schiller), getValues(ssd_bm), getValues(ssd_bode))
dat_ssd <- as.data.frame(dat_ssd)
names(dat_ssd) <- c("Sc", "Bo", "vB")


# compute Pearson correlations and p-values
crplt <- rcorr(as.matrix(na.omit(dat_ssd)), type = "pearson")
# make sure non-significant values are not shown in corrplot
mycol <- ifelse(c(crplt$P < 0.05), "black", "white")

# plotting
pdf(paste0(workdir, "/Corrplot_ssd.pdf"), width = 14, height = 8, paper = "special")
corrplot(crplt$r, method = "circle", col = rev(heat.colors(100)), 
         addCoef.col = mycol, tl.col = "black", sig.level = 0.05, tl.cex = 2.2, cl.cex = 2.4, number.cex = 2, cl.align.text = "l", 
         p.mat = crplt$P, insig = "blank")
dev.off()



############## correlation analysis for seed mass ###################


dat_sm <- cbind(getValues(sm_schiller), getValues(sm_bode))
dat_sm <- as.data.frame(dat_sm)
names(dat_sm) <- c("Sc", "vB")


# compute Pearson correlations and p-values
crplt <- rcorr(as.matrix(na.omit(dat_sm)), type = "pearson")
# make sure non-significant values are not shown in corrplot
mycol = ifelse(crplt$P <= 0.05, "black", "white")

# plotting
pdf(paste0(workdir, "/Corrplot_sm.pdf"), width = 14, height = 8, paper = "special")
corrplot(crplt$r, method = "circle", col = rev(heat.colors(100)), # order = "hclust", addrect = 2, 
         addCoef.col = mycol, tl.col = "black", sig.level = 0.05, tl.cex = 2.2, cl.cex = 2.4, number.cex = 2, cl.align.text = "l", 
         p.mat = crplt$P, insig = "blank")
dev.off()



############## correlation analysis for growth height ###################

dat_height <- cbind(getValues(height_schiller), getValues(height_bm))
dat_height <- as.data.frame(dat_height)
names(dat_height) <- c("Sc", "Bo")


# compute Pearson correlations and p-values
crplt <- rcorr(as.matrix(na.omit(dat_height)), type = "pearson")
# make sure non-significant values are not shown in corrplot
mycol = ifelse(crplt$P <= 0.05, "black", "white")

# plotting
pdf(paste0(workdir, "/Corrplot_height.pdf"), width = 14, height = 8, paper = "special")
corrplot(crplt$r, method = "circle", col = rev(heat.colors(100)),
         addCoef.col = mycol, tl.col = "black", sig.level = 0.05, tl.cex = 2.2, cl.cex = 2.4, number.cex = 2, cl.align.text = "l", 
         p.mat = crplt$P, insig = "blank")
dev.off()










