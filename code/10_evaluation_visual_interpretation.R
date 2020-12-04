##### in this script, the workflow for the evaluation of the images by visual interpretation is shown
##### input are images and metadata, 
##### part 1: output is a data table containing the interpretation results
##### part 2: output are the boxplots that visualise the results
##### part 3: output are the results of pairwise t-test for significant differences between groups shown in boxplot

### load libraries
library(scales)
library(ggsci)
library(rstatix)


### set paths
workdir = ""
imagedir = ""
path_results = ""


### set working directory
setwd(workdir)


############## part 1: visual interpretation #################

### read data
fls = list.files(path = paste0(workdir, imagedir), pattern = ".jpg")


### visual interpretation

### initialise list to be populated with the results of visual interpretation
eval_list.names <- c('pic_name', 'dist_to_obj', 'qual')
eval_list <- vector("list", length(eval_list.names))
names(eval_list) <- eval_list.names


### use loop to go through images consecutively and input the interpretation results
for (i in 1:length(fls))
{
  rast = stack(paste0(workdir, imagedir, "/", fls[i]))
  x11()
  plotRGB(rast)

  eval_list$pic_name[[i]] = fls[i]

  # ask for distance
  print(paste0("Name of image: ", fls[i], "; sample number: ", i))
  # distance to object
  eval_list$dist_to_obj[[i]] <- menu(c("< 1m", "1-5m", "> 5m"), title="What is the distance to the target species?")

  # ask for quality
  print(paste0("Name of image: ", fls[i], "; sample number: ", i))
  # image quality
  eval_list$qual[[i]] <- menu(c("hardly recognizable/strongly blurred", "medium/blurred", "high"), title="What is the image quality (concerning the target species)?")

  dev.off()
}

# convert to dataframe
eval_df <- as.data.frame(eval_list)

# save results to disk
fwrite(eval_df, file = paste0(workdir, path_results, "Results_visual_interpretation.txt"), col.names = TRUE)




########### part 2: produce boxplots ################

### read full dataset containing results of visual interpretation as well as growth form information and model predictions
dat <- read.csv(paste0(workdir, path_results, "eval_full.csv"), header = TRUE)

### combine plant functional types to growth forms 'woody' and 'non-woody'
dat$wood <- dat$gf_full
dat$wood <- as.character(dat$wood)
dat$wood[((dat$wood == "graminoid") | (dat$wood == "herb") | (dat$wood == "herb/shrub") | (dat$wood == "fern"))] <- "non-woody"
dat$wood[((dat$wood == "shrub") | (dat$wood == "tree") | (dat$wood == "shrub/tree"))] <- "woody"
dat$wood <- as.factor(dat$wood)
table(dat$wood)

# color scheme: nature publishing group
colors <- pal_npg("nrc")(10)

### plot the results as boxplots
pdf(paste0(workdir, path_results, "Boxplots_evaluation.pdf"), width=12, height=5)

layout(matrix(c(1, 1, 1, 2, 2, 3, 3, 3), nrow = 1, ncol = 8, byrow = TRUE))
par(mar = c(6,7,1,0), mgp=c(4.5,1.5,0))

## plot 1: image quality

boxplot(dat$mae ~ dat$qual, ylim = c(-0.02, 0.5), ylab = "Mean absolute error [unitless]", frame.plot = FALSE, 
        lty = 1, names = c("low", "medium", "high"), xlab = "Image quality", cex.axis = 2.5, cex.lab = 2.7, 
        boxfill = colors[2], boxlwd = 2, pch = 20) 

# add grid line
abline(h = seq(0, 0.5, 0.1), col = "grey")

# get grid lines to background
boxplot(dat$mae ~ dat$qual, ylim = c(-0.02, 0.5), ylab = "Mean absolute error [unitless]", frame.plot = FALSE, 
        lty = 1, names = c("low", "medium", "high"), xlab = "Image quality", cex.axis = 2.5, cex.lab = 2.7, 
        boxfill = colors[2], boxlwd = 2, pch = 20) 

# add label for plot
text(x = .5, y = .48, labels = "a)", cex = 2, font = 2)

# plot the frequencies below boxplot
freq <- aggregate(mae ~  qual, dat, length)
freq$mae <- freq$mae / sum(freq$mae) * 100
text(1:3, rep(min(dat$mae),3) - 0.02, paste0(round(freq$mae, 2), "%"), cex = 2.5)

# improve aesthetics
box(bty="c") # adds the lower axis (which has been removed before)

# add mean values to boxplot
means <- aggregate(mae ~  qual, dat, mean)
points(1:3, means$mae, col = "red", pch = 19)
text(1:3, means$mae + 0.015, labels = round(means$mae, 2), cex = 2.5)


## plot 2: growth form

par(mar = c(6, 0, 1, 0), xpd = TRUE, mgp=c(4.5,1.5,0))
boxplot(dat$mae ~ dat$wood, lty = 1, ylim = c(-0.02, 0.5), boxwex = 0.7, yaxt = "n", 
        ylab = "", frame.plot = FALSE, names = c("non-woody", "woody"), xlab = "Growth form", cex.axis = 2.5, cex.lab = 2.7, 
        boxfill = colors[9], boxlwd = 2, pch = 20) # boxfill = "gray70"

# add grid line
abline(h = seq(0, 0.5, 0.1), col = "grey")

# get grid lines to background
par(xpd = TRUE)
boxplot(dat$mae ~ dat$wood, lty = 1, ylim = c(-0.02, 0.5), boxwex = 0.7, yaxt = "n", 
        ylab = "", frame.plot = FALSE, names = c("non-woody", "woody"), xlab = "Growth form", cex.axis = 2.5, cex.lab = 2.7, 
        boxfill = colors[9], boxlwd = 2, pch = 20, add = TRUE)

# add label for plot
text(x = .6, y = .48, labels = "b)", cex = 2, font = 2)

# improve aesthetics
box(bty = "o", lty = 2)
abline(h=-0.0408, lwd = 1)
abline(h=.5208, lwd = 1)

# plot the frequencies below boxplot
freq <- aggregate(mae ~  wood, dat, length)
freq$mae <- freq$mae / sum(freq$mae) * 100
text(1:2, rep(min(dat$mae),3) - 0.02, paste0(round(freq$mae, 2), "%"), cex = 2.5) 

# add mean values to boxplot
means <- aggregate(mae ~  wood, dat, mean)
points(1:2, means$mae, col = "red", pch = 19)
text(1:2, means$mae + 0.015, labels = round(means$mae, 2), cex = 2.5)


## plot 3: image-target distance

par(mar = c(6, 0, 1, 1), xpd = FALSE, mgp=c(4.5,1.5,0)) 
dat$dist_to_obj <- as.character(dat$dist_to_obj)
boxplot(dat$mae ~ dat$dist_to_obj, yaxt = "n", ylab = "", frame.plot = FALSE, 
        lty = 1, ylim = c(-0.02, 0.5), names = c("<1m", "1-5m", ">5m"), 
        xlab = "Image-target distance", cex.axis = 2.5, cex.lab = 2.7, 
        boxfill = colors[1], boxlwd = 2, pch = 20) # , xaxt = "n" # boxfill = "grey70"


# add grid line
abline(h = seq(0, 0.5, 0.1), col = "grey")

# get grid lines to background
boxplot(dat$mae ~ dat$dist_to_obj, yaxt = "n", ylab = "", frame.plot = FALSE, 
        lty = 1, ylim = c(-0.02, 0.5), names = c("<1m", "1-5m", ">5m"), 
        xlab = "Distance to target species", cex.axis = 2.5, cex.lab = 2.7, 
        boxfill = colors[1], boxlwd = 2, pch = 20, add = TRUE) # , xaxt = "n"

# add label for plot
text(x = .5, y = .48, labels = "c)", cex = 2, font = 2)

# improve aesthetics
box(bty="]")

# plot the frequencies below boxplot
freq <- aggregate(mae ~  dist_to_obj, dat, length)
freq$mae <- freq$mae / sum(freq$mae) * 100
text(1:3, rep(min(dat$mae),3) - 0.02, paste0(round(freq$mae, 2), "%"), cex = 2.5) # second argument: position, third one: number to print

# add mean values to boxplot
means <- aggregate(mae ~  dist_to_obj, dat, mean)
points(1:3, means$mae, col = "red", pch = 19)
text(1:3, means$mae + 0.017, labels = round(means$mae, 2), cex = 2.5)
dev.off()


############ part 3: pairwise t-tests to check for significant differences between groups ##################

# Pairwise comparisons for image quality
pwc <- dat %>%
  pairwise_t_test(mae ~ qual, p.adjust.method = "bonferroni", detailed = TRUE)
pwc

# Pairwise comparisons for growth form
pwc <- dat %>%
  pairwise_t_test(mae ~ wood, p.adjust.method = "bonferroni", detailed = TRUE)
pwc

# pairwise comparisons for image-target distance
pwc <- dat %>%
  pairwise_t_test(mae ~ dist_to_obj, p.adjust.method = "bonferroni", detailed = TRUE)
pwc

