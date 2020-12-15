##### in this script, the results on all setups are plotted
##### input are the R-squared and mean absolute error values of each setup for each plant functional trait


### load libraries
library(ggsci)


### set paths
workdir = ""
path_data = ""


### read data
setwd(workdir)
dat <- read.csv(paste0(path_data, "Results_4setups.csv"), sep = ";", dec = ".", header = TRUE)

### divide values by test range to yield normalised MAE (NMAE)
dat$test_mae_pc <- dat$test_mae
dat$test_mae_pc[5:8] <- dat$test_mae[5:8] / (0.970477 - 0.06938291)
dat$test_mae_pc[13:16] <- dat$test_mae[13:16] / (0.9573911 - 0.01153836)
dat$test_mae_pc[17:20] <- dat$test_mae[17:20] / (0.98712 - 0.04378579)
dat$test_mae_pc[21:24] <- dat$test_mae[21:24] / (0.9097961 - 0.006506015)

# round the results
dat$test_mae_pc <- round(dat$test_mae_pc, 2)
dat$r2 <- round(dat$r2, 4)


### save pdf to disk
pdf(paste0(path_data, "Lineplot_final.pdf"), width=14, height=6)

m <- matrix(c(1, 1, 1, 2, 2, 2, 3), nrow = 1, ncol = 7,byrow = TRUE)
layout(m, heights = c(0.4, 0.4, 0.2))

# get the range for the x and y axis
xrange <- range(dat$step)
dat$test_mae_pc <- dat$test_mae_pc * 100
yrange <- range(dat$test_mae_pc)

# color scheme: nature publishing group
colors <- pal_npg("nrc")(length(unique(dat$trait)))

# first plot
par(mar = c(4, 6, 3, 3))
plot(xrange, yrange, type="n", xaxt = "n", xlab = "", 
     ylab= "Normalised MAE (%)", cex.axis = 2.4, cex.lab = 2.5)
abline(h = seq(10, 13, 1), col = "grey")

axis(1, at=1:4, labels=c("Baseline", "Plasticity", "Bioclim", "Ensemble"), cex.axis = 2.5, mgp = c(.5, 1.8, 0)) # mgp moves axis tick labels away from axis

# add lines
for (i in 1:length(unique(dat$trait))) {
  number <- subset(dat, trait==unique(dat$trait)[i])
  lines(number$step, number$test_mae_pc, type="b", lwd=4,
        lty=1, col=colors[i], pch=20)
}


### second plot: R-squared

# get the range for the x and y axis
yrange2 <- range(dat$r2)

par(mar = c(4, 6, 3, 3))

plot(xrange, yrange2, type="n", xlab = "",
     ylab=parse(text='R²'), xaxt = "n", cex.axis = 2.4, cex.lab = 2.5, ylim = c(0, 0.6))
axis(1, at=1:4, labels=c("Baseline", "Plasticity", "Bioclim", "Ensemble"), cex.axis = 2.5, mgp = c(.5, 1.8, 0))
abline(h = seq(0, 0.6, 0.1), col = "grey")

# add lines
for (i in 1:length(unique(dat$trait))) {
  number <- subset(dat, trait==unique(dat$trait)[i])
  lines(number$step, number$r2, type="b", lwd=4,
        lty=1, col=colors[i], pch=20)
}

text(x = 0.5, y = 0.63, labels = "b)", xpd = NA, cex = 2.6, font = 2)
text(x = -3.5, y = 0.63, labels = "a)", xpd = NA, cex = 2.6, font = 2)

### add legend
par(mar=c(0, 0, 0, 0.2))

plot(1, type = "n", axes=FALSE, xlab="", ylab="")

legend(x = "center", inset = 0, legend = c("LA", "GH", "SLA", "NM", "SM", "SSD"), cex=3, col=colors, # , legend = unique(dat$trait)
       pch=20, lty=1, title="Trait") # , horiz = TRUE # , ncol = 6

dev.off()

















