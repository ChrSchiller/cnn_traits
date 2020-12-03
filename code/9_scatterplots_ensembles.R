##### in this script, the results of the 3-fold cross-validation are plotted
##### input are the Ensemble predictions for all 3 model runs

### load packages
require(data.table)
library(scales)
library(ggsci)

### set paths
path_results = ""
workdir = ""

### read data
setwd(workdir)

la <- fread(paste0(workdir, path_results, "Ensemble_data_leaf_area.txt"), header = TRUE)
colors <- pal_npg("nrc")(6)


### plotting
par(mar = c(6, 6, 2, 2))


H <- Hpi(x=la[, c(2, 8)])      # estimate optimal bandwidth, column 2 = reference values, column 8 = ensemble predictions
est<- kde(x=la[, c(2, 8)], H=H, compute.cont=TRUE)     # estimate kernel density
# prepare drawing of contour levels
cl<-contourLevels(est, prob=c(0.5, 0.05), approx=TRUE)

plot(la$pred_ensemble ~ la$ref, ylim=c(-0.01, 1.01), xlim=c(-0.01, 1.01), asp = 1, cex.axis = 1, las = 1, 
     col = alpha(colors[1], 0.1), xlab = "", ylab = "", pch = 16)
mtext("Targets", cex=1.2, side=1, line=3)
mtext("Predictions", cex=1.2, side=2, line=3)
text(0.5, 1.1, labels = "Leaf area", font = 2, cex = 2, xpd = NA)
rug(la$ref, lwd = 0.2, ticksize = 0.015)
rug(la$pred_ensemble, side = 2, lwd = 0.2, ticksize = 0.015)
abline(a = 0, b = 1, col = "gray50", lty = 2)
plot(est,abs.cont=cl[1], labels=c(0.5),labcex=1, add=TRUE, lwd=0.8, col="grey10")
plot(est,abs.cont=cl[2], labels=c(0.95),labcex=1, add=TRUE, lwd=0.8, col="grey40")
nmae <- mean(abs(la$ref - la$pred_ensemble))/ (max(la$ref) - min(la$ref)) * 100
fit <- lm(la$pred_ensemble ~ la$ref_xcept)
cf <- round(coef(fit), 4) 

## printing NMAE and R2
mtext(paste0("NMAE = ", round(nmae, 2), " %"), 3, line=-1.5, cex = 1, adj = 0, at = 0)
mtext(paste0("R² = ", round(summary(fit)$r.squared, 4)), 3, line=-3, cex = 1, adj = 0, at = 0)












