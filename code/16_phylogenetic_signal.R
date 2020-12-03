##### this script contains the analysis of phylogenetic autocorrelation 
##### for one of the trait datasets with 200 images
##### input is the metadata containing predictions, reference values and taxa
##### as well as the metadata from GBIF/iNaturalist containing phylogenetic information on taxa
##### output is the pylogenetic tree with error bars

### load libraries
library(tidyverse)
library(data.table)
library(phylobase)
library(adephylo)
library(phylosignal)
library(phytools)
set.seed(123)

### set paths
workdir = ""
path_results = ""


### read data
setwd(workdir)

dat <- read.csv(paste0(workdir, path_results, "eval_full.csv"), header = TRUE)

### aggregate data on species level by mean values
agg_height <- aggregate(height[, c(1, 3)], FUN = mean, by = list(height$species))
names(agg_height)[1] <- "species"
names(agg_height)[2] <- "ref"
# compute mae
agg_height$mae <- abs(agg_height$pred_ensemble - agg_height$ref)

### load GBIF dataset containing phylogenetic information
treedata <- fread("gbif_darwin/occurrence.txt", header = T, sep = "\t", dec = ".", quote = "", data.table = T,
                                                select = c("species", "phylum", "class", "order", "family", "genus", "kingdom"))
treedata <- (treedata %>% distinct(species, .keep_all = TRUE))


### join the dataset with taxonomic information
dat_full <- left_join(agg_height, treedata, by = "species")


### convert taxonomic information to factors
dat_full$species <- as.factor(dat_full$species)
dat_full$genus <- as.factor(dat_full$genus)
dat_full$family <- as.factor(dat_full$family)
dat_full$order <- as.factor(dat_full$order)
dat_full$class <- as.factor(dat_full$class)
dat_full$phylum <- as.factor(dat_full$phylum)
dat_full$kingdom <- as.factor(dat_full$kingdom)

# extract mae information to add to phylo4d object
dat_full_nums <- as.data.frame(dat_full[, c("mae")])
rownames(dat_full_nums) <- dat_full$species
names(dat_full_nums)[1] <- "mae"

# prepare phylo object
frm <- ~kingdom/phylum/class/order/family/genus/species
tr <- as.phylo(frm, data = dat_full)

# compute branch lengths
tr <- compute.brlen(tr)

# root the phylogenetic tree
tr$root.edge <- 0
is.rooted(tr)

# prepare phylo4d object
tree_data <- phylo4d(tr, dat_full_nums) 

# check for phylogenetic signals
phyloSignal(tree_data)

### plot the result
pdf(paste0(workdir, path_results, "Phylotree.pdf"), height = 20, width = 20)
barplot(tree_data, tree.type = "fan", tip.cex = 0.6, show.trait = FALSE, tree.ratio = 0.5, scale = FALSE, center = FALSE, show.tip = FALSE)
dev.off()




















