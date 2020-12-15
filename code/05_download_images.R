##### in this script, images referenced by url's are downloaded for the trait dataset
##### input is a dataframe (produced in script "4_sample_trait_dataset.R"), 
##### output is a folder with all the images as .jpg files and corresponding dataframe containing the metadata


### load libraries
require(data.table)

### set paths and parameters
workdir <- ""
outputfolder <- ""

### read data
setwd(workdir)

dir.create(outputfolder)

# replace with any other trait sample
leaf_area <- fread("leaf_area_sample_rdy_for_download.txt", header = TRUE, sep = ",", dec = ".", quote = "", data.table = T)
leaf_area <- as.data.frame(leaf_area)

# initialise column "pic_name" to reference the name of the .jpg-file in the metadata
leaf_area$pic_name <- matrix(NA, nrow(leaf_area), 1)


### download the data
for (i in (1:nrow(leaf_area)))
{
  # assign unique picture name
  leaf_area$pic_name[i] <- paste("LA_", sprintf("%06d", i), ".jpg", sep="")
  download.file(as.character(leaf_area$identifier[i]), 
                  destfile = paste(outputfolder, "LA_", sprintf("%06d", i), ".jpg", sep=""), 
                  mode = "wb")
}

# remove observations with failed downloads (= missing image in the image folder)
fls <- list.files(outputfolder, pattern = ".jpg")
leaf_area <- leaf_area[as.character(leaf_area$pic_name) %in% fls,]

# write to disk
fwrite(leaf_area, file = paste0(outputfolder, "metadata.txt"), col.names = TRUE)
# continue with script "6_image_processing.R"




