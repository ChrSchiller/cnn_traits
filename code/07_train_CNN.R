##### in this script, the CNN models are trained
##### input is the metadata from script "6_image_processing.R" and the corresponding image data
##### all setups are considered in this modular script
##### the code for each setup is clearly marked, the TA and Bioclim setups can be added if desired


### libraries
library(NCmisc)
library(keras)
library(tensorflow)
library(tfdatasets)
library(tidyverse)
library(tibble)
library(rsample)
library(data.table)
set.seed(123)

### helper function
# normalisation
range01 <- function(x, min, max){(x - min)/(max - min)}

# function to adjust a variable randomly within a certain specified range, making use of the truncated normal distribution
truncatedsd <- function(mn, dev)
{
  rtruncnorm(1, a = (mn - dev), 
             b = (mn + dev), 
             mean = mn, sd = dev)
}


### set up tensorflow
tf$compat$v1$set_random_seed(as.integer(28))

# set memory growth policy
gpu1 <- tf$config$experimental$get_visible_devices('GPU')[[1]]
gpu2 <- tf$config$experimental$get_visible_devices('GPU')[[2]]
tf$config$experimental$set_memory_growth(device = gpu1, enable = TRUE)
tf$config$experimental$set_memory_growth(device = gpu2, enable = TRUE)
strategy <- tf$distribute$MirroredStrategy()
strategy$num_replicas_in_sync


### set paths (to corresponding trait dataset) and parameters
workdir = ""
path_img = ""
path_img = ""
path_ref = ""
outdir = ""
xres = 512
yres = 512
no_bands = 3

### set working directory and output directory
setwd(workdir)
dir.create(paste0(outdir), recursive = TRUE)



### read data

## reference
ref = read.table(paste0(workdir, path_ref), row.names=NULL, sep = ",", header = TRUE)

## list all img  data
path_img = list.files(paste0(workdir, path_img), full.names = T, pattern = "jpg")

# combine image data with target value data
dat <- cbind(path_img, ref)

### remove outliers
dat$ref <- log10(dat$ref)
outl <- which.outlier(dat$ref, thr = 3, method = "sd")
dat <- dat[-outl,]

### split test dataset from training/validation dataset
testIdx <- sample(x = 1:nrow(dat), size = floor(nrow(dat)/10), replace = F)
test_dat <- dat[testIdx, ]
test_img <- dat$path_img[testIdx]
test_ref <- dat$ref[testIdx]
dat <- dat[-testIdx, ]

### split training and validation data
valIdx <- sample(x = 1:nrow(dat), size = floor(nrow(dat)/5), replace = F)
val_dat <- dat[valIdx, ]
val_img <- dat$path_img[valIdx]
val_ref <- dat$ref[valIdx]
train_dat <- dat[-valIdx, ]
# train_dat contains the remaining training dataset


### save min and max values of original targets (without outliers) for succeeding normalisation
# they are needed to adjust the augmented targets to the same scale as validation and test data (in case of target augmentation)
min_ref <- min(train_dat$mean) # min of training (!) data
max_ref <- max(train_dat$mean) # max of training (!) data

### transform validation and test targets with the values from training data
val_ref <- range01_v2(val_ref, log10(min_ref), log10(max_ref))
test_ref <- range01_v2(test_ref, log10(min_ref), log10(max_ref))

### prepare training datasets
train_img = train_dat$path_img


### prepare training reference values
train_ref = range01_v2(train_dat$ref, log10(min_ref), log10(max_ref))

### prepare tibble
train_data = tibble(img = train_img, ref = train_ref)
val_data = tibble(img = val_img, ref = val_ref)


# save test data to disk
save(test_img, file = paste0(outdir, "test_img.RData"), overwrite = T)
save(test_ref, file = paste0(outdir, "test_ref.RData"), overwrite = T)



#############################################################
##### in case of TA setup, add the following code ###########
#############################################################

### target augmentation by truncated standard deviation
train_dat$mean_sd <- mapply(function(x, y) truncatedsd(x, y), train_dat$mean, train_dat$SD)

# in case of "NaN" (e.g. if SD == 0), take the value of mean of same row (no changes)
train_dat$mean_sd[is.nan(train_dat$mean_sd)] <- train_dat$mean[is.nan(train_dat$mean_sd)]
# in case SD = NA:
train_dat$mean_sd[is.na(train_dat$mean_sd)] <- train_dat$mean[is.na(train_dat$mean_sd)]
# in case we created negative values, allocate 0 to them:
train_dat$mean_sd[train_dat$mean_sd < 0] <- 0


### log-transformation of augmented targets (could not be done earlier, as the SD values are and should be in the original unit)
train_dat$mean_sd <- log10(train_dat$mean_sd)

# assign 0 to -Inf values (that were created by log10(0), which is not a meaningful value)
train_dat$mean_sd[train_dat$mean_sd == -Inf] <- 0

### adjust to approximately [0, 1]-range using the stored min_ref and max_ref values
# after this operation, the scale of validation/test data and training data is the same again (be aware of log10-transformation)
train_dat$mean_sd <- range01_v2(train_dat$mean_sd, min = log10(min_ref), max = log10(max_ref))

### in case the target augmentation resulted in values smaller than 0 and greater than 1, these should be assigned 0 and 1, respectively
# this procedure is equal to the data augmentation algorithm that is implemented as default
train_dat$mean_sd[train_dat$mean_sd < 0] <- 0
train_dat$mean_sd[train_dat$mean_sd > 1] <- 1

# overwrite train_ref (only if TA setup is used)
train_ref <- train_dat$mean_sd

### prepare tibble
train_data <- tibble(img = train_img, ref = train_ref)


#############################################################
##### in case of Bioclim setup, use the following code ######
##### (can be combined with TA code, see above) #############
#############################################################

# compute bio13 - bio14 (see Methods)
train_dat$bio1314 <- train_dat$bio13 - train_dat$bio14
val_dat$bio1314 <- val_dat$bio13 - val_dat$bio14
test_dat$bio1314 <- test_dat$bio13 - test_dat$bio14

# store min and max values for normalisation for each bioclim variable
# use only training dataset for computation of these parameters!
min_bio1 <- min(train_dat$bio1)
max_bio1 <- max(train_dat$bio1)
min_bio4 <- min(train_dat$bio4)
max_bio4 <- max(train_dat$bio4)
min_bio7 <- min(train_dat$bio7)
max_bio7 <- max(train_dat$bio7)
min_bio12 <- min(train_dat$bio12)
max_bio12 <- max(train_dat$bio12)
min_bio1314 <- min(train_dat$bio1314)
max_bio1314 <- max(train_dat$bio1314)
min_bio15 <- min(train_dat$bio15)
max_bio15 <- max(train_dat$bio15)

# normalise training data
train_dat$bio1 <- range01_v2(train_dat$bio1, min_bio1, max_bio1)
train_dat$bio4 <- range01_v2(train_dat$bio4, min_bio4, max_bio4)
train_dat$bio7 <- range01_v2(train_dat$bio7, min_bio7, max_bio7)
train_dat$bio12 <- range01_v2(train_dat$bio12, min_bio12, max_bio12)
train_dat$bio1314 <- range01_v2(train_dat$bio1314, min_bio1314, max_bio1314)
train_dat$bio15 <- range01_v2(train_dat$bio15, min_bio15, max_bio15)

# normalise validation data
val_dat$bio1 <- range01_v2(val_dat$bio1, min_bio1, max_bio1)
val_dat$bio4 <- range01_v2(val_dat$bio4, min_bio4, max_bio4)
val_dat$bio7 <- range01_v2(val_dat$bio7, min_bio7, max_bio7)
val_dat$bio12 <- range01_v2(val_dat$bio12, min_bio12, max_bio12)
val_dat$bio1314 <- range01_v2(val_dat$bio1314, min_bio1314, max_bio1314)
val_dat$bio15 <- range01_v2(val_dat$bio15, min_bio15, max_bio15)

# normalise test data
test_dat$bio1 <- range01_v2(test_dat$bio1, min_bio1, max_bio1)
test_dat$bio4 <- range01_v2(test_dat$bio4, min_bio4, max_bio4)
test_dat$bio7 <- range01_v2(test_dat$bio7, min_bio7, max_bio7)
test_dat$bio12 <- range01_v2(test_dat$bio12, min_bio12, max_bio12)
test_dat$bio1314 <- range01_v2(test_dat$bio1314, min_bio1314, max_bio1314)
test_dat$bio15 <- range01_v2(test_dat$bio15, min_bio15, max_bio15)


# prepare dataframes for auxiliary data input to CNN for validation and test datasets
train_aux <- subset(train_dat, select = c("bio1", "bio12", "bio4", "bio7", "bio15", "bio1314"))
val_aux <- subset(val_dat, select = c("bio1", "bio12", "bio4", "bio7", "bio15", "bio1314"))
test_aux <- subset(test_dat, select = c("bio1", "bio12", "bio4", "bio7", "bio15", "bio1314"))

### prepare tibble (intentionally overwrite existing tibbles for Baseline and TA)
train_data = tibble(img = train_img, aux = train_aux, ref = train_ref)
val_data = tibble(img = val_img, aux = val_aux, ref = val_ref)

# save auxiliary (bioclimatic) test data to disk (image and reference data was saved already, see above)
save(test_aux, file = paste0(outdir, "test_aux.RData"), overwrite = T)



####################################################################################
##### use the following code as input pipeline of either Baseline or TA setup #####
####################################################################################

### tfdatasets input pipeline 
create_dataset <- function(data,
                           train, # logical. TRUE for augmentation of training data
                           batch, # numeric. multiplied by number of available gpus since batches will be split between gpus
                           epochs,
                           useDSM = FALSE, 
                           shuffle = TRUE, # logical. default TRUE, set FALSE for test data
                           dataset_size){ # numeric. number of samples per epoch the model will be trained on
  if(useDSM) chnl = 4L else chnl = 3L
  
  if(shuffle){
    dataset = data %>%
      tensor_slices_dataset() %>%
      dataset_shuffle(buffer_size = length(data$img), reshuffle_each_iteration = TRUE)
  } else {
    dataset = data %>%
      tensor_slices_dataset() 
  } 
  
  dataset = dataset %>%
    dataset_map(~.x %>% list_modify( # read files and decode jpg
      img = tf$image$decode_jpeg(tf$io$read_file(.x$img), channels = chnl)
    )) %>%
    dataset_map(~.x %>% list_modify(
      img = tf$image$convert_image_dtype(.x$img, dtype = tf$float32) %>% # convert datatype
        tf$squeeze() # removes dimensions of size 1 from the shape of a tensor
    )) %>%
    dataset_map(~.x %>% list_modify( # set shape to avoid error at fitting stage "tensor of unknown rank"
      img = tf$reshape(.x$img, shape = c(512L, 512L, chnl))
    ))
  
  if(train) {
    dataset = dataset %>%
      dataset_map(~.x %>% list_modify( # randomly flip up/down
        img = tf$image$random_flip_up_down(.x$img, seed = 1L)
        #msk = tf$image$random_flip_up_down(.x$msk, seed = 1L)
      )) %>%
      dataset_map(~.x %>% list_modify( # randomly flip left/right
        img = tf$image$random_flip_left_right(.x$img, seed = 1L)
        #msk = tf$image$random_flip_left_right(.x$msk, seed = 1L)
      )) %>%
      dataset_map(~.x %>% list_modify( # randomly assign brightness, contrast and saturation to images
        img = tf$image$random_brightness(.x$img, max_delta = 0.1, seed = 1L) %>%
          tf$image$random_contrast(lower = 0.9, upper = 1.1, seed = 2L) %>%
          tf$image$random_saturation(lower = 0.9, upper = 1.1, seed = 3L) %>% # requires 3 chnl -> with useDSM chnl = 4
          tf$clip_by_value(0, 1) # clip the values into [0,1] range.
      )) %>%
      # dataset_repeat(count = ceiling(epochs * (dataset_size/length(train_data$img))) )
      dataset_repeat()
  }
  
  
  dataset = dataset %>%
    dataset_batch(batch, drop_remainder = TRUE) %>%
    dataset_map(unname) %>%
    dataset_prefetch(buffer_size = tf$data$experimental$AUTOTUNE)
}


####################################################################################
##### use the following code as input pipeline of Bioclim setup ###################
####################################################################################




# inspired by: https://blogs.rstudio.com/ai/posts/2019-08-23-unet/
# and: https://blogs.rstudio.com/ai/posts/2019-10-08-tf2-whatchanges/
# and: https://tensorflow.rstudio.com/tutorials/beginners/load/load_image/
create_dataset <- function(data,
                           train, # logical. TRUE for augmentation of training data
                           batch, # numeric. multiplied by number of available gpus since batches will be split between gpus
                           epochs,
                           useDSM = FALSE, 
                           shuffle = TRUE, # logical. default TRUE, set FALSE for test data
                           dataset_size){ # numeric. number of samples per epoch the model will be trained on
  if(useDSM) chnl = 4L else chnl = 3L
  
  if(shuffle){
    dataset = data %>%
      tensor_slices_dataset() %>%
      dataset_shuffle(buffer_size = length(data$img), reshuffle_each_iteration = TRUE) 
  } else {
    dataset = data %>%
      tensor_slices_dataset()
  } 
  
  
  # this is just the transformation from image paths to actual images that will be fed to the model
  dataset = dataset %>%
    dataset_map(~.x %>% list_modify( # read files and decode jpeg
      img = tf$image$decode_jpeg(tf$io$read_file(.x$img), channels = chnl)
    )) %>%
    dataset_map(~.x %>% list_modify(
      img = tf$image$convert_image_dtype(.x$img, dtype = tf$float32) %>% # convert datatype
        tf$squeeze() # removes dimensions of size 1 from the shape of a tensor
    )) %>%
    dataset_map(~.x %>% list_modify( # set shape to avoid error at fitting stage "tensor of unknown rank"
      img = tf$reshape(.x$img, shape = c(512L, 512L, chnl))
    ))
  
  
  if(train) {
    dataset = dataset %>%
      dataset_map(~.x %>% list_modify( # randomly flip up/down
        img = tf$image$random_flip_up_down(.x$img, seed = 1L)
      )) %>%
      dataset_map(~.x %>% list_modify( # randomly flip left/right
        img = tf$image$random_flip_left_right(.x$img, seed = 1L)
      )) %>%
      dataset_map(~.x %>% list_modify( # randomly assign brightness, contrast and saturation to images
        img = tf$image$random_brightness(.x$img, max_delta = 0.1, seed = 1L) %>%
          tf$image$random_contrast(lower = 0.9, upper = 1.1, seed = 2L) %>%
          tf$image$random_saturation(lower = 0.9, upper = 1.1, seed = 3L) %>% # requires 3 chnl -> with useDSM chnl = 4
          tf$clip_by_value(0, 1) # clip the values into [0,1] range.
      ))#  %>%
  }

  dataset = dataset %>%
    dataset_prepare(x = c(img, aux), y = ref, named_features = TRUE, batch_size = batch, drop_remainder = TRUE) %>% 
    dataset_prefetch(buffer_size = tf$data$experimental$AUTOTUNE)
  
}





###################################################
##### this code should be used for all setups #####
###################################################

### set parameters 
batch_size <- 20 
epochs <- 50
dataset_size <- length(train_data$img)

### prepare dataset that can be input to CNN
training_dataset <- create_dataset(train_data, train = TRUE, batch = batch_size, epochs = epochs, dataset_size = dataset_size)
validation_dataset <- create_dataset(val_data, train = FALSE, batch = batch_size, epochs = epochs)

### check dataset loading
dataset_iter = reticulate::as_iterator(training_dataset)
train_example = dataset_iter %>% reticulate::iter_next()
train_example[[1]]
train_example[[2]]

dataset_iter = reticulate::as_iterator(validation_dataset)
val_example = dataset_iter %>% reticulate::iter_next()
val_example




#################################################
##### define model for Baseline or TA setup #####
#################################################


### choose one of three pre-defined base model architectures: Inception-Resnet-v2, Xception, MobileNet-v2
### Inception-Resnet-v2 was used for Baseline and TA in the study
with(strategy$scope(), {
  
  # base CNN model definition, initial weights should be downloaded automatically from www.image-net.org upon compiling
  base_model <- application_inception_resnet_v2(weights = 'imagenet',
                                     include_top = FALSE, input_shape = c(xres, yres, no_bands))
  # base_model <- application_xception(weights = 'imagenet',
  #                               include_top = FALSE, input_shape = c(xres, yres, no_bands))
  # base_model <- application_mobilenet_v2(weights = 'imagenet', alpha = 0.5,
  #                          include_top = FALSE, input_shape = c(xres, yres, no_bands))
  
  # add custom layers as regressor
  predictions <- base_model$output %>%
    layer_global_average_pooling_2d() %>%
    layer_dense(units = 512, activation = 'relu') %>%
    layer_dense(units = 1, activation = 'linear') 

  # set up the model
  model <- keras_model(inputs = base_model$input, outputs = predictions)

  # compile model
  model %>% compile(
    loss = "mse",
    optimizer = optimizer_rmsprop(0.001, decay = 0.0001),
    metrics = c("mean_absolute_error")
  )
})


#################################################
##### define model for Bioclim setup ##########
#################################################

### choose one of three pre-defined base model architectures: Inception-Resnet-v2, Xception, MobileNet-v2
### Inception-Resnet-v2 was used for Baseline and TA in the study

with(strategy$scope(), {
  
  ### input branch A:
  input_cnn <- layer_input(shape = c(xres, yres, no_bands))
  
  ### output branch A:
  base_cnn <- input_cnn %>%
    application_inception_resnet_v2(weights = 'imagenet',
                             include_top = FALSE, input_shape = c(xres, yres, no_bands))
    # application_xception(weights = 'imagenet',
    #                               include_top = FALSE, input_shape = c(xres, yres, no_bands))
    # application_mobilenet_v2(weights = 'imagenet', alpha = 0.5,
    #                          include_top = FALSE, input_shape = c(xres, yres, no_bands))
  
  output_cnn <- base_cnn$output %>% 
    layer_global_average_pooling_2d() %>%
    layer_dense(units = 512, activation = 'relu') %>%
    layer_dense(units = 4, activation = 'linear') 
  
  ### input branch B:
  input_dense <- layer_input(shape = c(ncol(train_data$aux)))

  # output of branch B
  output_dense <- input_dense %>%
    layer_dense(units = 64, activation = 'relu') %>%
    layer_dense(units = 32, activation = 'relu') %>%
    layer_dense(units = 4, activation = 'linear') 
  
  
  # concatenated branch
  main_output <- layer_concatenate(c(output_cnn, output_dense)) %>%
    layer_dense(units = 8, activation = 'relu') %>%
    layer_dense(units = 8, activation = 'relu') %>%
    layer_dense(units = 4, activation = 'relu') %>%
    layer_dense(units = 1, activation = 'linear') 
  
  # set up complete model
  model <- keras_model(
    inputs = c(input_cnn, input_dense),
    outputs = c(main_output)
  )
  
  
  # compile model
  model %>% compile(
    loss = "mse", # loss = "mse", 
    optimizer = optimizer_rmsprop(0.001, decay = 0.0001), 
    metrics = c("mean_absolute_error")
  )
})





##############################################
##### fit model for Baseline or TA setup #####
##############################################


checkpoint_dir <- paste0(workdir,outdir, "checkpoints")
unlink(checkpoint_dir, recursive = TRUE)
dir.create(checkpoint_dir, recursive = TRUE)
filepath = file.path(checkpoint_dir, "weights.{epoch:02d}-{val_loss:.5f}.hdf5")

cp_callback <- callback_model_checkpoint(filepath = filepath,
                                         monitor = "val_loss",
                                         save_weights_only = FALSE,
                                         save_best_only = FALSE,
                                         verbose = 1,
                                         mode = "auto",
                                         save_freq = "epoch")

history <- model %>% fit(x = training_dataset,
                         epochs = epochs,
                         steps_per_epoch = floor(length(train_data$img)/batch_size), 
                         callbacks = list(cp_callback, callback_terminate_on_naan()),
                         validation_data = validation_dataset)



#########################################
##### fit model for Bioclim setup #####
#########################################

train_steps <- floor(length(train_data$img) / batch_size) # number of batches the model is trained upon per epoch
val_steps <- floor(length(val_data$img) / batch_size) # number of batches fed to model for validation

# initialize empty list to populate in succeeding loop:
summary_list.names <- c('train_loss', 'train_mae', 'val_loss', 'val_mae')
summary_list <- vector("list", length(summary_list.names))
names(summary_list) <- summary_list.names


### full model fit
for (t in 1:epochs){ # for-loop that loops through all training and validation steps
  
  # initiate new set of batches
  iter_train <- make_iterator_one_shot(training_dataset)
  next_batch_train <- iterator_get_next(iter_train)
  iter_val <- make_iterator_one_shot(validation_dataset)
  next_batch_val <- iterator_get_next(iter_val)
  
  mean_train = list()
  for (i in 1:train_steps) { # loop is gone through 'steps' times
    
    # training
    history <- model %>% train_on_batch(x = list(next_batch_train$x$img, next_batch_train$x$aux), 
                                        y = next_batch_train$y)
    
    if (i %% 50 == 0){ # every 50 steps, print some information
      print('training step:')
      print(i)
      print('train_loss (mse), train_mean_absolute_error')
      print(history)
    }

    mean_train$loss[[i]] <- history[1]
    mean_train$mae[[i]] <- history[2]
    
    # if iterator_get_next is called in last step, error will be thrown
    # this is avoided by this if-condition
    if (i < train_steps)
    {
    # create new batch of training data:
    next_batch_train <- iterator_get_next(iter_train)
    } 
    
  }
  
  print(paste('average train_loss and train_mean_absolute_error of epoch', t))
  print(mean(unlist(mean_train$loss)))
  print(mean(unlist(mean_train$mae)))
  
  mean_val = list() 
  for (k in 1:val_steps){
    # check validation loss:
    history <- model %>% test_on_batch(x = list(next_batch_val$x$img, next_batch_val$x$aux), 
                                       y = next_batch_val$y)
    
    if (k %% 20 == 0){ # every 30 steps, print some information
      print('validation step:')
      print(k)
      print('val_loss (mse), val_mean_absolute_error')
      print(history)
    }
    
    mean_val$loss[[k]] <- history[1]
    mean_val$mae[[k]] <- history[2]
    
    # if iterator_get_next is called in last step, error will be thrown
    # this is avoided by this if-condition
    if (k < val_steps)
    {
    # create new batch of validation data:
    next_batch_val <- iterator_get_next(iter_val)
    }
  }
  
  print(paste('average val_loss and val_mean_absolute_error of epoch', t))
  print(mean(unlist(mean_val$loss)))
  print(mean(unlist(mean_val$mae)))
  
  # save history
  summary_list$train_loss[[t]] <- mean(unlist(mean_train$loss))
  summary_list$train_mae[[t]] <- mean(unlist(mean_train$mae))
  summary_list$val_loss[[t]] <- mean(unlist(mean_val$loss))
  summary_list$val_mae[[t]] <- mean(unlist(mean_val$mae))
  
  if (t == 1){
    # save model of first epoch
    save_model_hdf5(model,
                    paste0(workdir,outdir, "checkpoints/", "weights.epoch_", t, "-val_loss_", round(summary_list$val_loss[[t]], 4), ".hdf5"),
                    overwrite = TRUE)
    print(paste('Model of epoch', t, 'saved!'))
  }
  else{
    if (summary_list$val_loss[[t]] < summary_list$val_loss[[t-1]]){
      # save model of epoch in case the validation loss has been improved compared to preceding epoch
      save_model_hdf5(model,
                      paste0(workdir,outdir, "checkpoints/", "weights.epoch_", t, "-val_loss_", round(summary_list$val_loss[[t]], 4), ".hdf5"),
                      overwrite = TRUE) # , custom_objects=list("loss_function" = RMSE)
      print(paste('Model of epoch', t, 'saved!', 'val_mae is', round(summary_list$val_mae[[t]], 4)))
    }
  }
  
  # end of loop that denotes epochs
}


### plot results
summary_df <- as.data.frame(summary_list)
min(summary_df$val_mae)
summary_df

write.csv(summary_df, paste0(workdir, outdir, "History.csv"))




###############################################
#### evaluation for Baseline and TA setup #####
###############################################

checkpoint_dir <- paste0(workdir, outdir, "checkpoints/")
load(paste0(workdir, outdir, "test_img.RData"))
load(paste0(workdir, outdir, "test_ref.RData"))

# convert to tibble
test_data = tibble(img = test_img, ref = test_ref)

# prepare dataset that can be input to the CNN
test_dataset <- create_dataset(test_data, train = FALSE, batch = 1, shuffle = FALSE, useDSM = FALSE)

# load model (use meaningful "modelname.hdf5" file)
model = load_model_hdf5(paste0(checkpoint_dir, "modelname.hdf5"), compile = TRUE) 

# evaluate test dataset
eval <- evaluate(object = model, x = test_dataset)
eval

# make predictions
test_pred = predict(model, test_dataset)

# save predictions
test_pred_df <- as.data.frame(test_pred)
test_pred_df <- test_pred_df$V1

# allocate image names and reference data to predictions of test dataset for succeeding analysis
data_full <- cbind(as.character(test_img), test_ref, test_pred_df)
write.csv(data_full, paste0(workdir, outdir, "Test_results.csv"))







##########################################
##### evaluation for Bioclim setup #######
##########################################

### load test data
checkpoint_dir <- paste0(workdir, outdir, "checkpoints/")
load(paste0(workdir, outdir, "test_img.RData"))
load(paste0(workdir, outdir, "test_aux.RData"))
load(paste0(workdir, outdir, "test_ref.RData"))

# convert to tibble
test_data = tibble(img = test_img,
                   aux = test_aux,
                   ref = test_ref)

# prepare dataset that can be input to the CNN
test_dataset <- create_dataset(test_data, train = FALSE, batch = 1, shuffle = FALSE, useDSM = FALSE)

# load model
model = load_model_hdf5(paste0(checkpoint_dir, "modelname.hdf5"), compile = TRUE)


### evaluate model on test dataset

# check data loading
iter_test <- make_iterator_one_shot(test_dataset)
next_batch_test <- iterator_get_next(iter_test)
next_batch_test[[1]]

# define test steps
test_steps <- length(test_data$img) # number of batches fed to model for testing: number of data points, cause batch = 1 (no batches needed)


mean_test = list() 
test_pred <- list()
for (j in 1:test_steps){
  # check validation loss:
  history <- model %>% test_on_batch(x = list(next_batch_test$x$img, next_batch_test$x$aux), 
                                     y = next_batch_test$y)
  # get predictions
  test_pred[[j]] <- predict_on_batch(model, list(next_batch_test$x$img, next_batch_test$x$aux))
  
  if (j %% 20 == 0){ # every 20 steps, print some information
    print('test step:')
    print(j)
    print('test_loss (mse), test_mean_absolute_error')
    print(history)
  }
  
  mean_test$loss[[j]] <- history[1]
  mean_test$mae[[j]] <- history[2]
  
  # if iterator_get_next is called in last step, error will be thrown
  # this is avoided by this if-condition
  if (j < test_steps)
  {
    # create new batch of validation data:
    next_batch_test <- iterator_get_next(iter_test)
  } 
}

print('average test_loss and test_mean_absolute_error:')
print(mean(unlist(mean_test$loss)))
print(mean(unlist(mean_test$mae)))

# convert to dataframe
test_pred_df <- unlist(test_pred)

# allocate image names and reference data to predictions of test dataset for succeeding analysis
data_full <- cbind(as.character(test_img), test_ref, test_pred_df)
write.csv(data_full, paste0(workdir, outdir, "Test_results.csv"))
