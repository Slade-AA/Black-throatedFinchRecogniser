# User input required -----------------------------------------------------

audioDirectory = "D:/2023_Trip2/3406_DLTR"
models = list("outputs/model_outputs/BTF_combined_inception_resnet_v2_2022-07-10.h5",
              "outputs/model_outputs/BTF_combined_inception_v3_2022-05-31.h5",
              "outputs/model_outputs/BTF_combined_resnet50_2022-06-16.h5",
              "outputs/model_outputs/DBF_combined_inception_resnet_v2_2022-06-15.h5",
              "outputs/model_outputs/DBF_combined_inception_v3_2022-06-05.h5",
              "outputs/model_outputs/DBF_combined_resnet50_2022-06-06.h5")
outputDirectory = "outputs/prediction_outputs/BTF_RecogniserSurveys2023_2_44.1kHz"
tempSpectrogramDirectory = "C:/tempSpectrograms"
callLength = 0.672
callFrequency = c(1.5, 6.5)
overlapFactor = 1
spectrogramWidth = 187
spectrogramHeight = 187
batch_size = 100

# Load packages and set-up ----
library(tidyverse)
library(ggplot2)
library(reshape2)
library(tensorflow)
tensorflow::use_condaenv("TensorflowKerasGPU")

library(reticulate)
conda_python(envname = "TensorflowKerasGPU")
library(tfestimators)

library(keras)

library(caret)

k = backend()

# Run prediction ----------------------------------------------------------

dir.create(path = outputDirectory, recursive = TRUE)
dir.create(path = paste0(tempSpectrogramDirectory, "/prediction"), recursive = TRUE)

# Load in models
Models <- list()
for (model in 1:length(models)) {
  Models[[basename(models[[model]])]] <- load_model_hdf5(models[[model]])
}

# Load custom CNN functions
source("scripts/CNNFunctions.R")


# Generate selections from list of audio files ----------------------------

audioFiles <- list.files(audioDirectory, 
                         pattern = "*.wav",
                         full.names = TRUE,
                         recursive = FALSE)


unknownSelections <- generateSelections(listOfRecordings = audioFiles,
                                        selectionLength = callLength,
                                        overlapFactor = overlapFactor)

#measure number of files to determine batch size
n_spectrograms <- nrow(unknownSelections)

check_factors <- list(`1200` = n_spectrograms %% 1200,
                      `1100` = n_spectrograms %% 1100, 
                      `1000` = n_spectrograms %% 1000, 
                      `900` = n_spectrograms %% 900, 
                      `800` = n_spectrograms %% 800, 
                      `700` = n_spectrograms %% 700, 
                      `600` = n_spectrograms %% 600)

rowsToRemove <- as.numeric(check_factors[which.min(check_factors)])
loop_size <- as.numeric(names(which.min(check_factors)))

unknownSelections <- slice(unknownSelections, 1:(n() - rowsToRemove)) #remove 2 rows so that the number of rows has more factors


# Create spectrograms to a directory and make predictions -----------------

for (model in 1:length(Models)) {
  unknownSelections[[names(Models[model])]] <- numeric(length = nrow(unknownSelections))
}


loops <- data.frame(start = seq(1, nrow(unknownSelections), loop_size),
                    end = seq(loop_size, nrow(unknownSelections), loop_size))


seq_to_predict <- 201:407 #(1:310 done)

tictoc::tic()
for (row in seq_to_predict) {
  print(paste0("Processing loop ", which(seq_to_predict == row), " of ", length((seq_to_predict))))
  
  createSpectrogramsToDirectory(unknownSelections[loops$start[row]:loops$end[row],], 
                                width = spectrogramWidth, height = spectrogramHeight, 
                                outputDirectory = paste0(tempSpectrogramDirectory, "/prediction"), 
                                flim = callFrequency)
  
  predict_datagen_test <- flow_images_from_directory(generator = image_data_generator(),
                                                     directory = tempSpectrogramDirectory,
                                                     target_size = c(187, 187),
                                                     color_mode = "grayscale",
                                                     batch_size = batch_size,
                                                     shuffle = FALSE,
                                                     class_mode = NULL)
  
  for (model in 1:length(Models)) {
    unknownSelections[[names(Models[model])]][loops$start[row]:loops$end[row]] <- predict_generator(object = Models[[model]], 
                                                                                                        generator = predict_datagen_test,
                                                                                                        steps = predict_datagen_test$n/predict_datagen_test$batch_size)
  }
  
  filesToRemove <- list.files(path = paste0(tempSpectrogramDirectory, "/prediction"),
                              full.names = TRUE)
  
  file.remove(filesToRemove)
  
}
tictoc::toc()

save(unknownSelections, loops, seq_to_predict, createSpectrogramsToDirectory, file = paste0(outputDirectory, "/", basename(audioDirectory), "_2023_1_", max_width(seq_to_predict), ".RData"))
