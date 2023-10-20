#function to take labelled data and save spectrograms for model training or prediction ----
createSpectrogramsToDirectory <- function(labels, width, height, outputDirectory, flim = c(0.3, 5), progressBar = TRUE) {
  spectrogram_size <- width * height
  
  #Check if files have previously been created and skip creating them again
  labels$RecordingName <- as.character(labels$RecordingName)
  labels$start.time <- round(labels$start.time, 3)
  labels$end.time <- round(labels$end.time, 3)
  
  old_size <- nrow(labels)
  
  previousFiles <- list.files(path = outputDirectory,
                              pattern = "*.png")
  
  if (length(previousFiles) > 0) {
    previousFiles <- data.frame(RecordingName = as.character(paste0(recordingsLocation, "/", gsub(".wav_.*", ".wav", previousFiles))),
                                start.time = round(as.numeric(gsub(".*.wav_(.+)_.*", "\\1", previousFiles)),3),
                                end.time = round(as.numeric(gsub(".*_(.+).png", "\\1", previousFiles)),3))
    
    labels <- dplyr::anti_join(labels, previousFiles)
  }
  
  new_size <- nrow(labels)
  
  if (new_size < old_size) {
    print(paste0(old_size - new_size, " spectrograms found already. ", new_size, " left to create."))
  }
  
  if (new_size == 0) {
    stop("No spectrograms to produce")
  }
  
  #progress bar
  if (progressBar == TRUE) {
    print(paste("Start processing", nrow(labels), "wave files"))
    pbapply::pboptions(type = "timer") # default
  } else {
    pbapply::pboptions(type = "none")
  }
  ## This function will resize an image, turn it into greyscale
  spectrogram_list <- pbapply::pblapply(1:nrow(labels), function(x) {
    ## Read wave
    wav <- tuneR::readWave(paste0(labels$RecordingName[x]),
                           from = labels$start.time[x], 
                           to = labels$end.time[x],
                           units = "seconds")
    ## Create spectrogram
    spectrogram <- seewave::spectro(wav, plot = FALSE, wl = 1024, ovlp = 85, noisereduction = FALSE, flim = flim)$amp
    ## Resize spectrogram
    spectrogram_resize <- rayshader::resize_matrix(spectrogram, width = width, height = height)
    
    spectrogram_resize_norm <- (spectrogram_resize-min(spectrogram_resize))/(max(spectrogram_resize)-min(spectrogram_resize))
    
    png::writePNG(spectrogram_resize_norm, paste0(outputDirectory, "/", 
                                                  basename(as.character(labels$RecordingName[x])), "_",
                                                  labels$start.time[x], "_",
                                                  labels$end.time[x],
                                                  ".png"))
  })
}

#function to copy files into training and validation sets ----
createTrainValSetsToDirectory <- function(spectrogramFiles, outputDirectoryTrain, outputDirectoryVal, proportionVal = 0.2, seed = 1) {
  set.seed(seed = seed)
  valIndex <- sample(length(spectrogramFiles), length(spectrogramFiles)*proportionVal)
  
  spectrograms_train <- spectrogramFiles[-valIndex]
  spectrograms_train_newFolder <- gsub(".*/", outputDirectoryTrain, spectrograms_train)
  
  file.copy(spectrograms_train, spectrograms_train_newFolder)
  
  spectrograms_val <- spectrogramFiles[valIndex]
  spectrograms_val_newFolder <- gsub(".*/", outputDirectoryVal, spectrograms_val)
  
  file.copy(spectrograms_val, spectrograms_val_newFolder)
}

#function to augment spectrpgrams by shifting them and blending them with spectrograms from another folder ----
augmentSpectrograms_blending_ToDirectory <- function(spectrogramFiles, blendingSpectrogramFiles, outputDirectory, targetDataSetSize = 1000) {
  spectrogram_augments <- list()
  
  n <- 1
  for (i in 1:targetDataSetSize) {
    if (n > length(spectrogramFiles)) {
      n <- 1
    }
    originalPositiveSpectrogram <- png::readPNG(spectrogramFiles[n])
    originalNegativeSpectrogram <- png::readPNG(blendingSpectrogramFiles[sample(length(blendingSpectrogramFiles), 1)])
    
    spectrogram_shifted <- OpenImageR::Augmentation(originalPositiveSpectrogram,
                                                    shift_cols = sample(-20:20, 1), 
                                                    shift_rows = sample(-10:10, 1),
                                                    padded_value = min(originalPositiveSpectrogram)) #0
    #blend the shifted spectrogram with other spectrograms
    randMix <- sample(seq(0.5, 1, 0.02), 1)
    spectrogram_blended <- (spectrogram_shifted * randMix) + (originalNegativeSpectrogram * (1-randMix))
    
    png::writePNG(spectrogram_blended, paste0(outputDirectory, gsub("*.png", "", basename(spectrogramFiles[n])), "_shifted&blended_", i, ".png"))
    
    n <- n + 1
  }
}

#function to generate empty selections for prediction ----
generateSelections <- function(listOfRecordings, selectionLength, overlapFactor) {
  unknownData <- list()
  for (n in 1:length(listOfRecordings)) {
    recordingInfo <- tuneR::readWave(listOfRecordings[n], header = TRUE)
    recordingLength <- recordingInfo$samples/recordingInfo$sample.rate
    
    unknownlabels <- data.frame(Selection = 1:length(seq(0, recordingLength-selectionLength, (selectionLength*overlapFactor))),
                                start.time = seq(0, recordingLength-selectionLength, (selectionLength*overlapFactor)),
                                end.time = seq(selectionLength, recordingLength, (selectionLength*overlapFactor)))
    unknownlabels$time <- rowMeans(unknownlabels[,2:3])
    
    unknownlabels$min.frq <- 0
    unknownlabels$max.frq <- 11000
    unknownlabels$RecordingName <- listOfRecordings[n]
    
    unknownData[[n]] <- unknownlabels
  }
  unknownlabels <- do.call(rbind, unknownData)
  
  return(unknownlabels)
}

#function to train a CNN given a folder of training and validation images ----
trainCNN <- function(trainFolder,
                     validationFolder,
                     outputFolder,
                     species,
                     dataset,
                     modelArchitecture = 'inception_v3',
                     width = 187,
                     height = 187,
                     batchSize = 16,
                     nEpochs = 50) {
  
  dir.create(path = outputFolder, recursive = TRUE)
  
  #Load and set-up
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
  
  
  #Setup data generators
  train_datagen <- flow_images_from_directory(generator = image_data_generator(),
                                              directory = trainFolder,
                                              target_size = c(height, width),
                                              color_mode = "grayscale",
                                              batch_size = batchSize,
                                              shuffle = TRUE,
                                              class_mode = "binary")
  
  val_datagen <- flow_images_from_directory(generator = image_data_generator(),
                                            directory = validationFolder,
                                            target_size = c(height, width),
                                            color_mode = "grayscale",
                                            batch_size = batchSize,
                                            shuffle = TRUE,
                                            class_mode = "binary")
  
  train_samples <- train_datagen$n
  validation_samples <- val_datagen$n
  
  
  #Model
  if (modelArchitecture == 'inception_v3') {
    base_model <- application_inception_v3(weights = NULL, 
                                           include_top = FALSE,
                                           input_shape = c(height, width, 1))
  } else if (modelArchitecture == 'resnet50') {
    base_model <- application_resnet50(weights = NULL, 
                                       include_top = FALSE,
                                       input_shape = c(height, width, 1))
  } else if (modelArchitecture == 'inception_resnet_v2') {
    base_model <- application_inception_resnet_v2(weights = NULL, 
                                                  include_top = FALSE,
                                                  input_shape = c(height, width, 1))
  }
  
  
  
  #Custom layers
  predictions <- base_model$output %>% 
    layer_global_average_pooling_2d() %>% 
    layer_dense(units = 1024, activation = "relu") %>% 
    layer_dense(units = 1, activation = "sigmoid")
  
  model <- keras_model(inputs = base_model$input, 
                       outputs = predictions)
  
  model %>% compile(
    loss = "binary_crossentropy",
    optimizer = optimizer_sgd(lr = 0.0001, 
                              momentum = 0.9, 
                              decay = 1e-5),
    metrics = c('accuracy', tf$keras$metrics$Precision(), tf$keras$metrics$Recall())#, tf$keras$metrics$AUC(num_thresholds = 50, curve = "PR"))
  )
  
  #Run model
  history <- model %>% fit_generator(
    generator = train_datagen,
    epochs = nEpochs, #number times that the learning algorithm will work through the entire training dataset
    steps_per_epoch = as.integer(train_samples/batchSize), #total dataset size divided by batch size
    validation_data = val_datagen,
    validation_steps = as.integer(validation_samples/batchSize),
    callbacks = list(callback_early_stopping(monitor = "val_loss",
                                             min_delta = 0.002,
                                             patience = 5,
                                             restore_best_weights = TRUE))
  )
  
  #Save outputs
  saveRDS(history, paste0(outputFolder, "/", species, "_", dataset, "_", modelArchitecture, "_", Sys.Date(), ".rds"))
  
  history$params$epochs <- length(history$metrics$loss)
  plot_history <- plot(history) + theme_bw()
  ggsave(filename = paste0(outputFolder, "/", species, "_", dataset, "_", modelArchitecture, "_", Sys.Date(), ".jpg"), 
         dpi = 800, width = 20, height = 15, units = "cm")
  
  model %>% save_model_hdf5(paste0(outputFolder, "/", species, "_", dataset, "_", modelArchitecture, "_", Sys.Date(), ".h5"))
  
  #clear session
  keras::k_clear_session()
  
}

#function to test model performance on a test set ----
testModels <- function(modelsToTest,
                       spectrogramDirectory,
                       outputDirectory) {
  
  #Lists to store performance outputs for each model
  Predictions <- list() #saves predictions of each individal model
  PRData <- list() #saves precision-recall data for each individual model
  ConfMatrices <- list() #stores confusion matrix plots of individual models
  PerformanceMetrics <- list() #stores performance metrics for each model
  PRCurves <- list() #stores ggplots of individual model PRCurves
  
  
  #load and setup
  library(tidyverse)
  library(tidymodels)
  library(cowplot)
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
  
  for (models in modelsToTest) {
    #Load pre-trained model
    
    if (R.utils::isAbsolutePath(models) == TRUE) {
      model <- load_model_hdf5(models)
    } else if (R.utils::isAbsolutePath(models) == FALSE) {
      model <- load_model_hdf5(paste0("outputs/model_outputs/", models))
    }
    
    #measure number of files to determine batch size
    n_files <- length(list.files(spectrogramDirectory, recursive = TRUE))
    
    #function to determine batch size
    max_factor <- function(x) {
      factors <- c()
      for(i in 1:200) {
        if((x %% i) == 0) {
          factors <- c(factors, i)
        }
      }
      return(max(factors))
    }
    
    batch_size <- max_factor(n_files)
    
    predict_datagen <- flow_images_from_directory(generator = image_data_generator(),
                                                  directory = spectrogramDirectory,
                                                  target_size = c(187, 187),
                                                  color_mode = "grayscale",
                                                  batch_size = batch_size,
                                                  shuffle = FALSE,
                                                  class_mode = NULL)
    
    predictions <- predict_generator(object = model, 
                                     generator = predict_datagen,
                                     steps = predict_datagen$n/predict_datagen$batch_size)
    
    #Add prediction probabilities and predictions (threshold 0.5) to a dataframe
    classes <- predict_datagen$classes
    
    Predictions[[gsub(".h5", "", models)]] <- data.frame(file = list.files(spectrogramDirectory, recursive = TRUE), 
                                                         truth = factor(classes), 
                                                         estimate = predictions)
    Predictions[[gsub(".h5", "", models)]] <- Predictions[[gsub(".h5", "", models)]] %>% mutate(prediction = factor(ifelse(estimate >= 0.5, 1, 0)))
    
    #Calcualte and plot performance metrics
    PRData[[gsub(".h5", "", models)]] <- pr_curve(Predictions[[gsub(".h5", "", models)]], truth, estimate, event_level = "second")
    
    ConfMatrices[[gsub(".h5", "", models)]] <- Predictions[[gsub(".h5", "", models)]] %>% conf_mat(truth = truth, estimate = prediction) %>% autoplot(type = "heatmap")
    
    class_metrics <- metric_set(yardstick::recall, 
                                yardstick::precision, 
                                yardstick::bal_accuracy, 
                                yardstick::f_meas, 
                                yardstick::kap) #custom metric set #yardstick::pr_auc
    
    PerformanceMetrics[[gsub(".h5", "", models)]] <- Predictions[[gsub(".h5", "", models)]] %>% 
      class_metrics(truth = truth, estimate = prediction, event_level = "second")
    
    PerformanceMetrics[[gsub(".h5", "", models)]] <- add_row(PerformanceMetrics[[gsub(".h5", "", models)]],
                                                             pr_auc(Predictions[[gsub(".h5", "", models)]], truth = truth, estimate = estimate, event_level = "second"))
    
    #area under precision-recall curve
    pr_auc_Model <- round(Predictions[[gsub(".h5", "", models)]] %>% pr_auc(truth, estimate, event_level = "second") %>% select(.estimate), 3)
    
    PRCurves[[gsub(".h5", "", models)]] <- ggplot(data = PRData[[gsub(".h5", "", models)]], aes(x = recall, y = precision)) +
      geom_line() +
      labs(x = "Recall", y = "Precision") +
      annotate("text", x = 0.2, y = 0.2, label = paste0("AUC: ", pr_auc_Model)) +
      theme_bw()
    
    #clear session
    keras::k_clear_session()
  }
  
  return(list(Predictions = Predictions,
              PRData = PRData,
              ConfMatrices = ConfMatrices,
              PerformanceMetrics = PerformanceMetrics,
              PRCurves = PRCurves))
}