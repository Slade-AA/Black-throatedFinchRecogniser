# User input required -----------------------------------------------------

selectionTableDirectory <- "selectionTables_DBF/2018"
recordingsLocation <- "E:/BTF_Recordings/BTF_2018_44.1kHz"
baseOutputDirectory <- "C:/Users/jc696551/Documents/Projects/callrecognition_BTF/DBF/2018/"
callLength <- 0.672
callFrequency <- c(1.5, 6.5)
spectrogramWidth <- 187
spectrogramHeight <- 187
species <- "DBF"

# Load packages and functions --------------------------------------------------------

library(tidyverse)

# Load custom CNN functions
source("scripts/CNNFunctions.R")

# Create folder structure for holding spectrograms ------------------------

dir.create(paste0(baseOutputDirectory, "all/negative"), recursive = TRUE)
dir.create(paste0(baseOutputDirectory, "all/positive"), recursive = TRUE)
dir.create(paste0(baseOutputDirectory, "train/negative"), recursive = TRUE)
dir.create(paste0(baseOutputDirectory, "train/positive"), recursive = TRUE)
dir.create(paste0(baseOutputDirectory, "val/negative"), recursive = TRUE)
dir.create(paste0(baseOutputDirectory, "val/positive"), recursive = TRUE)


# Generate dataframe of positive and negative samples ---------------------

# ├ Read in positive labelled data ----

labelledFiles <- list.files(path = selectionTableDirectory, 
                            pattern = "*.txt", full.names = TRUE)

labelledData <- list()
for (labels in labelledFiles) {
  tmp_data <- read.csv(labels, header = TRUE, sep = "\t")
  tmp_data <- tmp_data %>% rename(Label = Annotation)
  
  tmp_data <- tmp_data[c('Begin.Time..s.', 'End.Time..s.', 'Low.Freq..Hz.', 'High.Freq..Hz.', 'Label')]
  
  colnames(tmp_data) <- c("start.time", "end.time", "min.frq", "max.frq", "name")
  
  #For recordings with no labels create a single row so that 'eventEval' function works (Note - first row of 'eventEval' result needs to be removed)
  if (nrow(tmp_data) == 0) {
    tmp_data <- rbind(tmp_data, 
                      data.frame(start.time = as.numeric(-10), 
                                 end.time = as.numeric(-10), 
                                 min.frq = as.numeric(-10), 
                                 max.frq = as.numeric(-10), 
                                 name = "NoLabels",
                                 RecordingName = paste0(recordingsLocation, "/", gsub("*Table.*", "wav", basename(labels)))))
  } else {
    tmp_data$RecordingName <- paste0(recordingsLocation, "/", gsub("*Table.*", "wav", basename(labels)))
    tmp_data$name <- species
  }
  
  labelledData[[basename(labels)]] <- tmp_data
}

# ├ Create negative labels between positive labels ----

negativeData <- list()
for (n in 1:length(labelledData)) {
  recordingLength <- tuneR::readWave(paste0(labelledData[[n]]$RecordingName[1]), header = TRUE)
  recordingLength <- recordingLength$samples/recordingLength$sample.rate
  
  negativelabels <- data.frame(Selection = 1:length(seq(0, recordingLength-callLength, callLength)),
                               start.time = seq(0, recordingLength-callLength, callLength),
                               end.time = seq(callLength, recordingLength, callLength))
  negativelabels$time <- rowMeans(negativelabels[,2:3])
  negativelabels$score <- 12
  negativelabels$template <- "blank"
  
  #Use the eventEval function from monitoR to select audio that has not been labelled as target species (i.e., FALSE +'s)
  eval_negativelabels <- monitoR::eventEval(detections = negativelabels,
                                            standard = labelledData[[n]],
                                            tol = 1.3, #use a tolerance just lower than 2*call duration
                                            score.cutoff = 2)
  eval_negativelabels <- eval_negativelabels[eval_negativelabels$outcome == 'FALSE +',]
  eval_negativelabels$min.frq <- 0
  eval_negativelabels$max.frq <- 11000
  eval_negativelabels$RecordingName <- unique(labelledData[[n]]$RecordingName)
  
  negativeData[[n]] <- eval_negativelabels
}

# ├ Bind positive and negative data together ----

positivelabels <- do.call(rbind, labelledData)
positivelabels <- positivelabels[!positivelabels$name == 'NoLabels',]

negativelabels <- do.call(rbind, negativeData)


# Generate spectrograms for positive and negative samples -----------------

#Create and save spectrogram images
createSpectrogramsToDirectory(positivelabels, 
                              width = spectrogramWidth, height = spectrogramHeight, 
                              outputDirectory = paste0(baseOutputDirectory, "all/positive"), 
                              flim = callFrequency)

createSpectrogramsToDirectory(negativelabels, 
                              width = spectrogramWidth, height = spectrogramHeight, 
                              outputDirectory = paste0(baseOutputDirectory, "all/negative"), 
                              flim = callFrequency)

# Downsample negative class and split into train-val sets -----------------

#list all negative spectrograms
negativeSpectrograms <- list.files(path = paste0(baseOutputDirectory, "all/negative"), full.names = TRUE)

#randomly subsample negative spectrograms as there are too many!
set.seed(1234)
negativeSpectrograms_subsample <- sample(negativeSpectrograms, 50000)

#split random subsample into training and validation sets
createTrainValSetsToDirectory(spectrogramFiles = negativeSpectrograms_subsample, 
                              outputDirectoryTrain = paste0(baseOutputDirectory, "train/negative/"), 
                              outputDirectoryVal = paste0(baseOutputDirectory, "val/negative/"), 
                              proportionVal = 0.2,
                              seed = 1)


# Split positive class into train-val sets --------------------------------

#list all positive spectrograms
positiveSpectrograms <- list.files(path = paste0(baseOutputDirectory, "all/positive"), full.names = TRUE)

#split positive spectrograms into training and validation sets
createTrainValSetsToDirectory(spectrogramFiles = positiveSpectrograms, 
                              outputDirectoryTrain = paste0(baseOutputDirectory, "train/positive/"), 
                              outputDirectoryVal = paste0(baseOutputDirectory, "val/positive/"), 
                              proportionVal = 0.2,
                              seed = 1)

# Augment positive train samples ------------------------------------------

positiveTrainSpectrograms <- list.files(paste0(baseOutputDirectory, "train/positive"), "*.png", full.names = TRUE)
negativeTrainSpectrograms <- list.files(paste0(baseOutputDirectory, "train/negative"), "*.png", full.names = TRUE)

numOfPositivesToAugment <- length(negativeTrainSpectrograms) - length(positiveTrainSpectrograms)


augmentSpectrograms_blending_ToDirectory(spectrogramFiles = positiveTrainSpectrograms,
                                         blendingSpectrogramFiles = negativeTrainSpectrograms,
                                         outputDirectory = paste0(baseOutputDirectory, "train/positive/"),
                                         targetDataSetSize = numOfPositivesToAugment)