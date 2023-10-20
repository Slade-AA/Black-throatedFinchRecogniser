# Load function ----
source("scripts/CNNFunctions.R")

# BTF ----

# ├ 2018 ----
trainCNN(trainFolder = "C:/Users/jc696551/Documents/Projects/callrecognition_BTF/BTF/2018/train",
         validationFolder = "C:/Users/jc696551/Documents/Projects/callrecognition_BTF/BTF/2018/val",
         outputFolder = "outputs/model_outputs",
         species = "BTF",
         dataset = "2018",
         modelArchitecture = 'inception_v3',
         width = 187,
         height = 187,
         batchSize = 16,
         nEpochs = 50)

trainCNN(trainFolder = "C:/Users/jc696551/Documents/Projects/callrecognition_BTF/BTF/2018/train",
         validationFolder = "C:/Users/jc696551/Documents/Projects/callrecognition_BTF/BTF/2018/val",
         outputFolder = "outputs/model_outputs",
         species = "BTF",
         dataset = "2018",
         modelArchitecture = 'resnet50',
         width = 187,
         height = 187,
         batchSize = 16,
         nEpochs = 50)

trainCNN(trainFolder = "C:/Users/jc696551/Documents/Projects/callrecognition_BTF/BTF/2018/train",
         validationFolder = "C:/Users/jc696551/Documents/Projects/callrecognition_BTF/BTF/2018/val",
         outputFolder = "outputs/model_outputs",
         species = "BTF",
         dataset = "2018",
         modelArchitecture = 'inception_resnet_v2',
         width = 187,
         height = 187,
         batchSize = 16,
         nEpochs = 50)


# ├ 2020 ----
trainCNN(trainFolder = "C:/Users/jc696551/Documents/Projects/callrecognition_BTF/BTF/2020_RoadNoiseSurveys/train",
         validationFolder = "C:/Users/jc696551/Documents/Projects/callrecognition_BTF/BTF/2020_RoadNoiseSurveys/val",
         outputFolder = "outputs/model_outputs",
         species = "BTF",
         dataset = "2020_RoadNoiseSurveys",
         modelArchitecture = 'inception_v3',
         width = 187,
         height = 187,
         batchSize = 16,
         nEpochs = 50)

trainCNN(trainFolder = "C:/Users/jc696551/Documents/Projects/callrecognition_BTF/BTF/2020_RoadNoiseSurveys/train",
         validationFolder = "C:/Users/jc696551/Documents/Projects/callrecognition_BTF/BTF/2020_RoadNoiseSurveys/val",
         outputFolder = "outputs/model_outputs",
         species = "BTF",
         dataset = "2020_RoadNoiseSurveys",
         modelArchitecture = 'resnet50',
         width = 187,
         height = 187,
         batchSize = 16,
         nEpochs = 50)

trainCNN(trainFolder = "C:/Users/jc696551/Documents/Projects/callrecognition_BTF/BTF/2020_RoadNoiseSurveys/train",
         validationFolder = "C:/Users/jc696551/Documents/Projects/callrecognition_BTF/BTF/2020_RoadNoiseSurveys/val",
         outputFolder = "outputs/model_outputs",
         species = "BTF",
         dataset = "2020_RoadNoiseSurveys",
         modelArchitecture = 'inception_resnet_v2',
         width = 187,
         height = 187,
         batchSize = 16,
         nEpochs = 50)

# ├ combined ----
trainCNN(trainFolder = "C:/Users/jc696551/Documents/Projects/callrecognition_BTF/BTF/combined/train",
         validationFolder = "C:/Users/jc696551/Documents/Projects/callrecognition_BTF/BTF/combined/val",
         outputFolder = "outputs/model_outputs",
         species = "BTF",
         dataset = "combined",
         modelArchitecture = 'inception_v3',
         width = 187,
         height = 187,
         batchSize = 16,
         nEpochs = 50)

trainCNN(trainFolder = "C:/Users/jc696551/Documents/Projects/callrecognition_BTF/BTF/combined/train",
         validationFolder = "C:/Users/jc696551/Documents/Projects/callrecognition_BTF/BTF/combined/val",
         outputFolder = "outputs/model_outputs",
         species = "BTF",
         dataset = "combined",
         modelArchitecture = 'resnet50',
         width = 187,
         height = 187,
         batchSize = 16,
         nEpochs = 50)

trainCNN(trainFolder = "C:/Users/jc696551/Documents/Projects/callrecognition_BTF/BTF/combined/train",
         validationFolder = "C:/Users/jc696551/Documents/Projects/callrecognition_BTF/BTF/combined/val",
         outputFolder = "outputs/model_outputs",
         species = "BTF",
         dataset = "combined",
         modelArchitecture = 'inception_resnet_v2',
         width = 187,
         height = 187,
         batchSize = 16,
         nEpochs = 50)


# DBF ----


# ├ 2018 ----
trainCNN(trainFolder = "C:/Users/jc696551/Documents/Projects/callrecognition_BTF/DBF/2018/train",
         validationFolder = "C:/Users/jc696551/Documents/Projects/callrecognition_BTF/DBF/2018/val",
         outputFolder = "outputs/model_outputs",
         species = "DBF",
         dataset = "2018",
         modelArchitecture = 'inception_v3',
         width = 187,
         height = 187,
         batchSize = 16,
         nEpochs = 50)

trainCNN(trainFolder = "C:/Users/jc696551/Documents/Projects/callrecognition_BTF/DBF/2018/train",
         validationFolder = "C:/Users/jc696551/Documents/Projects/callrecognition_BTF/DBF/2018/val",
         outputFolder = "outputs/model_outputs",
         species = "DBF",
         dataset = "2018",
         modelArchitecture = 'resnet50',
         width = 187,
         height = 187,
         batchSize = 16,
         nEpochs = 50)

trainCNN(trainFolder = "C:/Users/jc696551/Documents/Projects/callrecognition_BTF/DBF/2018/train",
         validationFolder = "C:/Users/jc696551/Documents/Projects/callrecognition_BTF/DBF/2018/val",
         outputFolder = "outputs/model_outputs",
         species = "DBF",
         dataset = "2018",
         modelArchitecture = 'inception_resnet_v2',
         width = 187,
         height = 187,
         batchSize = 16,
         nEpochs = 50)


# ├ 2020 ----
trainCNN(trainFolder = "C:/Users/jc696551/Documents/Projects/callrecognition_BTF/DBF/2020_RoadNoiseSurveys/train",
         validationFolder = "C:/Users/jc696551/Documents/Projects/callrecognition_BTF/DBF/2020_RoadNoiseSurveys/val",
         outputFolder = "outputs/model_outputs",
         species = "DBF",
         dataset = "2020_RoadNoiseSurveys",
         modelArchitecture = 'inception_v3',
         width = 187,
         height = 187,
         batchSize = 16,
         nEpochs = 50)

trainCNN(trainFolder = "C:/Users/jc696551/Documents/Projects/callrecognition_BTF/DBF/2020_RoadNoiseSurveys/train",
         validationFolder = "C:/Users/jc696551/Documents/Projects/callrecognition_BTF/DBF/2020_RoadNoiseSurveys/val",
         outputFolder = "outputs/model_outputs",
         species = "DBF",
         dataset = "2020_RoadNoiseSurveys",
         modelArchitecture = 'resnet50',
         width = 187,
         height = 187,
         batchSize = 16,
         nEpochs = 50)

trainCNN(trainFolder = "C:/Users/jc696551/Documents/Projects/callrecognition_BTF/DBF/2020_RoadNoiseSurveys/train",
         validationFolder = "C:/Users/jc696551/Documents/Projects/callrecognition_BTF/DBF/2020_RoadNoiseSurveys/val",
         outputFolder = "outputs/model_outputs",
         species = "DBF",
         dataset = "2020_RoadNoiseSurveys",
         modelArchitecture = 'inception_resnet_v2',
         width = 187,
         height = 187,
         batchSize = 16,
         nEpochs = 50)

# ├ combined ----
trainCNN(trainFolder = "C:/Users/jc696551/Documents/Projects/callrecognition_BTF/DBF/combined/train",
         validationFolder = "C:/Users/jc696551/Documents/Projects/callrecognition_BTF/DBF/combined/val",
         outputFolder = "outputs/model_outputs",
         species = "DBF",
         dataset = "combined",
         modelArchitecture = 'inception_v3',
         width = 187,
         height = 187,
         batchSize = 16,
         nEpochs = 50)

trainCNN(trainFolder = "C:/Users/jc696551/Documents/Projects/callrecognition_BTF/DBF/combined/train",
         validationFolder = "C:/Users/jc696551/Documents/Projects/callrecognition_BTF/DBF/combined/val",
         outputFolder = "outputs/model_outputs",
         species = "DBF",
         dataset = "combined",
         modelArchitecture = 'resnet50',
         width = 187,
         height = 187,
         batchSize = 16,
         nEpochs = 50)

trainCNN(trainFolder = "C:/Users/jc696551/Documents/Projects/callrecognition_BTF/DBF/combined/train",
         validationFolder = "C:/Users/jc696551/Documents/Projects/callrecognition_BTF/DBF/combined/val",
         outputFolder = "outputs/model_outputs",
         species = "DBF",
         dataset = "combined",
         modelArchitecture = 'inception_resnet_v2',
         width = 187,
         height = 187,
         batchSize = 16,
         nEpochs = 50)