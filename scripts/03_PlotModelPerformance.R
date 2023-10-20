source("scripts/CNNFunctions.R")

#Use models trained on 2018 recording to predict on 2020 recordings and vice versa

# BTF ----

# ├ 2018 models ----
Performance_2018Models <- testModels(modelsToTest = list("BTF_2018_inception_resnet_v2_2022-05-22.h5",
                                                         "BTF_2018_inception_v3_2022-05-21.h5",
                                                         "BTF_2018_resnet50_2022-05-21.h5"),
                                     spectrogramDirectory = "C:/Users/jc696551/Documents/Projects/callrecognition_BTF/BTF/2020_RoadNoiseSurveys/all",
                                     outputDirectory = "outputs/modelPerformance")

saveRDS(Performance_2018Models, "outputs/modelPerformance/Performance_2018Models_BTF.rds")

# ├ 2020 models ----
Performance_2020Models <- testModels(modelsToTest = list("BTF_2020_RoadNoiseSurveys_inception_resnet_v2_2022-05-25.h5",
                                                         "BTF_2020_RoadNoiseSurveys_inception_v3_2022-05-22.h5",
                                                         "BTF_2020_RoadNoiseSurveys_resnet50_2022-05-22.h5"),
                                     spectrogramDirectory = "C:/Users/jc696551/Documents/Projects/callrecognition_BTF/BTF/2018/all",
                                     outputDirectory = "outputs/modelPerformance")

saveRDS(Performance_2020Models, "outputs/modelPerformance/Performance_2020Models_BTF.rds")

# DBF ----

# ├ 2018 models ----
Performance_2018Models_DBF <- testModels(modelsToTest = list("DBF_2018_inception_resnet_v2_2022-06-03.h5",
                                                             "DBF_2018_inception_v3_2022-06-02.h5",
                                                             "DBF_2018_resnet50_2022-06-02.h5"),
                                         spectrogramDirectory = "C:/Users/jc696551/Documents/Projects/callrecognition_BTF/DBF/2020_RoadNoiseSurveys/all",
                                         outputDirectory = "outputs/modelPerformance")

saveRDS(Performance_2018Models_DBF, "outputs/modelPerformance/Performance_2018Models_DBF.rds")


# ├ 2020 models ----
Performance_2020Models_DBF <- testModels(modelsToTest = list("DBF_2020_RoadNoiseSurveys_inception_resnet_v2_2022-05-28.h5",
                                                             "DBF_2020_RoadNoiseSurveys_inception_v3_2022-05-26.h5",
                                                             "DBF_2020_RoadNoiseSurveys_resnet50_2022-05-27.h5"),
                                         spectrogramDirectory = "C:/Users/jc696551/Documents/Projects/callrecognition_BTF/DBF/2018/all",
                                         outputDirectory = "outputs/modelPerformance")

saveRDS(Performance_2020Models_DBF, "outputs/modelPerformance/Performance_2020Models_DBF.rds")


# Plotting performance ----
library(tidyverse)
library(ggpubr)
library(cowplot)
library(yardstick)

# ├ Read results back into R ----

Performance_2018Models_BTF <- readRDS("outputs/modelPerformance/Performance_2018Models_BTF.rds")
Performance_2020Models_BTF <- readRDS("outputs/modelPerformance/Performance_2020Models_BTF.rds")
Performance_2018Models_DBF <- readRDS("outputs/modelPerformance/Performance_2018Models_DBF.rds")
Performance_2020Models_DBF <- readRDS("outputs/modelPerformance/Performance_2020Models_DBF.rds")


# ├ Calculate PRAUC for all models ----

lapply(Performance_2018Models_BTF$Predictions, function(x) pr_auc(data = x, truth = truth, estimate = estimate, event_level = "second"))
lapply(Performance_2020Models_BTF$Predictions, function(x) pr_auc(data = x, truth = truth, estimate = estimate, event_level = "second"))
lapply(Performance_2018Models_DBF$Predictions, function(x) pr_auc(data = x, truth = truth, estimate = estimate, event_level = "second"))
lapply(Performance_2020Models_DBF$Predictions, function(x) pr_auc(data = x, truth = truth, estimate = estimate, event_level = "second"))

# ├ Precision-recall curve of all models ----

# ├├ BTF ----
PRCurves_2018_BTF <- bind_rows(Performance_2018Models_BTF$PRData, .id = "model") %>% 
  mutate(model = gsub("BTF_2018_(.+)_2022.*", "\\1", model)) %>% 
  ggplot(aes(x = recall, y = precision, group = model, colour = model)) +
  geom_line(size = 1) +
  scale_color_viridis_d() +
  labs(x = "Recall", y = "Precision") +
  theme_bw() +
  theme(legend.position = "bottom")

PRCurves_2020_BTF <- bind_rows(Performance_2020Models_BTF$PRData, .id = "model") %>% 
  mutate(model = gsub("BTF_2020_RoadNoiseSurveys_(.+)_2022.*", "\\1", model)) %>% 
  ggplot(aes(x = recall, y = precision, group = model, colour = model)) +
  geom_line(size = 1) +
  scale_color_viridis_d() +
  labs(x = "Recall", y = "Precision") +
  theme_bw() +
  theme(legend.position = "bottom")

PRCurves_legend <- get_legend(PRCurves_2018_BTF)

Plot_PRCurves_BTF <- plot_grid(PRCurves_2018_BTF + rremove("legend"), PRCurves_2020_BTF + rremove("legend"),
                               nrow = 1,
                               labels = c("A", "B"))
Plot_PRCurves_BTF <- plot_grid(Plot_PRCurves_BTF, PRCurves_legend,
                               ncol = 1, rel_heights = c(1, 0.1))

ggsave("outputs/figures/PrecisionRecallCurves_BTF.png", Plot_PRCurves_BTF,
       dpi = 800, width = 18, height = 9, units = "cm")

# ├├ DBF ----
PRCurves_2018_DBF <- bind_rows(Performance_2018Models_DBF$PRData, .id = "model") %>% 
  mutate(model = gsub("DBF_2018_(.+)_2022.*", "\\1", model)) %>% 
  ggplot(aes(x = recall, y = precision, group = model, colour = model)) +
  geom_line(size = 1) +
  scale_color_viridis_d() +
  labs(x = "Recall", y = "Precision") +
  theme_bw() +
  theme(legend.position = "bottom")

PRCurves_2020_DBF <- bind_rows(Performance_2020Models_DBF$PRData, .id = "model") %>% 
  mutate(model = gsub("DBF_2020_RoadNoiseSurveys_(.+)_2022.*", "\\1", model)) %>% 
  ggplot(aes(x = recall, y = precision, group = model, colour = model)) +
  geom_line(size = 1) +
  scale_color_viridis_d() +
  labs(x = "Recall", y = "Precision") +
  theme_bw() +
  theme(legend.position = "bottom")

PRCurves_legend <- get_legend(PRCurves_2018_DBF)

Plot_PRCurves_DBF <- plot_grid(PRCurves_2018_DBF + rremove("legend"), PRCurves_2020_DBF + rremove("legend"),
                               nrow = 1,
                               labels = c("A", "B"))
Plot_PRCurves_DBF <- plot_grid(Plot_PRCurves_DBF, PRCurves_legend,
                               ncol = 1, rel_heights = c(1, 0.1))

ggsave("outputs/figures/PrecisionRecallCurves_DBF.png", Plot_PRCurves_DBF,
       dpi = 800, width = 18, height = 9, units = "cm")

# ├├ Plot both together ----

Plot_PRCurves_Combined <- plot_grid(PRCurves_2018_BTF + rremove("legend"), PRCurves_2020_BTF + rremove("legend"),
                                    PRCurves_2018_DBF + rremove("legend"), PRCurves_2020_DBF + rremove("legend"),
                                    nrow = 2,
                                    labels = c("A", "B", "C", "D"))
Plot_PRCurves_Combined <- plot_grid(Plot_PRCurves_Combined, PRCurves_legend,
                                    ncol = 1, rel_heights = c(1, 0.1))

ggsave("outputs/figures/PrecisionRecallCurves_Combined.png", Plot_PRCurves_Combined,
       dpi = 800, width = 18, height = 18, units = "cm")

# ├ Find threshold with greatest F-score ----

# ├├ BTF 2018 ----

Plots_ThresholdTuning_2018_BTF <- list()
for (model in 1:length(Performance_2018Models_BTF$Predictions)) {
  viridisColours <- viridis::viridis(3)
  
  f_threshold <- list()
  for (threshold in seq(0.01, 0.99, 0.01)) {
    f_threshold[[as.character(threshold)]] <- Performance_2018Models_BTF$Predictions[[model]] %>% 
      mutate(prediction_threshold = factor(ifelse(estimate >= threshold, 1, 0))) %>% 
      yardstick::f_meas(truth = truth, estimate = prediction_threshold, event_level = "second")
  }
  f_threshold <- bind_rows(f_threshold, .id = "threshold")
  
  Plots_ThresholdTuning_2018_BTF[[names(Performance_2018Models_BTF$Predictions[model])]] <- ggplot(data = f_threshold, aes(x = as.numeric(threshold), y = .estimate)) + 
    geom_hline(yintercept = f_threshold$.estimate[which.max(f_threshold$.estimate)], linetype = 'dashed') +
    annotate(geom = "text", 
             x = 0.25, 
             y = as.numeric(f_threshold$.estimate[[which.max(f_threshold$.estimate)]]) + 0.05, 
             label = paste0("F1 = ", round(f_threshold$.estimate[[which.max(f_threshold$.estimate)]],2))) +
    geom_line(size = 1, colour = viridisColours[model]) +
    geom_point(data = f_threshold[which.max(f_threshold$.estimate), ], color = "black", size = 3) +
    scale_y_continuous(limits = c(0, 1)) +
    annotate(geom = "text", 
             x = as.numeric(f_threshold[[which.max(f_threshold$.estimate), 1]]), 
             y = as.numeric(f_threshold[[which.max(f_threshold$.estimate), 4]]) - 0.05, 
             label = f_threshold[[which.max(f_threshold$.estimate), 1]]) +
    labs(x = "Threshold", y = "F1-score") +
    theme_bw()
}
plot_grid(plotlist = Plots_ThresholdTuning_2018_BTF, nrow = 1) %>% 
  ggsave(filename = "outputs/figures/F1_Threshold_BTF_2018.png",
         dpi = 800, width = 36, height = 12, units = "cm")

# ├├ BTF 2020 ----
Plots_ThresholdTuning_2020_BTF <- list()
for (model in 1:length(Performance_2020Models_BTF$Predictions)) {
  viridisColours <- viridis::viridis(3)
  
  f_threshold <- list()
  for (threshold in seq(0.01, 0.99, 0.01)) {
    f_threshold[[as.character(threshold)]] <- Performance_2020Models_BTF$Predictions[[model]] %>% 
      mutate(prediction_threshold = factor(ifelse(estimate >= threshold, 1, 0))) %>% 
      yardstick::f_meas(truth = truth, estimate = prediction_threshold, event_level = "second")
  }
  f_threshold <- bind_rows(f_threshold, .id = "threshold")
  
  Plots_ThresholdTuning_2020_BTF[[names(Performance_2020Models_BTF$Predictions[model])]] <- ggplot(data = f_threshold, aes(x = as.numeric(threshold), y = .estimate)) + 
    geom_hline(yintercept = f_threshold$.estimate[which.max(f_threshold$.estimate)], linetype = 'dashed') +
    annotate(geom = "text", 
             x = 0.25, 
             y = as.numeric(f_threshold$.estimate[[which.max(f_threshold$.estimate)]]) + 0.05, 
             label = paste0("F1 = ", round(f_threshold$.estimate[[which.max(f_threshold$.estimate)]],2))) +
    geom_line(size = 1, colour = viridisColours[model]) +
    geom_point(data = f_threshold[which.max(f_threshold$.estimate), ], color = "black", size = 3) +
    scale_y_continuous(limits = c(0, 1)) +
    annotate(geom = "text", 
             x = as.numeric(f_threshold[[which.max(f_threshold$.estimate), 1]]), 
             y = as.numeric(f_threshold[[which.max(f_threshold$.estimate), 4]]) - 0.05, 
             label = f_threshold[[which.max(f_threshold$.estimate), 1]]) +
    labs(x = "Threshold", y = "F1-score") +
    theme_bw()
}
plot_grid(plotlist = Plots_ThresholdTuning_2020_BTF, nrow = 1) %>% 
  ggsave(filename = "outputs/figures/F1_Threshold_BTF_2020.png",
         dpi = 800, width = 36, height = 12, units = "cm")

# ├├ DBF 2018 ----

Plots_ThresholdTuning_2018_DBF <- list()
for (model in 1:length(Performance_2018Models_DBF$Predictions)) {
  viridisColours <- viridis::viridis(3)
  
  f_threshold <- list()
  for (threshold in seq(0.01, 0.99, 0.01)) {
    f_threshold[[as.character(threshold)]] <- Performance_2018Models_DBF$Predictions[[model]] %>% 
      mutate(prediction_threshold = factor(ifelse(estimate >= threshold, 1, 0))) %>% 
      yardstick::f_meas(truth = truth, estimate = prediction_threshold, event_level = "second")
  }
  f_threshold <- bind_rows(f_threshold, .id = "threshold")
  
  Plots_ThresholdTuning_2018_DBF[[names(Performance_2018Models_DBF$Predictions[model])]] <- ggplot(data = f_threshold, aes(x = as.numeric(threshold), y = .estimate)) + 
    geom_hline(yintercept = f_threshold$.estimate[which.max(f_threshold$.estimate)], linetype = 'dashed') +
    annotate(geom = "text", 
             x = 0.25, 
             y = as.numeric(f_threshold$.estimate[[which.max(f_threshold$.estimate)]]) + 0.05, 
             label = paste0("F1 = ", round(f_threshold$.estimate[[which.max(f_threshold$.estimate)]],2))) +
    geom_line(size = 1, colour = viridisColours[model]) +
    geom_point(data = f_threshold[which.max(f_threshold$.estimate), ], color = "black", size = 3) +
    scale_y_continuous(limits = c(0, 1)) +
    annotate(geom = "text", 
             x = as.numeric(f_threshold[[which.max(f_threshold$.estimate), 1]]), 
             y = as.numeric(f_threshold[[which.max(f_threshold$.estimate), 4]]) - 0.05, 
             label = f_threshold[[which.max(f_threshold$.estimate), 1]]) +
    labs(x = "Threshold", y = "F1-score") +
    theme_bw()
}
plot_grid(plotlist = Plots_ThresholdTuning_2018_DBF, nrow = 1) %>% 
  ggsave(filename = "outputs/figures/F1_Threshold_DBF_2018.png",
         dpi = 800, width = 36, height = 12, units = "cm")

# ├├ DBF 2020 ----

Plots_ThresholdTuning_2020_DBF <- list()
for (model in 1:length(Performance_2020Models_DBF$Predictions)) {
  viridisColours <- viridis::viridis(3)
  
  f_threshold <- list()
  for (threshold in seq(0.01, 0.99, 0.01)) {
    f_threshold[[as.character(threshold)]] <- Performance_2020Models_DBF$Predictions[[model]] %>% 
      mutate(prediction_threshold = factor(ifelse(estimate >= threshold, 1, 0))) %>% 
      yardstick::f_meas(truth = truth, estimate = prediction_threshold, event_level = "second")
  }
  f_threshold <- bind_rows(f_threshold, .id = "threshold")
  
  Plots_ThresholdTuning_2020_DBF[[names(Performance_2020Models_DBF$Predictions[model])]] <- ggplot(data = f_threshold, aes(x = as.numeric(threshold), y = .estimate)) + 
    geom_hline(yintercept = f_threshold$.estimate[which.max(f_threshold$.estimate)], linetype = 'dashed') +
    annotate(geom = "text", 
             x = 0.25, 
             y = as.numeric(f_threshold$.estimate[[which.max(f_threshold$.estimate)]]) + 0.05, 
             label = paste0("F1 = ", round(f_threshold$.estimate[[which.max(f_threshold$.estimate)]],2))) +
    geom_line(size = 1, colour = viridisColours[model]) +
    geom_point(data = f_threshold[which.max(f_threshold$.estimate), ], color = "black", size = 3) +
    scale_y_continuous(limits = c(0, 1)) +
    annotate(geom = "text", 
             x = as.numeric(f_threshold[[which.max(f_threshold$.estimate), 1]]), 
             y = as.numeric(f_threshold[[which.max(f_threshold$.estimate), 4]]) - 0.05, 
             label = f_threshold[[which.max(f_threshold$.estimate), 1]]) +
    labs(x = "Threshold", y = "F1-score") +
    theme_bw()
}
plot_grid(plotlist = Plots_ThresholdTuning_2020_DBF, nrow = 1) %>% 
  ggsave(filename = "outputs/figures/F1_Threshold_DBF_2020.png",
         dpi = 800, width = 36, height = 12, units = "cm")


# ├├ Combined ----

Plot_ThresholdTuning_Combined <- plot_grid(plotlist = c(Plots_ThresholdTuning_2018_BTF, Plots_ThresholdTuning_2020_BTF,
                                                        Plots_ThresholdTuning_2018_DBF, Plots_ThresholdTuning_2020_DBF),
                                           nrow = 4,
                                           labels = "AUTO")
Plot_ThresholdTuning_Combined <- plot_grid(Plot_ThresholdTuning_Combined, PRCurves_legend,
                                           ncol = 1, rel_heights = c(1, 0.05))

ggsave("outputs/figures/ThresholdTuning_Combined.png", Plot_ThresholdTuning_Combined,
       dpi = 800, width = 18, height = 24, units = "cm")

# ├ Extracting multiple performance metrics at all thresholds ----

PerformanceMetrics <- list()
customMetricSet <- metric_set(recall, precision, f_meas, sensitivity, specificity, kap, bal_accuracy)

Performance_Predictions <- c('Performance_2018Models_BTF', 'Performance_2020Models_BTF', 'Performance_2018Models_DBF', 'Performance_2020Models_DBF')
for (predictions in Performance_Predictions) {
  for (model in 1:length(get(predictions)$Predictions)) {
    
    perfmetrics <- list()
    for (threshold in seq(0.01, 0.99, 0.01)) {
      perfmetrics[[as.character(threshold)]] <- get(predictions)$Predictions[[model]] %>% 
        mutate(prediction_threshold = factor(ifelse(estimate >= threshold, 1, 0))) %>% 
        customMetricSet(truth = truth, estimate = prediction_threshold, event_level = "second")
    }
    perfmetrics <- bind_rows(perfmetrics, .id = "threshold") %>% 
      mutate(species = gsub("_.*", "",names(get(predictions)$Predictions[model])),
             year = gsub("[A-Z]{3}_([0-9]{4})_.*", "\\1",names(get(predictions)$Predictions[model])),
             model = gsub("[A-Z]{3}_[0-9]{4}_(.*)_2022.*", "\\1",names(get(predictions)$Predictions[model])))
    
    PerformanceMetrics <- rbind(PerformanceMetrics, perfmetrics)
  }
}
PerformanceMetrics$model <- gsub("RoadNoiseSurveys_", "", PerformanceMetrics$model)

BestThreshold <- PerformanceMetrics %>% 
  filter(.metric == 'f_meas') %>% 
  group_by(species, year, model) %>% 
  summarise(threshold = threshold[which.max(.estimate)],
            .estimate = .estimate[which.max(.estimate)])

left_join(BestThreshold[,1:4], PerformanceMetrics) %>% pivot_wider(names_from = .metric, values_from = .estimate)


# ├ Predicted vs Observed (per recording) ----

# ├├ BTF 2018 ----
RecordingSummary_2018_BTF <- Performance_2018Models_BTF$Predictions$`BTF_2018_inception_v3_2022-05-21` %>% 
  mutate(Site = gsub(".*/([0-9]{1,2})_.*", "\\1", file),
         Date = as.POSIXct(gsub(".*/[0-9]{1,2}_([0-9]{8})_.*", "\\1", file), format = "%Y%m%d")) %>% 
  group_by(Site, Date) %>% 
  summarise(AP = sum(truth == '1'), 
            PP_0.5 = sum(prediction == '1'),
            PP_0.91 = sum(estimate >= 0.91),
            PP_0.9 = sum(estimate >= 0.9),
            PP_0.99 = sum(estimate >= 0.99))

Plots_PredObs_BTF_2018 <- ggplot(RecordingSummary_2018_BTF, aes(x = PP_0.91, y = AP)) + 
  geom_abline(slope = 1, linetype = 'dashed') + 
  geom_point() +
  coord_cartesian(xlim = c(0, 350), ylim = c(0, 350)) +
  labs(x = "Predicted Positives", y = "Actual Positives") +
  annotate(geom = "text", 
           x = 250, 
           y = 15,
           label = paste0("CCC: ", round(DescTools::CCC(x = RecordingSummary_2018_BTF$PP_0.91, y = RecordingSummary_2018_BTF$AP)$rho.c$est, 3)),
           vjust = 0) +
  theme_bw()


# ├├ BTF 2020 ----
RecordingSummary_2020_BTF <- Performance_2020Models_BTF$Predictions$`BTF_2020_RoadNoiseSurveys_inception_v3_2022-05-22` %>% 
  mutate(Site = gsub(".*/([0-9]{1,2})_.*", "\\1", file),
         Date = as.POSIXct(gsub(".*/[0-9]{1,2}_([0-9]{8})_.*", "\\1", file), format = "%Y%m%d")) %>% 
  group_by(Site, Date) %>% 
  summarise(AP = sum(truth == '1'), 
            PP_0.5 = sum(prediction == '1'),
            PP_0.86 = sum(estimate >= 0.86),
            PP_0.9 = sum(estimate >= 0.9),
            PP_0.99 = sum(estimate >= 0.99))

Plots_PredObs_BTF_2020 <- ggplot(RecordingSummary_2020_BTF, aes(x = PP_0.86, y = AP)) + 
  geom_abline(slope = 1, linetype = 'dashed') + 
  geom_point() +
  coord_cartesian(xlim = c(0, 350), ylim = c(0, 350)) +
  labs(x = "Predicted Positives", y = "Actual Positives") +
  annotate(geom = "text", 
           x = 250, 
           y = 15,
           label = paste0("CCC: ", round(DescTools::CCC(x = RecordingSummary_2020_BTF$PP_0.86, y = RecordingSummary_2020_BTF$AP)$rho.c$est, 3)),
           vjust = 0) +
  theme_bw()


plot_grid(Plots_PredObs_BTF_2018, Plots_PredObs_BTF_2020) %>% 
  ggsave(filename = "outputs/figures/Predicted_vs_Observed_BTF.png",
         dpi = 800, width = 24, height = 12, units = "cm")


# ├├ DBF 2018 ----
RecordingSummary_2018_DBF <- Performance_2018Models_DBF$Predictions$`DBF_2018_inception_v3_2022-06-02` %>% 
  mutate(Site = gsub(".*/([0-9]{1,2})_.*", "\\1", file),
         Date = as.POSIXct(gsub(".*/[0-9]{1,2}_([0-9]{8})_.*", "\\1", file), format = "%Y%m%d")) %>% 
  group_by(Site, Date) %>% 
  summarise(AP = sum(truth == '1'), 
            PP_0.5 = sum(prediction == '1'),
            PP_0.91 = sum(estimate >= 0.91),
            PP_0.9 = sum(estimate >= 0.9),
            PP_0.99 = sum(estimate >= 0.99))

Plots_PredObs_DBF_2018 <- ggplot(RecordingSummary_2018_DBF, aes(x = PP_0.91, y = AP)) + 
  geom_abline(slope = 1, linetype = 'dashed') + 
  geom_point() +
  coord_cartesian(xlim = c(0, 350), ylim = c(0, 350)) +
  labs(x = "Predicted Positives", y = "Actual Positives") +
  annotate(geom = "text", 
           x = 250, 
           y = 15,
           label = paste0("CCC: ", round(DescTools::CCC(x = RecordingSummary_2018_DBF$PP_0.91, y = RecordingSummary_2018_DBF$AP)$rho.c$est, 3)),
           vjust = 0) +
  theme_bw()


# ├├ DBF 2020 ----
RecordingSummary_2020_DBF <- Performance_2020Models_DBF$Predictions$`DBF_2020_RoadNoiseSurveys_inception_v3_2022-05-26` %>% 
  mutate(Site = gsub(".*/([0-9]{1,2})_.*", "\\1", file),
         Date = as.POSIXct(gsub(".*/[0-9]{1,2}_([0-9]{8})_.*", "\\1", file), format = "%Y%m%d")) %>% 
  group_by(Site, Date) %>% 
  summarise(AP = sum(truth == '1'), 
            PP_0.5 = sum(prediction == '1'),
            PP_0.75 = sum(estimate >= 0.75),
            PP_0.9 = sum(estimate >= 0.9),
            PP_0.99 = sum(estimate >= 0.99))

Plots_PredObs_DBF_2020 <- ggplot(RecordingSummary_2020_DBF, aes(x = PP_0.75, y = AP)) + 
  geom_abline(slope = 1, linetype = 'dashed') + 
  geom_point() +
  coord_cartesian(xlim = c(0, 350), ylim = c(0, 350)) +
  labs(x = "Predicted Positives", y = "Actual Positives") +
  annotate(geom = "text", 
           x = 250, 
           y = 15,
           label = paste0("CCC: ", round(DescTools::CCC(x = RecordingSummary_2020_DBF$PP_0.75, y = RecordingSummary_2020_DBF$AP)$rho.c$est, 3)),
           vjust = 0) +
  theme_bw()


plot_grid(Plots_PredObs_DBF_2018, Plots_PredObs_DBF_2020) %>% 
  ggsave(filename = "outputs/figures/Predicted_vs_Observed_DBF.png",
         dpi = 800, width = 24, height = 12, units = "cm")

# ├├ Combined ----

Plot_PredObs_Combined <- plot_grid(Plots_PredObs_BTF_2018, Plots_PredObs_BTF_2020, Plots_PredObs_DBF_2018, Plots_PredObs_DBF_2020,
                                   nrow = 2,
                                   labels = "AUTO")

ggsave(filename = "outputs/figures/Predicted_vs_Observed_Combined.png", plot = Plot_PredObs_Combined,
       dpi = 800, width = 24, height = 24, units = "cm")

# Confusion matrix plots ----

#Confusion matrices for BTF models
Performance_2018Models_BTF$ConfMatrices$`BTF_2018_inception_v3_2022-05-21`$layers[[1]]$aes_params$colour <- 'black'
Performance_2020Models_BTF$ConfMatrices$`BTF_2020_RoadNoiseSurveys_inception_v3_2022-05-22`$layers[[1]]$aes_params$colour <- 'black'

Performance_2018Models_BTF$ConfMatrices$`BTF_2018_inception_v3_2022-05-21`$labels$x <- 'True Class'
Performance_2018Models_BTF$ConfMatrices$`BTF_2018_inception_v3_2022-05-21`$labels$y <- 'Predicted Class'
Performance_2020Models_BTF$ConfMatrices$`BTF_2020_RoadNoiseSurveys_inception_v3_2022-05-22`$labels$x <- 'True Class'
Performance_2020Models_BTF$ConfMatrices$`BTF_2020_RoadNoiseSurveys_inception_v3_2022-05-22`$labels$y <- 'Predicted Class'

ConfusionMatrices_BTF <- plot_grid(Performance_2020Models_BTF$ConfMatrices$`BTF_2020_RoadNoiseSurveys_inception_v3_2022-05-22` + scale_x_discrete(labels = c("Negative", "Positive")) + scale_y_discrete(labels = c("Positive", "Negative")), 
                                   Performance_2018Models_BTF$ConfMatrices$`BTF_2018_inception_v3_2022-05-21` + scale_x_discrete(labels = c("Negative", "Positive")) + scale_y_discrete(labels = c("Positive", "Negative")), 
                                   labels = c("A", "B"))

ggsave(filename = "outputs/figures/ConfusionMatrices_BTF_inception_v3.png",
       plot = ConfusionMatrices_BTF,
       width = 18, height = 6, units = "cm", dpi = 800)

#Confusion matrices for DBF models
Performance_2018Models_DBF$ConfMatrices$`DBF_2018_inception_v3_2022-06-02`$layers[[1]]$aes_params$colour <- 'black'
Performance_2020Models_DBF$ConfMatrices$`DBF_2020_RoadNoiseSurveys_inception_v3_2022-05-26`$layers[[1]]$aes_params$colour <- 'black'

Performance_2018Models_DBF$ConfMatrices$`DBF_2018_inception_v3_2022-06-02`$labels$x <- 'True Class'
Performance_2018Models_DBF$ConfMatrices$`DBF_2018_inception_v3_2022-06-02`$labels$y <- 'Predicted Class'
Performance_2020Models_DBF$ConfMatrices$`DBF_2020_RoadNoiseSurveys_inception_v3_2022-05-26`$labels$x <- 'True Class'
Performance_2020Models_DBF$ConfMatrices$`DBF_2020_RoadNoiseSurveys_inception_v3_2022-05-26`$labels$y <- 'Predicted Class'

ConfusionMatrices_DBF <- plot_grid(Performance_2020Models_DBF$ConfMatrices$`DBF_2020_RoadNoiseSurveys_inception_v3_2022-05-26` + scale_x_discrete(labels = c("Negative", "Positive")) + scale_y_discrete(labels = c("Positive", "Negative")), 
                                   Performance_2018Models_DBF$ConfMatrices$`DBF_2018_inception_v3_2022-06-02` + scale_x_discrete(labels = c("Negative", "Positive")) + scale_y_discrete(labels = c("Positive", "Negative")), 
                                   labels = c("A", "B"))

ggsave(filename = "outputs/figures/ConfusionMatrices_DBF_inception_v3.png",
       plot = ConfusionMatrices_DBF,
       width = 18, height = 6, units = "cm", dpi = 800)

#Combined figure

ConfusionMatrices_Combined <- plot_grid(Performance_2020Models_BTF$ConfMatrices$`BTF_2020_RoadNoiseSurveys_inception_v3_2022-05-22` + scale_x_discrete(labels = c("Negative", "Positive")) + scale_y_discrete(labels = c("Positive", "Negative")), 
                                        Performance_2018Models_BTF$ConfMatrices$`BTF_2018_inception_v3_2022-05-21` + scale_x_discrete(labels = c("Negative", "Positive")) + scale_y_discrete(labels = c("Positive", "Negative")), 
                                        Performance_2020Models_DBF$ConfMatrices$`DBF_2020_RoadNoiseSurveys_inception_v3_2022-05-26` + scale_x_discrete(labels = c("Negative", "Positive")) + scale_y_discrete(labels = c("Positive", "Negative")), 
                                        Performance_2018Models_DBF$ConfMatrices$`DBF_2018_inception_v3_2022-06-02` + scale_x_discrete(labels = c("Negative", "Positive")) + scale_y_discrete(labels = c("Positive", "Negative")), 
                                        labels = c("A", "B", "C", "D"))

ggsave(filename = "outputs/figures/ConfusionMatrices_Combined_inception_v3.png",
       plot = ConfusionMatrices_Combined,
       width = 18, height = 12, units = "cm", dpi = 800)