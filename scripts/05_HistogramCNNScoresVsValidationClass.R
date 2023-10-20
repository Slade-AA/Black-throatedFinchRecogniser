library(tidyverse)

# Histograms of CNN scores vs Validation class ----

# ├ Labelled top200 ----

# ├├ BTF ----

BTF_Total <- read.csv("E:/BTF_Detections/BTF_top200_labelled.csv")

SurveysWithBTF <- unique(BTF_Total[which(BTF_Total$MANUAL.ID == 'Yes'),18:19])

semi_join(x = BTF_Total, y = SurveysWithBTF, by = c("Site","Year")) %>% 
  mutate(MANUAL.ID = fct_collapse(MANUAL.ID, No = c("No", "DBF", "Unsure"))) %>% 
  ggplot(aes(x = BTF_combined_inception_v3_2022.05.31.h5, fill = MANUAL.ID)) + 
  geom_histogram() +
  labs(x = "Confidence score", y = "Count") +
  scale_fill_manual(values = c("#868686FF", "#A73030FF"), name = "Label", labels = c("False-positive", "True-positive")) +
  facet_wrap(~Year+Site, scales = "free", ncol = 5, nrow = 2) +
  theme_bw()

ggsave(filename = "manuscript/02_Animal Conservation/Figure SX_BTFConfidenceHistograms.png",
       width = 24, height = 9, units = "cm", dpi = 800)


# ├├ DBF ----

DBF_Total <- read.csv("E:/BTF_Detections/DBF_top200_labelled.csv")

SurveysWithDBF <- unique(DBF_Total[which(DBF_Total$MANUAL.ID == 'Yes'),18:19])

semi_join(x = DBF_Total, y = SurveysWithDBF, by = c("Site","Year")) %>% 
  mutate(MANUAL.ID = fct_collapse(MANUAL.ID, No = c("No", "Unsure"))) %>% 
  ggplot(aes(x = DBF_combined_inception_v3_2022.06.05.h5, fill = MANUAL.ID)) + 
  geom_histogram() +
  labs(x = "Confidence score", y = "Count") +
  scale_fill_manual(values = c("#868686FF", "#A73030FF"), name = "Label", labels = c("False-positive", "True-positive")) +
  facet_wrap(~Year+Site, scales = "free") +
  theme_bw()

ggsave(filename = "manuscript/02_Animal Conservation/Figure SX_DBFConfidenceHistograms.png",
       width = 24, height = 21, units = "cm", dpi = 800)


# Number of sites occupied by validation effort ----

NumSitesOccupied <- data.frame()

# ├ BTF ----

for (val in seq(1,200,1)) {
  NumSitesOccupied <- bind_rows(NumSitesOccupied,
                                data.frame(n = BTF_Total %>% 
                                             filter(grepl("^D", Site)) %>% #just keeping DEU sites
                                             group_by(Site, Year) %>% 
                                             slice_max(order_by = BTF_combined_inception_v3_2022.05.31.h5, n = val) %>% 
                                             summarise(n = ifelse(sum(MANUAL.ID == 'Yes') >= 1, 1, 0)) %>% ungroup() %>% select(n) %>% sum(),
                                           val = val,
                                           species = "BTF"))
  
}



# ├ DBF ----

for (val in seq(1,200,1)) {
  NumSitesOccupied <- bind_rows(NumSitesOccupied,
                                data.frame(n = DBF_Total %>%
                                             filter(grepl("^D", Site)) %>% #just keeping DEU sites
                                             group_by(Site, Year) %>% 
                                             slice_max(order_by = DBF_combined_inception_v3_2022.06.05.h5, n = val) %>% 
                                             summarise(n = ifelse(sum(MANUAL.ID == 'Yes') >= 1, 1, 0)) %>% ungroup() %>% select(n) %>% sum(),
                                           val = val,
                                           species = "DBF"))
  
}

#Combined plot represented as percentage
NumSitesOccupied %>% 
  group_by(species) %>% 
  mutate(n = (n/max(n))*100) %>% 
  ggplot(aes(x = val, y = n, colour = species)) +
  geom_line(linewidth = 1) +
  ggsci::scale_color_jco() +
  scale_y_continuous(limits = c(0, 100)) +
  labs(x = "Number of detections validated", y = "Percentage of total occupied sites") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank())

ggsave(filename = "manuscript/02_Animal Conservation/Figure X.png",
       width = 12, height = 13, units = "cm", dpi = 800)