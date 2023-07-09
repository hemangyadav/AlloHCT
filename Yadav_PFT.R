# Load required packages
library(tidyverse)
library(survival)
library(tidycmprsk)
library(ggsurvfit)
library(survminer)

set.seed(1234) # Can be any number - use a consistent number to generate test/training sets 

studypop <- read_csv('StudyPop.csv') # Dataset 
glimpse(studypop)

# Develop training and test datasets 
studypop <- studypop %>%
  mutate(n = row_number()) %>% # Create row number 
  select(n, everything())

train <- studypop %>%
  group_by(ICU120d, IMV120d) %>% #any number of variables you wish to partition by proportionally
  sample_frac(.7) # '0.7' is the proportion of the original df you wish to sample
test <- anti_join(studypop, train) # creates test dataframe with those observations not in 'train.'

# Create individual Cox proportional hazard equations for each PFT variable. 
model.fev1.os <- coxph(Surv(SurvivalTime, OS) ~ FEV1Zscore, data = train)
model.fev1.icu <- coxph(Surv(DaysToICU, ICU120d) ~ FEV1Zscore, data = train)
model.fev1.imv <- coxph(Surv(IMVDays, IMV120d) ~ FEV1Zscore, data = train)
model.dlco.os <- coxph(Surv(SurvivalTime, OS) ~ DLCOZscore, data = train)
model.dlco.icu <- coxph(Surv(DaysToICU, ICU120d) ~ DLCOZscore, data = train) 
model.dlco.imv <- coxph(Surv(IMVDays, IMV120d) ~ DLCOZscore, data = train)
model.fvc.os <- coxph(Surv(SurvivalTime, OS) ~ FVCZscore, data = train)
model.fvc.icu <- coxph(Surv(DaysToICU, ICU120d) ~ FVCZscore, data = train) 
model.fvc.imv <- coxph(Surv(IMVDays, IMV120d) ~ FVCZscore, data = train)
model.os <- coxph(Surv(SurvivalTime, OS) ~ FEV1Zscore + DLCOZscore + AgeAtTransplant, data = train)
model.icu <- coxph(Surv(DaysToICU, ICU120d) ~ FEV1Zscore + DLCOZscore + AgeAtTransplant, data = train) 
model.imv <- coxph(Surv(IMVDays, IMV120d) ~ FEV1Zscore + DLCOZscore + AgeAtTransplant, data = train) 

# Visualize different relationships based on the Cox hazard equations above. 'Dummydata' is a set of variables that is used to plot these relationships based on the equations developed above. 
dummydata.fev1 <- data.frame (FEV1Zscore = seq(-3, 3, by = 0.01))
dummydata.fev1$lp.os <- predict(model.fev1.os, dummydata.fev1, type = "lp")
dummydata.fev1$lp.icu <- predict(model.fev1.icu, dummydata.fev1, type = "lp")
dummydata.fev1$lp.imv <- predict(model.fev1.imv, dummydata.fev1, type = "lp")
dummydata.fev1$HR.os <- exp(dummydata.fev1$lp.os)
dummydata.fev1$HR.icu <- exp(dummydata.fev1$lp.icu)
dummydata.fev1$HR.imv <- exp(dummydata.fev1$lp.imv)
dummydata.fvc <- data.frame (FVCZscore = seq(-3, 3, by = 0.01))
dummydata.fvc$lp.os <- predict(model.fvc.os, dummydata.fvc, type = "lp")
dummydata.fvc$lp.icu <- predict(model.fvc.icu, dummydata.fvc, type = "lp")
dummydata.fvc$lp.imv <- predict(model.fvc.imv, dummydata.fvc, type = "lp")
dummydata.fvc$HR.os <- exp(dummydata.fvc$lp.os)
dummydata.fvc$HR.icu <- exp(dummydata.fvc$lp.icu)
dummydata.fvc$HR.imv <- exp(dummydata.fvc$lp.imv)
dummydata.dlco <- data.frame (DLCOZscore = seq(-4, 2, by = 0.01))
dummydata.dlco$lp.os <- predict(model.dlco.os, dummydata.dlco, type = "lp")
dummydata.dlco$lp.icu <- predict(model.dlco.icu, dummydata.dlco, type = "lp")
dummydata.dlco$lp.imv <- predict(model.dlco.imv, dummydata.dlco, type = "lp")
dummydata.dlco$HR.os <- exp(dummydata.dlco$lp.os)
dummydata.dlco$HR.icu <- exp(dummydata.dlco$lp.icu)
dummydata.dlco$HR.imv <- exp(dummydata.dlco$lp.imv)

dummydata.fev1 %>% 
  dplyr::select(FEV1Zscore, HR.os:HR.imv) %>%
  pivot_longer(cols = HR.os:HR.imv) %>%
  ggplot(aes(x = FEV1Zscore, y = value, color = name)) +
  geom_line(linewidth = 2, show.legend = T) + 
  scale_color_manual(labels=c("ICU admission (120 days)",
                              "Mechanical ventilation (120 days)",
                              "Overall survival"),
                     values = c("aquamarine3", "darkred", "blue")) + 
  theme_bw() + 
  ylim(0,4) +
  geom_hline(yintercept = 1.0, linetype = "dashed") +
  labs(title = "Forced expiratory volume in 1 second", x = "FEV1 Z-score", y = "Hazard Ratio") +
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        legend.text=element_text(size=14),
        plot.title = element_text(hjust = 0.5, face = "bold"))
  
dummydata.fvc %>% 
  dplyr::select(FVCZscore, HR.os:HR.imv) %>%
  pivot_longer(cols = HR.os:HR.imv) %>%
  ggplot(aes(x = FVCZscore, y = value, color = name)) +
  geom_line(linewidth = 2) + 
  scale_color_manual(labels=c("ICU admission (120 days)",
                              "Mechanical ventilation (120 days)",
                              "Overall survival"),
                     values = c("aquamarine3", "darkred", "blue")) + 
  theme_bw() + 
  ylim(0,4) +
  geom_hline(yintercept = 1.0, linetype = "dashed") +
  labs(title = "Forced vital capacity", x = "FVC Z-score", y = "Hazard Ratio") +
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        legend.text=element_text(size=14),
        plot.title = element_text(hjust = 0.5, face = "bold")) + 
  guides(color = guide_legend(order = 1, nrow=3))

dummydata.dlco %>% 
  dplyr::select(DLCOZscore, HR.os:HR.imv) %>%
  pivot_longer(cols = HR.os:HR.imv) %>%
  ggplot(aes(x = DLCOZscore, y = value, color = name)) +
  geom_line(linewidth = 2, show.legend = T) + 
  scale_color_manual(labels=c("ICU admission (120 days)",
                              "Mechanical ventilation (120 days)",
                              "Overall survival"),
                     values = c("aquamarine3", "darkred", "blue")) + 
  theme_bw() + 
  ylim(0,4) +
  geom_hline(yintercept = 1.0, linetype = "dashed") +
  labs(title = "Diffusing Capacity", x = "DLCO Z-score", y = "Hazard Ratio") +
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        legend.text=element_text(size=14),
        plot.title = element_text(hjust = 0.5, face = "bold"))

# Visualize HR distributions in a histogram that can help stratify patients into high/normal/low risk
train$lp.os <- predict(model.os, train, type = "lp")
train$lp.icu <- predict(model.icu, train, type = "lp")
train$lp.imv <- predict(model.imv, train, type = "lp")
train$hr.os <- exp(train$lp.os)
train$hr.icu <- exp(train$lp.icu)
train$hr.imv <- exp(train$lp.imv)

train <- train %>% 
  mutate(risk.strata.os = ifelse(hr.os >= 1.3, "High Risk",
                                 ifelse(hr.os <= 0.7, "Low Risk",
                                        "Normal Risk"))) %>%
  mutate(risk.strata.icu = ifelse(hr.icu >= 1.3, "High Risk",
                                  ifelse(hr.icu <= 0.7, "Low Risk",
                                         "Normal Risk"))) %>%
  mutate(risk.strata.imv = ifelse(hr.imv >= 1.3, "High Risk",
                                  ifelse(hr.imv <= 0.7, "Low Risk",
                                         "Normal Risk")))

train$risk.strata.os <- factor(train$risk.strata.os, levels = c("Low Risk", "Normal Risk", "High Risk"))
train$risk.strata.icu <- factor(train$risk.strata.icu, levels = c("Low Risk", "Normal Risk", "High Risk"))
train$risk.strata.imv <- factor(train$risk.strata.imv, levels = c("Low Risk", "Normal Risk", "High Risk"))

train %>%
  ggplot(aes(x = hr.os)) +
  geom_histogram(bins = 100, aes(fill = risk.strata.os)) + theme_bw() + 
  xlim(0,5) + 
  labs(title = "Predicted hazard ratios for survival", x = "Hazard Ratio", y = "Frequency", fill = "Estimated Risk") +
  scale_fill_manual(labels=c("Low", "Normal", "High"), values = c("#31a354", "#7fcdbb", "#f03b20")) +
  theme(legend.position = "none")

train %>%
  ggplot(aes(x = hr.icu)) +
  geom_histogram(bins = 100, aes(fill = risk.strata.icu)) + theme_bw() + 
  xlim(0,5) + 
  labs(title = "Predicted hazard ratios for ICU admission", x = "Hazard Ratio", y = "Frequency", fill = "Estimated Risk") + 
  scale_fill_manual(labels=c("Low", "Normal", "High"), values = c("#31a354", "#7fcdbb", "#f03b20")) +
  theme(legend.position = "none")

train %>%
  ggplot(aes(x = hr.imv)) +
  geom_histogram(bins = 100, aes(fill = risk.strata.imv)) + theme_bw() + 
  xlim(0,5) + 
  labs(title = "Predicted hazard ratios for mechanical ventilation", x = "Hazard Ratio", y = "Frequency", fill = "Estimated Risk") + 
  scale_fill_manual(labels=c("Low (HR < 0.7)", "Normal (HR 0.7 - 1.3)", "High (HR > 1.3)"), values = c("#31a354", "#7fcdbb", "#f03b20")) +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 11), 
        legend.text = element_text(size = 11))

#### Testing the prediction model in the test cohort. 
test$lp.os <- predict(model.os, test, type = "lp")
test$lp.icu <- predict(model.icu, test, type = "lp")
test$lp.imv <- predict(model.imv, test, type = "lp")
test$hr.os <- exp(test$lp.os)
test$hr.icu <- exp(test$lp.icu)
test$hr.imv <- exp(test$lp.imv)

# Assign patients in the test cohort to risk strata (low, medium, high risk) 
test <- test %>% 
  mutate(risk.strata.os = ifelse(hr.os >= 1.3, "High Risk",
                                 ifelse(hr.os <= 0.7, "Low Risk",
                                        "Normal Risk"))) %>%
  mutate(risk.strata.icu = ifelse(hr.icu >= 1.3, "High Risk",
                                  ifelse(hr.icu <= 0.7, "Low Risk",
                                         "Normal Risk"))) %>%
  mutate(risk.strata.imv = ifelse(hr.imv >= 1.3, "High Risk",
                                  ifelse(hr.imv <= 0.7, "Low Risk",
                                         "Normal Risk")))
test$risk.strata.os <- factor(test$risk.strata.os, levels = c("Low Risk", "Normal Risk", "High Risk"))
test$risk.strata.icu <- factor(test$risk.strata.icu, levels = c("Low Risk", "Normal Risk", "High Risk"))
test$risk.strata.imv <- factor(test$risk.strata.imv, levels = c("Low Risk", "Normal Risk", "High Risk"))

test$OS <- factor(test$OS, levels = c(0,1))
test$ICU120d <- factor(test$ICU120d, levels = c(0,1))
test$IMV120d <- factor(test$IMV120d, levels = c(0,1))

# Generate cumulative incidence curves to test the performance of the risk strata 
cuminc(Surv(SurvivalTime, OS) ~ risk.strata.os, data = test) %>%
  ggcuminc(outcome = 1, linewidth = 1) + 
  labs(title = "Mortality", x = "Time from Transplant (years)", y = "Cumulative risk of death") +
  scale_color_manual(values = c("#117733",  "#FFAA33", "#CC6677"))  + 
  scale_y_continuous(
    limits = c(0, 0.80),
    labels = scales::percent,
    expand = c(0.01, 0)
  ) +
  coord_cartesian (x = c(0,5)) +
  theme_ggsurvfit_KMunicate() + 
  add_pvalue(caption = "{p.value}", location  = "annotation", x = 4.5, y = 0.04, size = 5) +
  theme(legend.position = "none", 
        legend.text = element_text(size = 14, color = "black", face = "plain"),
        axis.title = element_text(size = 12, color = "black", face = "plain"),
        axis.text = element_text(size = 12, color = "black", face = "plain"),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5))

cuminc(Surv(DaysToICU, ICU120d) ~ risk.strata.icu, data = test) %>%
  ggcuminc(outcome = 1, linewidth = 1) + 
  labs(title = "ICU admission (120 days)", x = "Time from Transplant (days)", y = "Cumulative risk of ICU Admission") +
  scale_color_manual(values = c("#117733",  "#FFAA33", "#CC6677"))  + 
  scale_y_continuous(
    limits = c(0, 0.50),
    labels = scales::percent,
    expand = c(0.01, 0)
  ) + 
  scale_x_continuous(breaks = c(20, 40, 60, 80, 100, 120)) + 
  theme_ggsurvfit_KMunicate() + 
  add_pvalue(caption = "{p.value}", location  = "annotation", x = 100, y = 0.03, size = 5) +
  theme(legend.position = "bottom", 
        legend.text = element_text(size = 14, color = "black", face = "plain"),
        axis.title = element_text(size = 12, color = "black", face = "plain"),
        axis.text = element_text(size = 12, color = "black", face = "plain"),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5)) 

cuminc(
  Surv(IMVDays, IMV120d) ~ risk.strata.imv, data = test) %>%
  ggcuminc(outcome = 1, linewidth = 1) + 
  labs(title = "Mechanical ventilation (120 days)", x = "Time from Transplant (days)", y = "Cumulative risk of Mechanical Ventilation") +
  scale_color_manual(values = c("#117733",  "#FFAA33", "#CC6677"))  + 
  scale_y_continuous(
    limits = c(0, 0.30),
    labels = scales::percent,
    expand = c(0.01, 0)
  ) + 
  scale_x_continuous(breaks = c(20, 40, 60, 80, 100, 120)) + 
  theme_ggsurvfit_KMunicate() + 
  add_pvalue(caption = "{p.value}", location  = "annotation", x = 100, y = 0.01, size = 5) +
  theme(legend.position = "none", 
        legend.text = element_text(size = 14, color = "black", face = "plain"),
        axis.title = element_text(size = 12, color = "black", face = "plain"),
        axis.text = element_text(size = 12, color = "black", face = "plain"),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5)) 

