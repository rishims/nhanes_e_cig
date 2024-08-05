# Discordance between self-reported e-cigarette use and measured cotinine levels in non-smoking e-cigarette users
# Rishi Shah and Adith Arun
# July 21, 2024
# Study period is 2013-2020 and data source is the National Health and Nutrition Examination Survey: https://wwwn.cdc.gov/nchs/nhanes/continuousnhanes/default.aspx

# load necessary libraries
library(nhanesA)
library(tidyverse)
library(survey)
library(ggplot2)
library(ggsci)
library(ggpubr)
library(gtsummary)
library(tibble)
library(pROC)
library(boot)

# import data -----
# import demographic data
demo_data_13_14 <- nhanes("DEMO_H") # demographic data for 2013-2014
demo_data_15_16 <- nhanes("DEMO_I") # demographic data for 2015-2016
demo_data_17_20 <- nhanes("P_DEMO") # demographic data for 2017-2020

# import recent tobacco use data
smoke_data_13_14 <- nhanes("SMQRTU_H") # recent tobacco use 2013-2014
smoke_data_15_16 <- nhanes("SMQRTU_I") # recent tobacco use 2015-2016
smoke_data_17_20 <- nhanes("P_SMQRTU") # recent tobacco use 2017-2020

# import at home smoker data
shs_data_13_14 <- nhanes("SMQFAM_H") # household smoking data for 2013-2014
shs_data_15_16 <- nhanes("SMQFAM_I") # household smoking data for 2015-2016
shs_data_17_20 <- nhanes("P_SMQFAM") # household smoking data for 2017-2020

# import serum cotinine data
cot_data_13_14 <- nhanes("COT_H") # serum cotinine data for 2013-2014
cot_data_15_16 <- nhanes("COT_I") # serum cotinine data for 2015-2016
cot_data_17_20 <- nhanes("P_COT") # serum cotinine data for 2017-2020

# clean and merge data ------
# demographics data
# demographic variables
vars_13_16 <- c(
  "SEQN",       # Respondent sequence number
  "SDDSRVYR",   # Data release cycle
  "RIDSTATR",   # Interview/Examination status
  "RIAGENDR",   # Gender
  "RIDAGEYR",   # Age in years at screening
  "RIDRETH1",   # Race/Hispanic origin
  "RIDRETH3",   # Race/Hispanic origin w/ NH Asian
  "DMDEDUC2",   # Education level - Adults 20+
  "DMDMARTL",   # Marital status
  "INDFMPIR",   # Ratio of family income to poverty
  "SDMVPSU",    # Masked variance pseudo-PSU
  "SDMVSTRA",   # Masked variance pseudo-stratum
  "WTINT2YR",   # Full sample 2 year interview weight
  "WTMEC2YR"    # Full sample 2 year MEC exam weight
)

vars_17_20 <- c(
  "SEQN",       # Respondent sequence number
  "SDDSRVYR",   # Data release cycle
  "RIDSTATR",   # Interview/Examination status
  "RIAGENDR",   # Gender
  "RIDAGEYR",   # Age in years at screening
  "RIDRETH1",   # Race/Hispanic origin
  "RIDRETH3",   # Race/Hispanic origin w/ NH Asian
  "DMDEDUC2",   # Education level - Adults 20+
  "DMDMARTZ",   # Marital status
  "INDFMPIR",   # Ratio of family income to poverty
  "SDMVPSU",    # Masked variance pseudo-PSU
  "SDMVSTRA",   # Masked variance pseudo-stratum
  "WTINTPRP",   # Full sample interview weight
  "WTMECPRP"    # Full sample MEC exam weight
)

# match demographic variable names across survey cycles
demo_data_13_14 <- demo_data_13_14 %>%
  select(all_of(vars_13_16)) %>%
  rename(
    DMDMARTZ = DMDMARTL,  
    WTINTPRP = WTINT2YR,  
    WTMECPRP = WTMEC2YR   
  )

demo_data_15_16 <- demo_data_15_16 %>%
  select(all_of(vars_13_16)) %>%
  rename(
    DMDMARTZ = DMDMARTL,  
    WTINTPRP = WTINT2YR,  
    WTMECPRP = WTMEC2YR   
  )

demo_data_17_20 <- demo_data_17_20 %>%
  select(all_of(vars_17_20))


# adjust survey weights for pooled year analyses: https://www.cdc.gov/nchs/data/series/sr_02/sr02-190.pdf
demo_data_13_14 <- demo_data_13_14 %>% mutate(WTADJ = WTINTPRP * (2 / 7.2), WTADJMEC = WTMECPRP * (2 / 7.2))
demo_data_15_16 <- demo_data_15_16 %>% mutate(WTADJ = WTINTPRP * (2 / 7.2), WTADJMEC = WTMECPRP * (2 / 7.2))
demo_data_17_20 <- demo_data_17_20 %>% mutate(WTADJ = WTINTPRP * (3.2 / 7.2), WTADJMEC = WTMECPRP * (3.2 / 7.2))

# match cotinine data variables across survey cycles
cot_data_17_20 <- cot_data_17_20 %>% rename(LBXHCT = LBXHCOT, LBDHCTLC = LBDHCOLC)

# select shs data
shs_data_13_14 <- shs_data_13_14 %>% select("SEQN", "SMD470") # of people who smoke inside respondent's home
shs_data_15_16 <- shs_data_15_16 %>% select("SEQN", "SMD470") # of people who smoke inside respondent's home
shs_data_17_20 <- shs_data_17_20 %>% select("SEQN", "SMD470") # of people who smoke inside respondent's home

# smoking variables
vars <- c(
  "SEQN",    # Respondent sequence number
  "SMQ681",  # Smoked tobacco last 5 days?
  "SMQ690A", # Used last 5 days - Cigarettes
  "SMQ710",  # Number of days smoked cigarettes in the last 5 days
  "SMQ720",  # Number of cigarettes smoked per day
  "SMQ725",  # When did the respondent smoke the last cigarette?
  "SMQ690B", # Used last 5 days - Pipes
  "SMQ740",  # Number of days smoked pipe in the last 5 days
  "SMQ690C", # Used last 5 days - Cigars
  "SMQ770",  # Number of days smoked cigars in the last 5 days
  "SMQ690G", # Used last 5 days - Hookah, water pipes
  "SMQ845",  # Number of days smoked water pipe in the last 5 days
  "SMQ690H", # Used last 5 days - E-cigarettes
  "SMQ849",  # Number of days smoked e-cigarette in the last 5 days
  "SMQ851",  # Used smokeless tobacco last 5 days
  "SMQ863",  # Used nicotine replacement last 5 days
  "SMDANY"   # Used any tobacco product last 5 days
)

smoke_data_13_14 <- smoke_data_13_14 %>% select(all_of(vars))
smoke_data_15_16 <- smoke_data_15_16 %>% select(all_of(vars))
smoke_data_17_20 <- smoke_data_17_20 %>% select(all_of(vars))

# bind demographics data
demographics <- bind_rows(demo_data_13_14, demo_data_15_16, demo_data_17_20)

# bind serum cotinine data
cotinine <- bind_rows(cot_data_13_14, cot_data_15_16, cot_data_17_20)

# bind shs data
shs <- bind_rows(shs_data_13_14, shs_data_15_16, shs_data_17_20)

# bind recent tobacco use data
e_cigarette <- bind_rows(smoke_data_13_14, smoke_data_15_16, smoke_data_17_20)

# join all data
data <- left_join(demographics, cotinine, by = "SEQN")
data <- left_join(data, e_cigarette, by = "SEQN")
data <- left_join(data, shs, by = "SEQN")

# analysis ----
# exclude all smokers (other than e-cigarette users)
data <- data %>% mutate(ONLY_VAPES = case_when(!is.na(LBDCOTLC) & (SMQ863 == "No" | is.na(SMQ863)) & SMQ851 == "No" & SMQ681 == "Yes" & SMQ690H == 8 & is.na(SMQ690A) & is.na(SMQ690B) & is.na(SMQ690C) & is.na(SMQ690G) ~ 1,
                                              TRUE ~ 0)) # SMQ690A != 1 & SMQ690B != 2 & SMQ690C != 3 & SMQ690G != 7, also does smokeless tobacco matter?
                                                         # !is.na(LBDCOTLC) & LBDCOTLC == "At or above the detection limit" (23 NAs < 10% of 244, so do not need to adjust sample weights per NCHS recommendations: https://wwwn.cdc.gov/nchs/nhanes/tutorials/datasets.aspx)

data <- data %>% mutate(SHS_EXPOSURE = case_when(SMD470 %in% c("1 household member smokes inside the house", 
                                                               "2 household members smoke inside the house",
                                                               "2 or more household members smoke inside the house",
                                                               "3 or more household members smoke inside the house") ~ 1,
                                                 TRUE ~ 0))

data <- data %>% mutate(NON_SMOKER = case_when(SMQ681 == "No" & SMQ851 == "No" & SMQ863 == "No" ~ 1,
                                               TRUE ~ 0))

# cotinine level analysis
# create full survey design: https://wwwn.cdc.gov/nchs/nhanes/tutorials/VarianceEstimation.aspx
full_design <- svydesign(
  id = ~SDMVPSU,
  strata = ~SDMVSTRA,
  weights = ~WTADJMEC,
  data = data,
  nest = TRUE
)

svy_cotinine <- subset(full_design, ONLY_VAPES == 1 & SHS_EXPOSURE == 0) # data %>% filter(ONLY_VAPES == 1) # lower limit of detection for cotinine is 0.015 ng/mL

data_svy <- svyby(~LBXCOT, ~SMQ849, svy_cotinine, svyquantile, quantiles = c(0.05, 0.25, 0.5, 0.75, 0.95), ci = TRUE)
data_svy <- as.data.frame(data_svy)

# survey-adjusted distribution of cotinine levels by # of days last used e-cigarette in the last 5 days
survey_cotinine <- ggplot(data_svy, aes(x = as.factor(SMQ849), y = LBXCOT.0.5)) +
  geom_boxplot(aes(
    lower = LBXCOT.0.25, middle = LBXCOT.0.5, upper = LBXCOT.0.75,
    ymin = LBXCOT.0.05, ymax = LBXCOT.0.95, fill = as.factor(SMQ849)
  ), stat = "identity", alpha = 0.5) +
  scale_fill_jama() +
  scale_y_continuous(trans = "log10", 
                     breaks = c(0.01, 0.1, 1, 10, 100, 1000),
                     labels = c("0.01", "0.1", "1", "10", "100", "1000"), limits = c(0.01, 1000)) +
  labs(
    title = "National Estimates of Serum Cotinine Levels by\nDays of E-cigarette Use in Last 5 Days",
    x = "Number of Days Used E-cigarette in Last 5 Days",
    y = "Serum Cotinine Levels (ng/mL)", # expression(paste("Log"[10], "-Transformed 
    fill = "Days of E-cigarette Use"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "none"
  )

# raw distribution of cotinine levels by # of days last used e-cigarette in the last 5 days
raw_cotinine <- ggplot(data, aes(x = as.factor(SMQ849), y = LBXCOT)) +
  geom_boxplot(aes(fill = as.factor(SMQ849))) +
  scale_fill_jama() +
  labs(
    title = "Raw Serum Cotinine Levels by\nDays of E-cigarette Use in Last 5 Days",
    x = "Number of Days Used E-cigarette in Last 5 Days",
    y = "Serum Cotinine Levels (ng/mL)",
    fill = "Days of E-cigarette Use"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "none"
  )


# regression
# Q: What are factors associated with *detectable* e-cigarette use among the US non-actively tobacco-using population?
# covariates 

# race/ethnicity, RIDRETH1 is already factored
# gender, RIAGENDR is already factored

# age
data$AGECAT <- NA
data$AGECAT[data$RIDAGEYR >= 12 & data$RIDAGEYR <= 18] <- 1
data$AGECAT[data$RIDAGEYR >= 19 & data$RIDAGEYR <= 25] <- 2
data$AGECAT[data$RIDAGEYR >= 26] <- 3

levels <- c("12-18", "19-25", "≥26")
labels <- c(1, 2, 3)
data$AGECAT <- factor(data$AGECAT, levels = labels, labels = levels)

# income
# binarize low and middle/high family income
# low = < 200% of the federal poverty limit = 0
# middle/high = >= 200% of the federal poverty limit = 1
data$INC_BINARY <- NA
data$INC_BINARY[data$INDFMPIR <= 1.00] <- 0
data$INC_BINARY[data$INDFMPIR > 1.00] <- 1

levels <- c("Low income", "High income")
labels <- c(0, 1)
data$INC_BINARY <- factor(data$INC_BINARY, levels = labels, labels = levels)

# create sample summary table
# subset data to only include exclusive e-cigarette users and non-tobacco users
data_filtered <- data %>% filter(
  (ONLY_VAPES == 1 | NON_SMOKER == 1) & SHS_EXPOSURE == 0
)

t1_all <- (Table1_all_format <- tbl_summary(
  data = data_filtered,
  by = "ONLY_VAPES",
  statistic = list(
    AGECAT ~ "{n} ({p})",
    RIAGENDR ~ "{n} ({p})",
    RIDRETH1 ~ "{n} ({p})",
    INC_BINARY ~ "{n} ({p})"),
  missing_text = "Missing",
  include = c(AGECAT, RIAGENDR, RIDRETH1, INC_BINARY)
)) %>% add_p(pvalue_fun = function(x) style_pvalue(x, digits = 3))

# survey-ify data
data2 <- as_survey(data, 
                   id = SDMVPSU, 
                   weight = WTADJMEC,
                   strata = SDMVSTRA, nest = TRUE)

# make summary table with national estimates (abridged)
tbl_svysummary(subset(data2, (ONLY_VAPES == 1 | NON_SMOKER == 1) & SHS_EXPOSURE == 0), by = ONLY_VAPES, 
               include = c(AGECAT, RIAGENDR, RIDRETH1, INC_BINARY)
) %>% add_p()

# fit regression model
fit <- svyglm(ONLY_VAPES ~ 
                relevel(RIDRETH1, ref = "Non-Hispanic White") + 
                relevel(RIAGENDR, ref = "Male") +
                relevel(INC_BINARY, ref = "High income") + 
                relevel(AGECAT, ref = "≥26"),
              design = subset(full_design, (ONLY_VAPES == 1 | NON_SMOKER == 1) & SHS_EXPOSURE == 0),
              family = quasibinomial(link = 'logit'))

logistic.display(fit)

# roc analysis
# Q: What is the optimal cutpoint to distinguish between self-reported exclusive e-cig users from non–tobacco users?
data_filtered <- as_tibble(data_filtered)

df <- data_filtered %>% select(SHS_EXPOSURE, NON_SMOKER, ONLY_VAPES, LBXCOT, SMQ849, AGECAT, INC_BINARY, RIAGENDR, RIDRETH1)

# filter for SHS_EXPOSURE == 0 and non-missing cotinine levels
df <- df %>% filter(SHS_EXPOSURE == 0) %>% filter(!is.na(LBXCOT))

# perform ROC analysis
rocobj <- pROC::roc(df$ONLY_VAPES, df$LBXCOT, direction = "<", ci=TRUE)

# extract ROC data
df.roc <- data.frame(invspec = (1 - rocobj$specificities), sens = rocobj$sensitivities)

# extract AUC and CI
lb <- rocobj$ci[1] 
auc <- rocobj$ci[2] 
ub <- rocobj$ci[3]

# create annotation text for AUC
lab.text <- paste0("AUC: ", signif(auc, 2), " (", signif(lb, 2), " - ", signif(ub, 2), ")")
ann.text <- data.frame(invspec = 0.25, sens = 0.05, label = lab.text, n = length(rocobj$cases))

# find the optimal threshold with specificity > 0.95
optimal_coords <- coords(rocobj, x = "all", ret = c("threshold", "sensitivity", "specificity"))
optimal_coords <- optimal_coords[optimal_coords$specificity > 0.95, ]

# select the threshold with the highest sensitivity from those with specificity > 0.95
optimal_threshold <- optimal_coords[which.max(optimal_coords$sensitivity), ]

# bootstrapping for confidence intervals
# function to compute optimal threshold for bootstrapping
compute_optimal_threshold <- function(data, indices) {
  boot_data <- data[indices, ]
  rocobj <- pROC::roc(boot_data$ONLY_VAPES, boot_data$LBXCOT, direction = "<")
  optimal_coords <- coords(rocobj, x = "all", ret = c("threshold", "sensitivity", "specificity"))
  optimal_coords <- optimal_coords[optimal_coords$specificity > 0.95, ]
  optimal_threshold <- optimal_coords[which.max(optimal_coords$sensitivity), "threshold"]
  return(optimal_threshold)
}

# perform bootstrapping
set.seed(72124)  # for reproducibility
boot_results <- boot(df, statistic = compute_optimal_threshold, R = 2000, strata = df$ONLY_VAPES)

# get the bootstrapped confidence intervals
boot_ci <- boot.ci(boot_results, type = "perc")

# plot ROC curve
overall.roc <- ggplot(df.roc, aes(invspec, sens)) +
  geom_line(size = 1, color = "#df8f44", alpha = 0.7) +
  scale_x_continuous(expand = c(0, 0.01),
                     breaks = c(0, 0.25, 0.5, 0.75, 1),
                     labels = as.character(c("0", ".25", ".50", ".75", "1.0"))) +
  scale_y_continuous(expand = c(0, 0.01),
                     labels = as.character(c("0", ".25", ".50", ".75", "1.0"))) +
  theme_minimal(base_size = 17) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "none"
  ) +
  labs(
    title = "ROC Curve for Cotinine Levels Discriminating\nE-cigarette Users vs. Non-Tobacco Users",
    x = "1 - Specificity",
    y = "Sensitivity"
  ) +
  geom_text(data = ann.text, aes(label = label), size = 5)

print(optimal_threshold) #    threshold sensitivity specificity
                         #        1.015   0.7988827   0.9503029
print(boot_ci)           #    level   percentile     
                         #      95%   (0.800, 1.245) 

# concordance analysis
df <- df %>% mutate(
  COTININE_CLASSIFICATION = ifelse(LBXCOT >= optimal_threshold$threshold, 1, 0)
)

# calculate concordance
# concordance is when self-reported e-cigarette use matches cotinine classification
df <- df %>% mutate(
  CONCORDANCE = case_when(
    ONLY_VAPES == 1 & COTININE_CLASSIFICATION == 1 ~ 1,
    NON_SMOKER == 1 & COTININE_CLASSIFICATION == 0 ~ 1,
    TRUE ~ 0
  )
)

# generate concordance summaries
calculate_concordance <- function(data, group_var) {
  data %>%
    group_by(!!sym(group_var)) %>%
    summarise(
      n = n(),
      concordant = sum(CONCORDANCE),
      percent_concordant = (concordant / n) * 100
    ) %>%
    mutate(group = group_var)
}

# non-tobacco users
non_smoker_concordance <- bind_rows(
  calculate_concordance(df %>% filter(NON_SMOKER == 1), "AGECAT"),
  calculate_concordance(df %>% filter(NON_SMOKER == 1), "RIAGENDR"),
  calculate_concordance(df %>% filter(NON_SMOKER == 1), "RIDRETH1"),
  calculate_concordance(df %>% filter(NON_SMOKER == 1), "INC_BINARY")
)

# exclusive e-cigarette users
vapers_concordance <- bind_rows(
  calculate_concordance(df %>% filter(ONLY_VAPES == 1), "AGECAT"),
  calculate_concordance(df %>% filter(ONLY_VAPES == 1), "RIAGENDR"),
  calculate_concordance(df %>% filter(ONLY_VAPES == 1), "RIDRETH1"),
  calculate_concordance(df %>% filter(ONLY_VAPES == 1), "INC_BINARY")
)

concordance_results <- bind_rows(
  non_smoker_concordance %>% mutate(group_type = "Non-Tobacco Users"),
  vapers_concordance %>% mutate(group_type = "Exclusive E-Cigarette Users")
)


concordance_pivot <- concordance_results %>%
  pivot_wider(
    names_from = group_type,
    values_from = c(n, concordant, percent_concordant),
    names_glue = "{group_type}_{.value}"
  )


# stitch together final figure
figure <- ggarrange(
  survey_cotinine,
  overall.roc,
  labels = c("A", "B"),
  ncol = 2, nrow = 1,
  common.legend = FALSE
)

ggsave(
  filename = "nhanes_e_cig_figure.pdf",
  plot = figure,
  device = "pdf",
  width = 14,
  height = 6,
  units = "in",
  dpi = 300
)
  
