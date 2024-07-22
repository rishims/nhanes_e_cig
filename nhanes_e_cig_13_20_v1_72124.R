# Discordance between self-reported e-cigarette use and measured cotinine levels in non-smoking e-cigarette users
# Rishi Shah
# July 21, 2024
# Study period is 2013-2020 and data source is the National Health and Nutrition Examination Survey

# load necessary libraries
library(nhanesA)
library(tidyverse)
library(survey)
library(ggplot2)
library(ggsci)
library(ggpubr)

# import data -----
# import demographic data
demo_data_13_14 <- nhanes("DEMO_H") # demographic data for 2013-2014
demo_data_15_16 <- nhanes("DEMO_I") # demographic data for 2015-2016
demo_data_17_20 <- nhanes("P_DEMO") # demographic data for 2017-2020

# import recent tobacco use data
smoke_data_13_14 <- nhanes("SMQRTU_H") # recent tobacco use 2013-2014
smoke_data_15_16 <- nhanes("SMQRTU_I") # recent tobacco use 2015-2016
smoke_data_17_20 <- nhanes("P_SMQRTU") # recent tobacco use 2017-2020

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
demo_data_17_20 <- demo_data_17_20 %>% mutate(WTADJ = WTINTPRP * (3.2 / 7.2), WTADJMEC = WTMECPRP * (2 / 7.2))

# match cotinine data variables across survey cycles
cot_data_17_20 <- cot_data_17_20 %>% rename(LBXHCT = LBXHCOT, LBDHCTLC = LBDHCOLC)

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
  "SMQ849"   # Number of days smoked e-cigarette in the last 5 days
)

smoke_data_13_14 <- smoke_data_13_14 %>% select(all_of(vars))
smoke_data_15_16 <- smoke_data_15_16 %>% select(all_of(vars))
smoke_data_17_20 <- smoke_data_17_20 %>% select(all_of(vars))

# bind demographics data
demographics <- bind_rows(demo_data_13_14, demo_data_15_16, demo_data_17_20)

# bind serum cotinine data
cotinine <- bind_rows(cot_data_13_14, cot_data_15_16, cot_data_17_20)

# bind recent tobacco use data
e_cigarette <- bind_rows(smoke_data_13_14, smoke_data_15_16, smoke_data_17_20)

# join all data
data <- left_join(demographics, cotinine, by = "SEQN")
data <- left_join(data, e_cigarette, by = "SEQN")

# analysis ----
# exclude all smokers (other than e-cigarette users)
data <- data %>% mutate(VAPES = case_when(SMQ690H == 8 & is.na(SMQ690A) & is.na(SMQ690B) & is.na(SMQ690C) & is.na(SMQ690G) ~ 1,
                                              TRUE ~ 0)) # SMQ690A != 1 & SMQ690B != 2 & SMQ690C != 3 & SMQ690G != 7, also does smokeless tobacco matter?

data <- data %>% filter(VAPES == 1 & LBDCOTLC == "At or above the detection limit") # lower limit of detection for cotinine is 0.015 ng/mL

# define the survey design
svy_design <- svydesign(
  id = ~SDMVPSU,
  strata = ~SDMVSTRA,
  weights = ~WTADJMEC,
  data = data,
  nest = TRUE
)

data_svy <- svyby(~LBXCOT, ~SMQ849, svy_design, svyquantile, quantiles = c(0.05, 0.25, 0.5, 0.75, 0.95), ci = TRUE)
data_svy <- as.data.frame(data_svy)

# survey-adjusted distribution of cotinine levels by # of days last used e-cigarette in the last 5 days
survey_cotinine <- ggplot(data_svy, aes(x = as.factor(SMQ849), y = LBXCOT.0.5)) +
  geom_boxplot(aes(
    lower = LBXCOT.0.25, middle = LBXCOT.0.5, upper = LBXCOT.0.75,
    ymin = LBXCOT.0.05, ymax = LBXCOT.0.95, fill = as.factor(SMQ849)
  ), stat = "identity") +
  scale_fill_jama() +
  labs(
    title = "National Estimates of Serum Cotinine Levels by\nDays of E-cigarette Use in Last 5 Days",
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
data$INC_BINARY[data$INDFMPIR <= 2.00] <- 0
data$INC_BINARY[data$INDFMPIR > 2.00] <- 1

levels <- c("Low income", "Middle/High income")
labels <- c(0, 1)
data$INC_BINARY <- factor(data$INC_BINARY, levels = labels, labels = levels)

# create sample summary table
t1_all <- (Table1_all_format <- tbl_summary(
  data = data, 
  statistic = list(
    AGECAT ~ "{n} ({p})",
    RIAGENDR ~ "{n} ({p})",
    RIDRETH1 ~ "{n} ({p})",
    INC_BINARY ~ "{n} ({p})"),
  missing_text = "Missing",
  include = c(AGECAT, RIAGENDR, RIDRETH1, INC_BINARY)
)) #%>% add_p(pvalue_fun = function(x) style_pvalue(x, digits = 3))

