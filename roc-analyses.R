#ROC Analyses
#Adith Arun, building off of Rishi's original code

# load necessary libraries
library(nhanesA)
library(tidyverse)
library(pROC)

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

# mathc cotinine data variables across survey cycles
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
  "SMQ863"   # Used nicotine replacement last 5 days 
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
# exclude all smokers (other than e-cigarette users) and all smokeless tobacco users and nicotine replacement product users
# 1 = exclusive e-cigarette user in the last 5 days
data <- data %>% mutate(ONLY_VAPES = case_when(LBDCOTLC == "At or above the detection limit" & (SMQ863 == "No" | is.na(SMQ863)) & SMQ851 == "No" & SMQ681 == "Yes" & SMQ690H == 8 & is.na(SMQ690A) & is.na(SMQ690B) & is.na(SMQ690C) & is.na(SMQ690G) ~ 1,
                                              TRUE ~ 0)) # lower limit of detection for cotinine is 0.015 ng/mL

# create variable for SHS exposure inside the respondent's home: 1 = respondent lives with at least 1 person who smokes inside their home
data <- data %>% mutate(SHS_EXPOSURE = case_when(SMD470 %in% c("1 household member smokes inside the house", 
                                                               "2 household members smoke inside the house",
                                                               "2 or more household members smoke inside the house",
                                                               "3 or more household members smoke inside the house") ~ 1,
                                                 TRUE ~ 0))

# create variable for tobacco use status in the last 5 days: 1 = non smoker/tobacco user
data <- data %>% mutate(NON_SMOKER = case_when(SMQ681 == "No" & SMQ851 == "No" & SMQ863 == "No" ~ 1,
                                               TRUE ~ 0))


# for roc and regression analysis, subset data to only include exclusive e-cigarette users and non-tobacco users of any kind
data_filtered <- data %>% filter(
  ONLY_VAPES == 1 | NON_SMOKER == 1
)

data_filtered <- as_tibble(data_filtered)

df <- data_filtered %>% select(SHS_EXPOSURE, NON_SMOKER, ONLY_VAPES, LBXCOT, SMQ849)


#NON-smokers versus people who vape the majority of days 
#df <- df %>% filter(SHS_EXPOSURE == 0) %>% filter(!is.na(LBXCOT)) %>% filter((is.na(SMQ849) | SMQ849 >= 3))

df <- df %>% filter(SHS_EXPOSURE == 0) %>% filter(!is.na(LBXCOT))
#
rocobj <- pROC::roc(df$ONLY_VAPES, df$LBXCOT, direction = "<", ci=TRUE)

df.roc <- data.frame(invspec=(1-rocobj$specificities), sens=rocobj$sensitivities)

lb <- rocobj$ci[1] 
auc <- rocobj$ci[2] 
ub <- rocobj$ci[3]

lab.text <- paste0("AUC: ", auc %>% signif(2), " (", lb %>% signif(2), " - ", ub %>% signif(2) , ")")

ann.text <- data.frame(invspec=0.25, sens=0.05, label=lab.text, n=length(rocobj$cases))

overall.roc <- ggplot(df.roc, aes(invspec, sens)) +
      geom_line(size=1) +
        scale_x_continuous(expand=c(0, 0.01),
                        breaks=c(0, 0.25, 0.5, 0.75, 1),
                        labels=as.character(
                            c("0", ".25", ".50", ".75", "1.0"))) +
        scale_y_continuous(expand=c(0, 0.01),
                           labels=as.character(
                               c("0", ".25", ".50", ".75", "1.0")))  +
        theme_classic(base_size=17) +
        theme(panel.background=element_blank(),
              panel.grid=element_blank(),
              legend.position="top",          axis.line.x.bottom=element_line(color="black"),
              strip.background=element_blank(),
              aspect.ratio=0.95) +
        xlab("1 - Specificity") + ylab("Sensitivity") + geom_text(data = ann.text,label = lab.text, size=5)

  
overall.roc

coords(rocobj, x="best", input="threshold", best.method="youden")


coords(rocobj, 5)
















#
