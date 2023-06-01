# main censor information

#############################################################
## ## ferratlauric@gmail.com - September 2018
#############################################################


############################## 
# 1 - Source files
##############################
#load data


##############################
# 2 - remove information afters day: day

source(paste0(codePath,"load_data/select_before_days/day_demographics.R"))
source(paste0(codePath,"load_data/select_before_days/day_ab.R"))
source(paste0(codePath,"load_data/select_before_days/day_family.R"))
#source(paste0(codePath,"load_data/select_before_days/day_probiotics.R"))
source(paste0(codePath,"load_data/select_before_days/day_growth.R"))
#source(paste0(codePath,"load_data/select_before_days/day_respiratory.R"))
source(paste0(codePath,"load_data/select_before_days/day_rie_gie_inf.R"))
source(paste0(codePath,"load_data/GRS2.R"))


finaldata <- demographics_day %>% 
  dplyr::left_join(date_last_test, by = "MP68_MaskID") %>%
  dplyr::left_join(ab_day, by = "MP68_MaskID") %>%
  dplyr::left_join(family_day, by = "MP68_MaskID") %>%
  dplyr::left_join(GRS2, by = "MP68_MaskID") %>%
  dplyr::left_join(growth_day, by = "MP68_MaskID") %>%
  #  dplyr::left_join(probiotics_day, by = "MP68_MaskID") %>%
  #  dplyr::left_join(respiratory_day, by = "MP68_MaskID") %>%
  dplyr::left_join(rie_gie_inf_day, by = "MP68_MaskID") %>%
  mutate(siblingsDiabetesType = ifelse(is.na(siblingsDiabetesType),"no",siblingsDiabetesType),
         siblingsDiabetesType = as.factor(siblingsDiabetesType),
         MotherDiabetesType = if_else(MotherDiabetesType == "Type 1", 1 , 0),
         MotherDiabetesType = if_else(is.na(MotherDiabetesType), 0, MotherDiabetesType),
         MotherDiabetesType = as.factor(MotherDiabetesType),
         FatherDiabetesType = if_else(FatherDiabetesType == "Type 1", 1 , 0),
         FatherDiabetesType = if_else(is.na(FatherDiabetesType), 0, FatherDiabetesType),
         FatherDiabetesType = as.factor(FatherDiabetesType)) %>%
  filter(day <= (last_test*30.5 + 6*30.5)) %>% # remove if last test was more than 6 months ago
  mutate(t1d = if_else(t1d_diag_agedys > day + dayend, 0, 1, missing = 0)) %>%
  mutate(t1d = factor(t1d)) %>% 
  filter(!is.na(GRS2)) %>% 
  mutate(t1d_diag_agedys = if_else(!is.na(t1d_diag_agedys),t1d_diag_agedys,100000)) # to avoid to be imputed few line later

