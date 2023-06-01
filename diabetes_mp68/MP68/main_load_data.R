# main load

#############################################################
## ## ferratlauric@gmail.com - September 2018
#############################################################

############################## 
# 1 - Source file 
##############################

source(paste0(codePath,"load_data/demographics_data.R"))
source(paste0(codePath,"load_data/ab.R"))
source(paste0(codePath,"load_data/date_last_ab_test.R"))
source(paste0(codePath,"load_data/family.R"))
#source(paste0(codePath,"load_data/probiotic.R"))
source(paste0(codePath,"load_data/HLA.R"))
source(paste0(codePath,"load_data/growth.R"))
#source(paste0(codePath,"load_data/respiratory.R"))
source(paste0(codePath,"load_data/rie_gie_inf.R"))



#############################
# make basic transformation on data
#############################



sizetext <- 14
face <- "plain"

demographics <- data.frame(demographics)
#remove people without information on antibodies
demographics <- demographics %>% filter(indeterminate == 0)
proportion <- sum(demographics[,"t1d"] == "1")/dim(demographics)[1]
demographics$t1d <- factor(demographics$t1d)
demographics$status <- factor(demographics$status)
demographics$Sex <- factor(demographics$sex)
demographics$country_cd <- factor(demographics$country_cd)
demographics$c_section <- factor(demographics$c_section)
demographics$race_ethnicity <- factor(demographics$race_ethnicity)
demographics$fdr <- factor(demographics$fdr)
demographics$indeterminate <- factor(demographics$indeterminate)
demographics$maternal <- factor(demographics$maternal)
demographics$positive <- factor(demographics$positive)
demographics$persist_conf_ab <- factor(demographics$persist_conf_ab)
demographics$HLA_Category <- factor(demographics$hla_category)
demographics$cc <- factor(demographics$cc)
demographics <- demographics %>%
  mutate(HLA_Category_all = HLA_Category,
         HLA_Category = recode_factor(HLA_Category,'0' = 'other','3' = 'other','5' = 'other','6' = 'other','7' = 'other','8' = 'other','10' = 'other',.default = levels(HLA_Category)))

##############
# add grs1 score
##############
#source(paste0(codePath,'load_data/GRS1_score.R'))

##############
# add grsz score
##############
dataPathGRS1 <- dataPath
dataFile<- "MP68_all_GRS_masked.xlsx"
nonHLAScore <-  read_excel(paste0(dataPath,dataFile))
##############################
# 2 - Compute grs1 score
##############################

demographics <- demographics %>%
  mutate(HLAscore_grs_Z = case_when(
    HLA_Category_all == "0" ~ -0.330683508445073, #X/X
    HLA_Category_all == "1" ~ 3.87478847833289, # DR4-DR3
    HLA_Category_all == "2" ~ 3.09129603963818, # DR4-DR4
    HLA_Category_all == "3" ~ 1.94989493270617, #DR4-X
    HLA_Category_all == "4" ~ 1.94989493270617, #DR4-X
    HLA_Category_all == "5" ~ 1.94989493270617, #DR4-X
    HLA_Category_all == "6" ~ 1.94989493270617, #DR4-X
    HLA_Category_all == "7" ~ 1.94989493270617, #DR4-X
    HLA_Category_all == "8" ~ 1.94989493270617, #DR4-X
    HLA_Category_all == "9" ~ 3.05252703167994, #DR3-DR3
    HLA_Category_all == "10" ~ 1.50820106390584 #DR3-X
  )) %>%
  left_join(nonHLAScore, by = c("MP68_MaskID" = "mp68_maskid")) %>%
  mutate(GRSZ = TEDDY_GRS_without_DR_DQ + HLAscore_grs_Z)

######
# load basic code
#####

create_IC <- function(table,confidencev=1.96){
  n <- dim(table)[1]
  CI_l <- rep(0,n)
  CI_u <- rep(0,n)
  for (i in 1:n) {
    if (i %% 2 == 1) {
      total <- table[i,"freq"] + table[i + 1,"freq"]
      p <- table[i,"freq"]/total
      p <- 1 - p
      CI_l[i] <- max(p - confidencev*sqrt(p*(1 - p)/total),0)
      CI_u[i] <- min(p + confidencev*sqrt(p*(1 - p)/total),1)
    }
    else{
      CI_l[i] <- NA
      CI_u[i] <- NA
    }
  }
  IC <- data.frame(CI_l,CI_u)
  return(IC)
}

