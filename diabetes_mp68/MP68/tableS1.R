#############################################################
## compute AUC to predict T1D with each variables
## ferratlauric@gmail.com - January 2019
#############################################################

##############################
# 0 - Load librairies
###############library(caret)


############################## 
# 1 - Source file 
##############################

# source(paste0(codePath,"formula_f.R"))


#load data
source(paste0(codePath,"main_load_data.R"))
source(paste0(codePath,'ggplotAUC.R'))
source(paste0(codePath,'Confidence_Interval_for_AUC.R'))
#############################
# load data
#############################
day <- 2*365.25 #landmark
dayend <- 8*365.25 # horizon time
source(paste0(codePath,"Extract_information_per_date.R"))

############################
# choose predictor, formula and so on.
############################
variables_of_interest <- c("t1d") #,"persist_conf_ab_all_time","multiple_autoantibody_all_time")
if (day < 1) {
  predictor_of_interest <- c("Sex","country_cd","c_section","fdr","siblingsDiabetesType","FatherDiabetesType","MotherDiabetesType","grs1","GRS2","fathers_age","maternal_age","resp_gest_inf", "weight")
  predictor_of_interest_true_name <- c("Sex","country","caesarean section","race and ethnicity","first degree relative","siblings statue","Father statue","Mother statue","grs1","GRS2","father age","mother age","resp. gest. inf.", "weight")
} else{
  predictor_of_interest <- c("Sex","country_cd","c_section","number_autoantibody","persist_conf_gad ","persist_conf_ia2a ","persist_conf_miaa ","probio","fdr","siblingsDiabetesType","FatherDiabetesType","MotherDiabetesType","grs1","GRS2","fathers_age","maternal_age","resp_gest_inf","fevergrp_tot_day","common_cold_tot_day","laryngitis_trac_tot_day","influenza_tot_day","acute_sinusitis","resp_tot_day","gastro_tot_day","weight")
  predictor_of_interest_true_name <- c("Sex","Country","Caesarean section","Race and ethnicity","Autoantibody number","GAD ","IA2A","MIAA","Probiotic","First degree relative","Siblings statue","Father statue","Mother statue","GRS1","GRS2","Father age","Mother age","Gestational respiratory infections","Fever days","Common cold days","Laryngitis trac. days","Influenza days","Acute sinusitis","Respiratory days","gastro. days","weight")
  
}
n_v <- length(variables_of_interest)
n_p <- length(predictor_of_interest)
data <- finaldata
###########################
# compute the different AUC
###########################
AUC_m <- matrix(0L, nrow = n_p, ncol = n_v)
seAUC_m <- matrix(0L, nrow = n_p, ncol = n_v)
p_m <- matrix(0L, nrow = n_p, ncol = n_v)
CI_min_m <- matrix(0L, nrow = n_p, ncol = n_v)
CI_max_m <- matrix(0L, nrow = n_p, ncol = n_v)
for (j in 1:n_v) {
  datasetj <- data %>% drop_na(variables_of_interest[j])
  for (i in 1:n_p) {
    
    datasetj[[variables_of_interest]] <- factor(datasetj[[variables_of_interest]],labels = c("Yes","No"))
    # organise data base to be used with conventional time dependant approach
    
    # formula for the model
    
    formula.model <- as.formula(paste0("Surv(last_clinic_visit_agedys, t1d)","~",paste(predictor_of_interest[i], collapse = " + ")))
    
    datasetj[[variables_of_interest]] <- as.numeric(datasetj[[variables_of_interest]]) - 1
    
    
    res.cox <- coxph(formula.model, data = datasetj)
    
    # extract linear predictio in the log hazard function
    marker_score <- predict(res.cox, datasetj, type = "risk")
    
    
    ################### compute AUC score with score achieve with Cox model
    delta <- datasetj[[variables_of_interest]]
    ROC.T <- timeROC(T = datasetj$last_clinic_visit_agedys,
                     delta = delta,marker = marker_score,
                     cause = 1,
                     times = c(1000,dayend),
                     iid = TRUE)
    
    AUC_m[i,j] <- ROC.T$AUC[2]
    seAUC_m[i,j] <- ROC.T$inference$vect_sd_1[2]
    p_m[i,j] <- summary(res.cox)$logtest[3] #mean(summary(res.cox)$coef[,5])
    
  }
}
variables_of_interest_nice_name <- c("AUC (CI) pvalue")
rownames(AUC_m) <- predictor_of_interest_true_name
colnames(AUC_m) <- variables_of_interest_nice_name 
rownames(p_m) <- predictor_of_interest_true_name
colnames(p_m) <- variables_of_interest_nice_name 

save(AUC_m,seAUC_m,p_m,file = paste0(pathSaveModels,"filter_results_",day,"_",dayend,"R.data"))


load(paste0(pathSaveModels,"filter_results_",day,"_",dayend,"R.data"))

for (i in 1:dim(AUC_m)[1]) {
  AUC_m[i,1] <- max(AUC_m[i,1],1 - AUC_m[i,1])
}

CI_max_m <- create_IC_se(AUC_m,seAUC_m,confidencev = 1.96, class = "up")
CI_min_m <- create_IC_se(AUC_m,seAUC_m,confidencev = 1.96, class = "low")
rownames(CI_min_m) <- predictor_of_interest_true_name
colnames(CI_min_m) <- variables_of_interest_nice_name 
rownames(CI_max_m) <- predictor_of_interest_true_name
colnames(CI_max_m) <- variables_of_interest_nice_name 
print(AUC_m)
levels_order <- predictor_of_interest_true_name

ggplotAUC2_pvalue(AUC_m,CI_min_m,CI_max_m,p_m,levels_order)

ggsave(paste0("AUC_individual_variable_",day,".jpg",file = ""))
# 

table_summary <- as.data.frame(cbind(row.names(AUC_m),round(as.numeric(AUC_m),2),round(as.numeric(CI_min_m),2),round(as.numeric(CI_max_m),2),signif(as.numeric(p_m),2)))
colnames(table_summary) <- c("variable_name","AUC", "lower", "upper","pvalue")
table_summary  <- table_summary  %>% arrange(AUC)
table_summary <- table_summary %>% mutate(AUCmin = paste(" (",lower,"-", sep = '')) %>%
  mutate(AUCmax = paste(upper,")", sep = '')) %>%
  mutate(text = paste(AUC, AUCmin, AUCmax,pvalue, sep = '')) %>% 
  select(variable_name,text,pvalue)

################## autoantibody ################
myft <- regulartable(
  table_summary)
myft
myft <- theme_vanilla(myft)
myft


doc <- read_docx()
doc <- body_add_flextable(doc, value = myft)
print(doc, target = paste0(pathfigures_tables,"table1.docx"))

