
##### main t-AUC
#############################################################
## ## ferratlauric@gmail.com - Novembre 2018
#############################################################
# library to load
library(timeROC)
library(survival)
library(caret)
set.seed(123)

source(paste0(codePath,"/t_AUC - GRS2/list_variable_f.R"))


method1 <- "Cox"
complexity1 <- "abn_grs_fdr"
complexity2 <- "abn"


day_beginv <- c(60,365.25,365.25 + 183,365.25*2 + seq(0,8*365.25,365)) + 45



n_time_begin <- length(day_beginv)
AICm <- matrix(data = NA,nrow = length(day_beginv),ncol = 2)
for (i in 1:n_time_begin) {
  day = day_beginv[i]
  dayend = Inf
  source(paste0(codePath,"Extract_information_per_date.R"))
  dataset_ml <- finaldata
  dataset_ml[,"t1d"] <- factor(dataset_ml[,"t1d"],labels = c("Yes","No"))
  dataset_ml[,"t1d"] <- as.numeric(dataset_ml[,"t1d"]) - 1
  # formula for the models
  variables_for_prediction1 <- list_variable_f(day,complexity1)
  formula.model1 <- as.formula(paste0("Surv(last_clinic_visit_agedys, t1d)","~",paste(variables_for_prediction1, collapse = " + ")))
  
  variables_for_prediction2 <- list_variable_f(day,complexity2)
  formula.model2 <- as.formula(paste0("Surv(last_clinic_visit_agedys, t1d)","~",paste(variables_for_prediction2, collapse = " + ")))
  
  res.cox1<- coxph(formula.model1, data = dataset_ml)
  AIC1_i <- extractAIC(res.cox1, scale, k = log(dim(dataset_ml)[1]))
  res.cox2<- coxph(formula.model2, data = dataset_ml)
  AIC1_2_i <- extractAIC(res.cox2, scale, k = log(dim(dataset_ml)[1]))
  AICm[i,] <- c(AIC1_i[2],AIC1_2_i[2])
}
