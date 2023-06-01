tAUC_ml <- function(complexity,dataset_ml,dataset_timedep,day_begin,day_end,weighting = "marginal", variable_of_interest = "t1d",path = pathSaveModels, control = trainControl(method = "repeatedcv", number = 5, repeats = 2,classProbs = TRUE,summaryFunction = twoClassSummary,savePredictions = TRUE)){
  
  # organise data base to be used with conventional ml and logistic approach
  
  dataset_ml[[variable_of_interest]] <- factor(dataset_ml[[variable_of_interest]],labels = c("Yes","No"))
  # organise data base to be used with conventional time dependant approach
  
  dataset_timedep[[variable_of_interest]] <- factor(dataset_timedep[[variable_of_interest]],labels = c("Yes","No"))
  
  
  # formula for the models
  variables_for_prediction <- list_variable_f(day_begin,complexity)
  
  formula.model <- as.formula(paste0("Surv(last_clinic_visit_agedys, t1d)","~",paste(variables_for_prediction, collapse = " + ")))
  
  dataset_ml[[variable_of_interest]] <- as.numeric(dataset_ml[[variable_of_interest]]) - 1
  #computeCox model
  res.cox <- coxph(formula.model, data = dataset_ml)
  
  # extract linear predictio in the log hazard function
  marker_score <- predict(res.cox, dataset_ml, type = "risk")
  
  
  ################### compute AUC score with score achieve with Cox model
  delta <- if_else(dataset_timedep[[variable_of_interest]] == "No",1,0)
  ROC.T <- timeROC(T = dataset_timedep$last_clinic_visit_agedys,
                   delta = delta,marker = marker_score,
                   cause = 1, weighting = weighting,
                   times = day_end,
                   iid = TRUE)
  names_tAUC <- paste0("t_AUC_","_complexity_",complexity1,"_day_begin_",day_begin)
  
  
  # save results
  save(ROC.T, file = paste0(path,names_tAUC,".RData"))
  return(ROC.T)
}
source(paste0(codePath,"t_AUC - GRS2/list_variable_f.R"), echo=TRUE)
##### main t-AUC
#############################################################
## ## ferratlauric@gmail.com - Novembre 2018
#############################################################
# library to load

set.seed(123)



method1 <- "Cox"
complexity1 <- "grs"
# complexity1 <- "abn_grs_fdr"
day_beginv <- c(60,365.25,365.25 + 183,365.25*2 + seq(0,8*365.25,365))
# day_beginv <- c(365.25*5 + seq(0,3*365.25,365))

day_endv <- c(365.25*3,8*365.25)


n_time_begin <- length(day_beginv)
n_time_end <- length(day_endv)


day = 365.25*0
dayend = Inf
source(paste0(codePath,"Extract_information_per_date.R"))
dataset_ml <- finaldata %>% filter(!is.na(TEDDY_GRS_without_DR_DQ))
TimeRoc1 <- tAUC_ml(complexity1,dataset_ml,dataset_ml,day,day + day_endv, variable_of_interest = "t1d")


TP <- TimeRoc1$TP
colnames(TP) <- c("years3","years8")
FP <- TimeRoc1$FP
colnames(FP) <- c("years3","years8")


TP <- TP %>% data.frame() %>% gather("year","TP",1:length(day_endv)) %>%  mutate(year = factor(year, levels = c("years3","years8")))
FP <- FP %>% data.frame() %>% gather("year","FP",1:length(day_endv)) %>%  select("FP")  

data1 <- cbind(TP,FP)

ggplot(data1, aes(x = FP,y = TP, colour = year)) +
  geom_line(size = 2) +
  coord_equal() +
  style_roc() +
  scale_colour_discrete(labels = c("3 years","8 years"))

#####################################
method2 <- "Cox"
complexity2 <- "grs1"
# complexity1 <- "abn_grs_fdr"
day_beginv <- c(60,365.25,365.25 + 183,365.25*2 + seq(0,8*365.25,365))
# day_beginv <- c(365.25*5 + seq(0,3*365.25,365))

day_endv <- c(365.25*3,8*365.25)


n_time_begin <- length(day_beginv)
n_time_end <- length(day_endv)


day = 365.25*0
dayend = Inf
source(paste0(codePath,"Extract_information_per_date.R"))
dataset_ml <- finaldata %>% filter(!is.na(TEDDY_GRS_without_DR_DQ))
TimeRoc2 <- tAUC_ml(complexity2,dataset_ml,dataset_ml,day,day + day_endv, variable_of_interest = "t1d")  

TP <- TimeRoc2$TP
colnames(TP) <- c("years3","years8")
FP <- TimeRoc2$FP
colnames(FP) <- c("years3","years8")


TP <- TP %>% data.frame() %>% gather("year","TP",1:length(day_endv)) %>%  mutate(year = factor(year, levels = c("years3","years8")))
FP <- FP %>% data.frame() %>% gather("year","FP",1:length(day_endv)) %>%  select("FP")  

data2 <- cbind(TP,FP)

ggplot(data2, aes(x = FP,y = TP, colour = year)) +
  geom_line(size = 2) +
  coord_equal() +
  style_roc() +
  scale_colour_discrete(labels = c("3 years","8 years"))



#############################

method1 <- "Cox"
complexity3 <- "HLAscore_grs_Z"
# complexity1 <- "abn_grs_fdr"
day_beginv <- c(60,365.25,365.25 + 183,365.25*2 + seq(0,8*365.25,365))
# day_beginv <- c(365.25*5 + seq(0,3*365.25,365))

day_endv <- c(365.25*3,8*365.25)


n_time_begin <- length(day_beginv)
n_time_end <- length(day_endv)


day = 365.25*0
dayend = Inf
source(paste0(codePath,"Extract_information_per_date.R"))
dataset_ml <- finaldata %>% filter(!is.na(TEDDY_GRS_without_DR_DQ))
TimeRoc3 <- tAUC_ml(complexity3,dataset_ml,dataset_ml,day,day + day_endv, variable_of_interest = "t1d")


TP <- TimeRoc3$TP
colnames(TP) <- c("years3","years8")
FP <- TimeRoc3$FP
colnames(FP) <- c("years3","years8")


TP <- TP %>% data.frame() %>% gather("year","TP",1:length(day_endv)) %>%  mutate(year = factor(year, levels = c("years3","years8")))
FP <- FP %>% data.frame() %>% gather("year","FP",1:length(day_endv)) %>%  select("FP")  

data3 <- cbind(TP,FP)

ggplot(data3, aes(x = FP,y = TP, colour = year)) +
  geom_line(size = 2) +
  coord_equal() +
  style_roc() +
  scale_colour_discrete(labels = c("3 years","8 years"))

#############################

method4 <- "Cox"
complexity4 <- "TEDDY_GRS_without_DR_DQ"
# complexity1 <- "abn_grs_fdr"
day_beginv <- c(60,365.25,365.25 + 183,365.25*2 + seq(0,8*365.25,365))
# day_beginv <- c(365.25*5 + seq(0,3*365.25,365))

day_endv <- c(365.25*3,8*365.25)


n_time_begin <- length(day_beginv)
n_time_end <- length(day_endv)


day = 365.25*0
dayend = Inf
source(paste0(codePath,"Extract_information_per_date.R"))
dataset_ml <- finaldata %>% filter(!is.na(TEDDY_GRS_without_DR_DQ))
TimeRoc4 <- tAUC_ml(complexity4,dataset_ml,dataset_ml,day,day + day_endv, variable_of_interest = "t1d")


TP <- TimeRoc4$TP
colnames(TP) <- c("years3","years8")
FP <- TimeRoc4$FP
colnames(FP) <- c("years3","years8")


TP <- TP %>% data.frame() %>% gather("year","TP",1:length(day_endv)) %>%  mutate(year = factor(year, levels = c("years3","years8")))
FP <- FP %>% data.frame() %>% gather("year","FP",1:length(day_endv)) %>%  select("FP")  

data4 <- cbind(TP,FP)

ggplot(data4, aes(x = FP,y = TP, colour = year)) +
  geom_line(size = 2) +
  coord_equal() +
  style_roc() +
  scale_colour_discrete(labels = c("3 years","8 years"))
###########################

#############################

method5 <- "Cox"
complexity5 <- "GRSZ"
# complexity1 <- "abn_grs_fdr"
day_beginv <- c(60,365.25,365.25 + 183,365.25*2 + seq(0,8*365.25,365))
# day_beginv <- c(365.25*5 + seq(0,3*365.25,365))

day_endv <- c(365.25*3,8*365.25)


n_time_begin <- length(day_beginv)
n_time_end <- length(day_endv)


day = 365.25*0
dayend = Inf
source(paste0(codePath,"Extract_information_per_date.R"))
dataset_ml <- finaldata %>% filter(!is.na(TEDDY_GRS_without_DR_DQ))
TimeRoc5 <- tAUC_ml(complexity5,dataset_ml,dataset_ml,day,day + day_endv, variable_of_interest = "t1d")


TP <- TimeRoc5$TP
colnames(TP) <- c("years3","years8")
FP <- TimeRoc5$FP
colnames(FP) <- c("years3","years8")


TP <- TP %>% data.frame() %>% gather("year","TP",1:length(day_endv)) %>%  mutate(year = factor(year, levels = c("years3","years8")))
FP <- FP %>% data.frame() %>% gather("year","FP",1:length(day_endv)) %>%  select("FP")  

data5 <- cbind(TP,FP)

ggplot(data5, aes(x = FP,y = TP, colour = year)) +
  geom_line(size = 2) +
  coord_equal() +
  style_roc() +
  scale_colour_discrete(labels = c("3 years","8 years"))

#############################################

data1$model <- "1"
data2$model <- "2"
data3$model <- "3"
data4$model <- "4"
data5$model <- "5"
data_all <- rbind(data1,data2,data5) 
data_all <- data_all %>% filter(year == "years8")
p <- ggplot(data_all, aes(x = FP,y = TP,colour = model )) +
  geom_line(size = 1) +
  coord_equal() +
  style_roc(ylab = "sensitivity", xlab = "1 - specificity") +
  scale_colour_discrete(name = "Model",labels = c("GRS 2 ","GRS 1","GRS combined TEDDY")) +
  # scale_colour_brewer(palette = "Set1",name = "Horizon time", labels = c("3 years","8 years")) +
  theme(legend.position = c(0.7,0.4),text = element_text(size = 14,face = "bold"))
p

ggsave(paste0(pathfigures_tables,"figure_with_GRSZ.jpg"),width = 6.8,height = 6.8)
# 
# library("tidyverse")
# library("officer")
# library("rvg")
# read_pptx() %>%
#   add_slide(layout = "Title and Content", master = "Office Theme") %>%
#   ph_with_vg(code = print(p), type = "body") %>% 
#   print(target = "C:/Users/Lauric/Dropbox/TEDDY prediction model paper/figures and tables/GRScomparison_all.pptx")

compare(TimeRoc1, TimeRoc5, adjusted = FALSE, abseps = 1e-06)
