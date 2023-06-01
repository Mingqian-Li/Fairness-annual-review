# main load

#############################################################
## ## ferratlauric@gmail.com - May 2019
#############################################################
result_time_serie_proba <- list()
complexity <- "abn_grs_fdr"

nyv <- c(1,3,5)
day <- 365.25 * 2 + 45
dayend = Inf
source(paste0(codePath,"Extract_information_per_date.R"))
# Generate differnt typical children to whom predict the probability to develop T1D
names_tAUC <- paste0("Cox_model_","_complexity_",complexity,"_day_begin_",day)
load(file = paste0(pathSaveModels,names_tAUC,".RData"))
fdr <- rep(c(0,1,0,1), 4)
GRS2 <- rep(c(13.25386,13.25386,15.60408,15.60408),4) # 90 and 99% percentile in UK bio bank
number_autoantibody <- rep(c(0,1,2,3),each = 4)
peopleID <- 1:16
dataset_ml <- data.frame(peopleID,fdr, GRS2, number_autoantibody) %>% mutate(fdr = factor(fdr), number_autoantibody = factor(number_autoantibody))

# compute the probability to develop T1D
for (people in peopleID) {
  k <- 1
  personID <- people
  # collect the coefficient for each landmark point
  prediction_df <- matrix(0,length(nyv) , 3)
  for (i in nyv) {
    ny <- i
    print(i)
    
    if (dim(dataset_ml[dataset_ml$peopleID == personID,])[1] != 0) {
      prediction_i <- predict(res.cox, dataset_ml[dataset_ml$peopleID == personID,], type = "risk") # https://kkoehler.public.iastate.edu/stat565/coxph.4page.pdf
      marker_score <- predict(res.cox, finaldata, type = "risk")
      delta <- as.numeric(finaldata$t1d) - 1
      
      SpeandSen_i <- SeSpPPVNPV(cutpoint =  prediction_i, T = finaldata$last_clinic_visit_agedys, delta = delta, marker = marker_score, cause = 1,weighting = "marginal", times = day + i*365.25, iid = FALSE)
      prediction_df[k,] <- c( ny,SpeandSen_i$TP[2],SpeandSen_i$FP[2])
      k <- k + 1
      
      TP <- sum((marker_score > prediction_i) & (delta  == 1))
      FP <- sum((marker_score > prediction_i) & (delta  == 0))
      TN <- sum((marker_score < prediction_i) & (delta  == 0))
      FN <- sum((marker_score < prediction_i) & (delta  == 1))
      sens <- TP / (TP + FN)
      print(sens)
      spe <- TN/(TN +FP)
      print(spe)
      TPc <- sum((finaldata$number_autoantibody == 3 & finaldata$GRS2 > 16.5) & (delta  == 1) & (finaldata$last_clinic_visit_agedys < 3 * 365))
      FPc <- sum((finaldata$number_autoantibody == 3 & finaldata$GRS2 > 16.5) & ((delta  == 0)  | (finaldata$last_clinic_visit_agedys > 3 * 365)) )
      print(TPc/(TPc+FPc))
      TPc <- sum((finaldata$number_autoantibody == 3 & finaldata$GRS2 < 14) & (delta  == 1) & (finaldata$last_clinic_visit_agedys < 3 * 365))
      FPc <- sum((finaldata$number_autoantibody == 3 & finaldata$GRS2 < 14) & ((delta  == 0)  | (finaldata$last_clinic_visit_agedys > 3 * 365)) )
      print(TPc/(TPc+FPc))
      TPc <- sum((finaldata$number_autoantibody == 3 & finaldata$GRS2 > 16.5) & (delta  == 1) & (finaldata$last_clinic_visit_agedys < 7 * 365))
      FPc <- sum((finaldata$number_autoantibody == 3 & finaldata$GRS2 > 16.5) & ((delta  == 0)  | (finaldata$last_clinic_visit_agedys > 7 * 365)) )
      print(TPc/(TPc+FPc))
      TPc <- sum((finaldata$number_autoantibody == 3 & finaldata$GRS2 < 14) & (delta  == 1) & (finaldata$last_clinic_visit_agedys < 7 * 365))
      FPc <- sum((finaldata$number_autoantibody == 3 & finaldata$GRS2 < 14) & ((delta  == 0)  | (finaldata$last_clinic_visit_agedys > 7 * 365)) )
      print(TPc/(TPc+FPc))
    }
  }
  prediction_df <- data.frame(prediction_df)
  colnames(prediction_df) <- c("time", "TP","FP")
  #prediction_df$ID <- personID
  # confidence interval formula log transformation  eq 4.3.2 from Klein, John P.  Moeschberger, Melvin L. 2003
  # A Note on Confidence Intervals and Bands for the Survival Function Based on Transformations Borgan, Ørnulf Liestøl, Knut
  result_time_serie_proba[[personID]] <- prediction_df
}

# Reshape the results to export them
proba_times_series <- bind_rows(result_time_serie_proba,.id = "personID")

proba_times_series2 <- proba_times_series %>% 
  filter( time != 0) %>% 
  mutate(Sensitivity = TP*100) %>% 
  mutate(Specificity = (100 - FP*100)) %>% 
  mutate(personID = as.numeric(personID))

# table_proba <- proba_times_series2 %>% mutate( display_P = paste0(round(proba*10)/10," [", round(lower*10)/10," - ",round(upper*10)/10,"]")) %>% select(time, personID, display_P) 
table_Sen<- proba_times_series2 %>%  select(time, personID, Sensitivity) %>% spread(time, Sensitivity) 
table_Spe<- proba_times_series2 %>%  select(time, personID, Specificity) %>% spread(time, Specificity)
table_Spe_Sen <- table_Spe %>% left_join(table_Sen, by = c("personID" = "personID" ))
fdr <- rep(c("No","Yes","No","Yes"), 4)
GRS2 <- rep(c("++","++","+++","+++"),4) # 90 and 99% percentile
number_autoantibody <- rep(c("0","1","2","3"),each = 4)
peopleID <- 1:16
dataset_label <- data.frame(peopleID,fdr, GRS2, number_autoantibody)
label <- dataset_label
table_proba_prediction <- left_join(label,table_Spe_Sen, by = c("peopleID" = "personID")) %>%  select(-peopleID)
names(table_proba_prediction) <- c("Family history","Genetic risk","Autoantibody statue","Spe 1 year horizon","Spe 3 year horizon", "Spe 5 year horizon","Se 1 year horizon","Se 3 year horizon", "Se 5 year horizon")



table_proba_prediction <- table_proba_prediction %>% select(c("Autoantibody statue","Genetic risk","Family history","Se 1 year horizon","Spe 1 year horizon","Se 3 year horizon","Spe 3 year horizon", "Se 5 year horizon", "Spe 5 year horizon"))
# export table to word
myft <- regulartable(
  table_proba_prediction)
myft <- set_header_labels(myft, `Se 1 year horizon` = "1 year horizon",`Spe 1 year horizon` = "1 year horizon",`Se 3 year horizon` = "3 year horizon",`Spe 3 year horizon`= "3 year horizon", `Se 5 year horizon` = "5 year horizon", `Spe 5 year horizon` = "5 year horizon")
myft <-  merge_at( myft, i = 1, j = 4:5, part = "header" )
myft <-  merge_at( myft, i = 1, j = 6:7, part = "header" )
myft <-  merge_at( myft, i = 1, j = 8:9, part = "header" )
myft <- add_header_row(myft, values =c("Autoantibody statue","Genetic risk","Family history",rep(c("Sensitivity","Specificity"),3)), top = FALSE)
myft <-  merge_at( myft, i = 1:2, j = 1, part = "header" )
myft <-  merge_at( myft,i = 1:2, j = 2, part = "header" )
myft <-  merge_at( myft,i = 1:2, j = 3, part = "header" )
myft <- theme_vanilla(myft)
myft


myft <- autofit(myft)
myft <- align( myft, align = "center", part = "all" )
myft
doc <- read_docx()
doc <- body_add_flextable(doc, value = myft)
print(doc, target = paste0(pathfigures_tables,"table2_Spec_sen.docx"))