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
fdr <- rep(c(0,1,0,1,0,1), 4)
GRS2 <- rep(c(12.4,12.4,13.25386,13.25386,15.60408,15.60408),4) # 80th, 90th and 99th percentile in UK bio bank
number_autoantibody <- rep(c(0,1,2,3),each = 6)
peopleID <- 1:24
dataset_ml <- data.frame(peopleID,fdr, GRS2, number_autoantibody) %>% mutate(fdr = factor(fdr), number_autoantibody = factor(number_autoantibody))

# compute the probability to develop T1D
for (people in peopleID) {
  k <- 1
  personID <- people
  # collect the coefficient for each landmark point
  prediction_df <- matrix(0,length(nyv) , 7)
  for (i in nyv) {
    ny <- i
    print(i)
    
    if (dim(dataset_ml[dataset_ml$peopleID == personID,])[1] != 0) {
      prediction_i <- summary(survfit(res.cox,dataset_ml[dataset_ml$peopleID == personID,]),times = day + ny*365.25) # https://kkoehler.public.iastate.edu/stat565/coxph.4page.pdf
      prediction_df[k,] <- c(prediction_i$time,prediction_i$n.risk,prediction_i$n.event,prediction_i$surv,prediction_i$std.err,prediction_i$lower,prediction_i$upper)
      k <- k + 1
    }
  }
  prediction_df <- data.frame(prediction_df)
  colnames(prediction_df) <- c("time", "n.risk", "n.event","survival","std.err","lower","upper")
  #prediction_df$ID <- personID
  # confidence interval formula log transformation  eq 4.3.2 from Klein, John P.  Moeschberger, Melvin L. 2003
  # A Note on Confidence Intervals and Bands for the Survival Function Based on Transformations Borgan, Ørnulf Liestøl, Knut
  result_time_serie_proba[[personID]] <- prediction_df
}

# Reshape the results to export them
proba_times_series <- bind_rows(result_time_serie_proba,.id = "personID")
proba_times_series2 <- proba_times_series %>%
  filter( time != 0) %>% 
  mutate( time = round((time - 2 * 365.25)/365.25*2)/2) %>% 
  mutate(proba = (1 - survival)*100) %>% 
  mutate(lower1 = (1 - upper)*100) %>% 
  mutate(upper = (1 - lower)*100) %>% 
  mutate(lower = lower1)

T1D_onset <- finaldata[finaldata$MP68_MaskID %in% peopleID,c("MP68_MaskID","t1d","t1d_diag_agedys")]
T1D_onset$MP68_MaskID <- as.character(T1D_onset$MP68_MaskID)
T1D_onset$t1d_diag_agedys <- T1D_onset$t1d_diag_agedys/365.25
T1D_onset_y <- proba_times_series2 %>% 
  group_by(personID) %>% 
  top_n(1,time) %>% 
  ungroup() %>% 
  mutate(proba = 1 - survival) %>% 
  select(proba,personID)
T1D_onset_d <- left_join(T1D_onset_y,T1D_onset, by = c("personID" = "MP68_MaskID")) %>% 
  filter(t1d == 1) %>% 
  mutate(time = t1d_diag_agedys) %>% 
  select(personID,time,t1d,proba)

proba_times_series3 <- proba_times_series2 %>% 
  mutate(t1d = 0,
         proba = 1 - survival) %>% 
  select(personID,time,t1d,proba)
data_proba  <- rbind(proba_times_series3, T1D_onset_d)                       


proba_times_series2

table_proba <- proba_times_series2 %>% mutate( display_P = paste0(round(proba*10)/10," [", round(lower*10)/10," - ",round(upper*10)/10,"]")) %>% select(time, personID, display_P) 
table_proba <- table_proba %>%  spread(time, display_P) %>% mutate(personID = as.numeric(personID))


fdr <- rep(c("No","Yes","No","Yes","No","Yes"), 4)
GRS2 <- rep(c("++","++","+++","+++","++++","++++"),4) # 90 and 99% percentile
number_autoantibody <- rep(c("0","1","2","3"),each = 6)
peopleID <- 1:24
dataset_label <- data.frame(peopleID,fdr, GRS2, number_autoantibody)
label <- dataset_label
table_proba_prediction <- left_join(label,table_proba, by = c("peopleID"="personID")) %>% select(number_autoantibody,GRS2,fdr,`1`,`3`,`5`)
names(table_proba_prediction) <- c("Autoantibody statue","Genetic risk","family history","1 year horizon","3 year horizon", "5 year horizon")

# export table to word
myft <- regulartable(
  table_proba_prediction)
myft
myft <- theme_vanilla(myft)
myft


myft <- autofit(myft)
myft <- align( myft, align = "center", part = "all" ) 
myft
doc <- read_docx()
doc <- body_add_flextable(doc, value = myft)
print(doc, target = paste0(pathfigures_tables,"table2_new_all.docx"))
