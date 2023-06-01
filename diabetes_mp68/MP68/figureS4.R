##### main t-AUC
#############################################################
## ## ferratlauric@gmail.com - Novembre 2018
#############################################################
# library to load

method <- "Cox"
# complexity <- "full_model_1"
complexity <- "abn_grs_fdr"
day_beginv <- c(365.25,365.25 * 2,1460.5) + 45
AUCv <- c(0.81,0.92,0.96)
landmarkv <- c("1 year", "2 years", "4 years")
n_time_begin <- length(day_beginv)

p_score_distribution <- list()
p_calibration <- list()
for (i in 1:(n_time_begin)) {
  day <- day_beginv[i]
  dayend = 3650.5
  source(paste0(codePath,"Extract_information_per_date.R"))
  dataset_ml <- finaldata %>% filter(t1d == 1| last_clinic_visit_agedys >  day + 5*365.25)
  #load Cox model
  names_tAUC <- paste0("Cox_model_","_complexity_",complexity,"_day_begin_",day)
  load(file = paste0(pathSaveModels,names_tAUC,".RData"))
  
  #create score Cox model
  lp.pred <- predict(res.cox, dataset_ml, type = "lp")
  # Baseline Function
  base <- basehaz(res.cox)
  
  base_time <- base[ which.min(abs(base$time - (day + 5*365.25))),"hazard"]
  #https://stats.stackexchange.com/questions/288393/calculating-survival-probability-per-person-at-time-t-from-cox-ph
  Pred.val_1 <- 1 - exp(-base_time[1])^exp(lp.pred)
  score <- lp.pred
  dataset_ml$t1d <- ifelse(dataset_ml$t1d_diag_agedys >  (day + 5 * 365.25),0,as.numeric(dataset_ml$t1d)-1)
  data_d <- data.frame(Pred.val_1,dataset_ml$t1d,score)
  colnames(data_d) <- c("proba","t1d","score")
  data_d$t1d <- factor(data_d$t1d)
  
  predictionVobs_proba <- data_d %>%
    mutate(p_group = floor(proba*10)/10) %>%
    ungroup() %>%
    mutate(p_group = factor(p_group)) %>%
    mutate(p_group = recode(p_group, `1` = "0.9")) %>% 
    mutate(obs = t1d)
  
  # Make groups on basis of the predicted probabilities
  # after they are sorted from low to high
  group.cut <- quantile(predictionVobs_proba$proba, c(seq(0, 1, 0.005)))
  group <- cut(predictionVobs_proba$proba, group.cut)
  pred.prob <- tapply(predictionVobs_proba$proba, group, mean)
  
  # Observed probabilities
  obs.prob <- tapply(as.numeric(predictionVobs_proba$t1d)-1, group, mean)
  obs.prob.sd <- tapply(as.numeric(predictionVobs_proba$t1d)-1, group, sd)
  total <- tapply(as.numeric(predictionVobs_proba$t1d)-1, group, length)
  groupID <- 1:length(levels(group)) 
  dataplot <- data.frame(pred.prob,obs.prob,groupID)
  confidencev <- 2.23
  dataplot <- dataplot %>% group_by(groupID) %>%
    mutate( obs.low = max(obs.prob - confidencev*sqrt(obs.prob*(1 - obs.prob)/total),0),
            obs.up = min(obs.prob + confidencev*sqrt(obs.prob*(1 - obs.prob)/total),1)) %>% 
    ungroup()
  # Calibration plot
  p_calibration[[i]] <- ggplot(dataplot, aes(x = pred.prob, y = obs.prob)) +
    geom_abline(slope = 1) +
    geom_point() +
    geom_errorbar(aes(x = pred.prob, ymin = obs.low, ymax = obs.up)) +
    xlim(c(0,1)) + ylim(c(0,1)) +
    ggtitle(paste0("Landmark: ",landmarkv[i])) +
    xlab("prediction probability") + ylab("observation ratio") +
    # ggtitle(paste0("Calibration plot at landmark ",round(day/365.25)," years, horizon time of 5 years ")) +
    theme(axis.text = element_text(size = sizetext,face = face),text = element_text(size = sizetext + 2,face = face))
  
  
  p_score_distribution[[i]] <- ggplot(data_d, aes(x = score)) +
    geom_density(aes(fill = t1d),alpha = 0.7) +
    ylab("density") +
    xlab("score") +
    ggtitle(paste0("AUC: ",AUCv[i]," - Landmark: ",landmarkv[i])) +
    theme(axis.text = element_text(size = sizetext,face = face),text = element_text(size = sizetext + 2,face = face), legend.position = c(0.5,0.7)) +
    scale_fill_discrete(name = "",labels = c("T1D free","T1D")) 
}


############## score distribution ###########
p1 <- ggdraw(p_score_distribution[[1]])
p2 <- ggdraw(p_score_distribution[[2]])
p3 <- p_score_distribution[[3]]
p <- ggdraw() +
  draw_plot(p1, x = 0, y = 0, width = 0.33, height = 1) +
  draw_plot(p2, x = 0.34, y = 0, width = 0.33, height = 1) +
  draw_plot(p3, x = 0.67, y = 0, width = 0.33, height = 1) +
  draw_plot_label(label = c("A", "B", "C"), size = 15,
                  x = c(0, 0.34, 0.67), y = c(1, 1, 1))
p
# ggsave("C:/Users/Lauric/Desktop/Postdoc/t_AUC_GRS2/figures/figure_score_distribution.jpg",width = 11.84, height = 7.65 )


# read_pptx() %>%
#   add_slide(layout = "Title and Content", master = "Office Theme") %>%
#   ph_with_vg(code = print(p), type = "body") %>% 
#   print(target = "C:/Users/Lauric/Dropbox/TEDDY prediction model paper/figures and tables/figure_score_distribution.pptx")

################ calibration ##############

p1 <- ggdraw(p_calibration[[1]])
p2 <- ggdraw(p_calibration[[2]])
p3 <- ggdraw(p_calibration[[3]])
p <- ggdraw() +
  draw_plot(p1, x = 0, y = 0, width = 0.33, height = 1) +
  draw_plot(p2, x = 0.34, y = 0, width = 0.33, height = 1) +
  draw_plot(p3, x = 0.67, y = 0, width = 0.33, height = 1) +
  draw_plot_label(label = c("A", "B", "C"), size = 15,
                  x = c(0, 0.34, 0.67), y = c(1, 1, 1))
p
# ggsave("C:/Users/Lauric/Desktop/Postdoc/t_AUC_GRS2/figures/figure_calibration_plot.jpg",width = 11.84, height = 7.65 )

################# all together ############
# read_pptx() %>%
# add_slide(layout = "Title and Content", master = "Office Theme") %>%
# ph_with_vg(code = print(p), type = "body") %>% 
# print(target = "C:/Users/Lauric/Dropbox/TEDDY prediction model paper/figures and tables/figure_calibration_plot.pptx")



AUC1 <- ggdraw(p_score_distribution[[1]])
AUC2 <- ggdraw(p_score_distribution[[2]])
AUC3 <- p_score_distribution[[3]]
C1 <- ggdraw(p_calibration[[1]])
C2 <- ggdraw(p_calibration[[2]])
C3 <- ggdraw(p_calibration[[3]])
p <- ggdraw() +
  draw_plot(AUC1, x = 0, y = 0.5, width = 0.33, height = 0.5) +
  draw_plot(AUC2, x = 0.33, y = 0.5, width = 0.33, height = 0.5) +
  draw_plot(AUC3, x = 0.66, y = 0.5, width = 0.33, height = 0.5) +
  draw_plot(C1, x = 0, y = 0, width = 0.33, height = 0.5) +
  draw_plot(C2, x = 0.33, y = 0, width = 0.33, height = 0.5) +
  draw_plot(C3, x = 0.66, y = 0, width = 0.33, height = 0.5) +
  draw_plot_label(label = c("a", "b", "c","d","e","f"), size = 15,
                  x = c(0, 0.34, 0.6,0, 0.34, 0.67), y = c(1, 1, 1,0.5,0.5,0.5))
p
ggsave(paste0(pathfigures_tables,"figureS4.jpg"),width = 11.84, height = 7.65 )


# read_pptx() %>%
#   add_slide(layout = "Title and Content", master = "Office Theme") %>%
#   ph_with_vg(code = print(p), type = "body") %>% 
#   print(target = "C:/Users/Lauric/Dropbox/TEDDY prediction model paper/figures and tables//supplementary/model_performance.pptx")
# 
# 
