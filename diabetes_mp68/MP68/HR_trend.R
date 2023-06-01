library("dplyr")
library("ggplot2")
library("plotly")
library(survival)
daysv <- c(60,365.25,365.25 + 183,365.25 * 2,1095.5,1460.5,1825.5,2190.5,2555.5) + 45

result_plot <- list()
complexity <- "abn_grs_fdr"
k <- 1

# collect the coefficient for each landmark point
for (i in daysv) {
  print(i)
  day <- i
  k <- k + 1
  names_tAUC <- paste0("Cox_model_","_complexity_",complexity,"_day_begin_",day)
  load(file = paste0(pathSaveModels,names_tAUC,".RData"))
  x <- summary(res.cox)
  #beta<-signif(x$coef[,2], digits=2);#coeficient beta
  HR <- signif(x$coef[,2], digits = 3);#exp(beta)
  HR.confint.lower <- signif(x$conf.int[,"lower .95"], 3)
  HR.confint.upper <- signif(x$conf.int[,"upper .95"],3)
  
  res <- data.frame(names(HR),HR,HR.confint.lower,HR.confint.upper)
  colnames(res) <- c("variables","HR","lower","upper")
  
  result_plot[[as.character(day)]] <- res
}

Importance_scores2 <- bind_rows(result_plot,.id = "times")
Importance_scores <- Importance_scores2 %>%
  mutate(times_n = as.numeric(times)/365.25,
         times = case_when(
           times == "60" ~"birth",
           times == "183"~"6 months",
           times == "365.25" ~"1 year",
           times == "548.25" ~"18 months",
           times == "730.5" ~"2 years",
           times == "1095.5" ~"3 years",
           times == "1460.5" ~"4 years",
           times == "1825.5" ~"5 years",
           times == "2191.5" ~"6 years",
           times == "2556.75" ~"7 years",
           times == "2922" ~"8 years",
           times == "3287.25" ~"9 years",
           times == "3652.5" ~"10 years",
           times == "4017.75" ~"11 years",
           TRUE ~ "error"))
Importance_scores <- left_join(expand.grid(times = unique(Importance_scores$times),variables = unique(Importance_scores$variables)),Importance_scores)
Importance_scores <- Importance_scores %>%
  mutate(variables = case_when(
    variables == "GRS2" ~ "Genetic risk score",
    variables == "fdr1" ~ "First degree relative",
    variables == "number_autoantibody" ~ "number of autoantibodies",
    variables == "number_autoantibody1" ~ "1 autoantibody",
    variables == "number_autoantibody2" ~ "2 autoantibodies",
    variables == "number_autoantibody3" ~ "3 autoantibodies"))
potentialdate <- c("birth", "6 months", "1 year","18 months", "2 years", "3 years", "4 years", "5 years","6 years","7 years", "8 years","9 years", "10 years","11 years")
present_date <-  potentialdate %in% Importance_scores$times
level_time <- potentialdate[present_date]

potential_variable <- c("Genetic risk score","First degree relative","number of autoantibodies","1 autoantibody","2 autoantibodies","3 autoantibodies")
present_variable <-  potential_variable %in% Importance_scores$variables
level_variable <- potential_variable[present_variable ]
Importance_scores <- Importance_scores %>%
  mutate(times = factor(times, levels = level_time),
         variables = factor(variables, levels = level_variable))
nlow <- -1.5
nhight <- 1.5



p <- ggplot(Importance_scores, aes(x = HR, y = variables)) +
  geom_point(aes(frame = times, ids = variables),size = 6) +
  theme_bw() +
  geom_vline(aes(frame = times, ids = variables, xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(frame = times, ids = variables, xmax = upper, xmin = lower), size = .5, height = .2, color = "gray50") 

ggplotly(p,tooltip = c("all")) %>%
  animation_opts(
    frame = 100, 
    transition = 0, 
    redraw = TRUE
  )
p_parameters <- ggplot(Importance_scores, aes(x = times_n, y = HR)) +
  geom_point(aes(colour = variables),size = 6,alpha = 0.5) +
  geom_line(aes(colour = variables),size = 0.5) + 
  xlab("Age at Prediction Scoring (years)") + 
  ylab("Hazard ratio") +
  #coord_cartesian(ylim = c(0,100)) +
  scale_y_log10(limits = c(0.5,1.6e3), expand = c(0, 0)) +
  geom_ribbon(aes(ymin = lower,ymax = upper,fill = variables),alpha = 0.1,show.legend = FALSE, colour = "transparent") +
  # ggtitle("Fig 1. Time-dependent AUC in the Teddy database for\n several values of landmark age and horizon times") + 
  theme_bw(base_size =18) +
  theme(legend.position = c(0.25, 0.85), legend.title = element_blank())
p_parameters
ggsave(paste0(pathfigures_tables,"HR_change_",complexity,".jpg"),width = 6.8, height = 6.8)


library("tidyverse")
library("officer")
library("rvg")

