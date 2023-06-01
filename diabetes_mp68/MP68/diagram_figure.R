#############################################################
## ## ferratlauric@gmail.com - June 2019
#############################################################

# how to count the number of visits in different settings
count_visit_passive <- function(p_passive,year){
  n_visit <- case_when(
    year < 4  ~ p_passive,
    year <8 ~ p_passive/2,
    TRUE ~ 0)
  return(n_visit)
}

count_visit_active <- function(p_active,year){
  n_visit <- case_when(
    year < 1  ~ p_active * 1,
    year < 3  ~ p_active * 4,
    year < 6 ~ p_active * 2,
    year < 8 ~ p_active,
    TRUE ~ p_active/2)
  return(n_visit)
}

# compute visit in the simple adaptative design when people are keep all the time
source(paste0(codePath,'t_AUC - GRS2/cohort_design/simpledesign.R'))
# compute visit in the asimple adaptative design when people are keep if they have a probability to devlop t1d before age 10 above 0.008
source(paste0(codePath,'t_AUC - GRS2/cohort_design/simple_adaptative_cohort.R'))
# compute visit in the an advanced adaptative design when people are closely follow if they have a probability to devlop t1d in the next future above 0.006
source(paste0(codePath,'t_AUC - GRS2/cohort_design/advanced_adaptative_cohort.R'))
# compute cost of visit by removing the fact that in TEDDY some people drop out
source(paste0(codePath,'t_AUC - GRS2/cohort_design/n_visit.R'))



# draw figure
################ COHORT 1 ############

cohort <- c("TEDDY cohort      ")
population <- c(9.3)
Ratio_T1D_cover <- 75
Screening_captured <- 75 
Ratio_T1D_cover_cohort <- 100
mean_n_visit <- 18
median_n_visit <- 18
case_detected_per_visit <- round(mean_n_visit*population/100/(500/100000*Ratio_T1D_cover/100))
Ratio_T1D_cover <- paste0(Ratio_T1D_cover,"%")
Ratio_T1D_cover_cohort <- paste0(Ratio_T1D_cover_cohort,"%")
Screening_captured <- paste0(Screening_captured,"%")
# value <- c( Ratio_T1D_cover,Ratio_T1D_cover_cohort,mean_n_visit,median_n_visit,case_detected_per_visit)
# name <- c("T1D captured (population)","T1D captured (cohort)","visits per person (mean)","visits per person (median)","case detected per autoantibody test")

value <- c( Screening_captured, Ratio_T1D_cover_cohort,Ratio_T1D_cover,mean_n_visit,case_detected_per_visit)
name <- c("Screening captured","follow up retained","final captured"," mean visits/followed child","visits/case captured")

stable <- data.frame(name,value)
stable.p1 <- ggtexttable(stable,rows = NULL, cols = NULL,theme = ttheme(base_style = "default", base_size = 8))
y <- c(1,1)*population
x <- c(0,1)*10
df <- data.frame(x,y)
p1 <- ggplot(df,aes(x = x,y = y)) +
  geom_area(fill = "#009E73") +
  theme_classic() + 
  xlab("time") +
  ylab("propotion of population") +
  scale_y_continuous(breaks = c(0,5,10,population,0),limits = c(0,12)) +
  scale_x_continuous(breaks = c(0,2,4,6,8,10)) 
p1

################ Adaptive simple cohort ############
cohort <- c("Adaptive simple cohort   ")
population <- c(10.7)
Ratio_T1D_cover <- 75
Screening_captured <- 81.5 
Ratio_T1D_cover_cohort <- 92
mean_n_visit <- 10.6
median_n_visit <- 18
case_detected_per_visit <- round(mean_n_visit*population/100/(500/100000*Ratio_T1D_cover/100))
Ratio_T1D_cover <- paste0(Ratio_T1D_cover,"%")
Ratio_T1D_cover_cohort <- paste0(Ratio_T1D_cover_cohort,"%")
Screening_captured <- paste0(Screening_captured,"%")

value <- c( Screening_captured, Ratio_T1D_cover_cohort,Ratio_T1D_cover,mean_n_visit,case_detected_per_visit)
name <- c("Screening captured","follow up retained","final captured"," mean visits/followed child","visits/case captured")

stable <- data.frame(name,value)
stable.p2 <- ggtexttable(stable,rows = NULL, cols = NULL,theme = ttheme(base_style = "default", base_size = 8))
x <- round(day_beginv1/365.25*2)/2
x <- c(0, rep(x[-1],each = 2)) + c(0,rep(c(0,0.000001),length.out = 2*(length(x) - 1)))
# x <- c(0.0,  1.0,  1.0 + 0.00001,  1.5,  1.5 + 0.00001,  2.0,  2.0 + 0.00001,  3.0,  3.0 + 0.00001,  4.0,  4.0 + 0.00001,  5.0,  5.0 + 0.00001, 10.0, 10.0 + 0.00001)
y <- c(rep(ratio_kept_people1,each = 2),0)*population
# y <- (1 - c(0,0,0.8^5*0.5,0.8^5*0.5,0.8^5,0.8^5,0.8^4,0.8^4,0.8^3,0.8^3,0.8^2,0.8^2,0.8,0.8,1))/5
df <- data.frame(x,y)
p2 <- ggplot(df,aes(x = x,y = y)) +
  # geom_line()+
  geom_area(fill = "#009E73") +
  theme_classic() +
  xlab("time") +
  ylab("propotion of population") +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = c(0,5,10,population),limits = c(0,12)) +
  scale_x_continuous(breaks = c(0,2,4,6,8,10)) 

p2

#################### Adaptive advanced cohort ###########

cohort <- c("Adaptive advanced cohort")
population <- c(11.2)
Ratio_T1D_cover <- 75
Screening_captured <- 84 
Ratio_T1D_cover_cohort <- 90
mean_n_visit <- 8.6
median_n_visit <- 18
case_detected_per_visit <- round(mean_n_visit*population/100/(500/100000*Ratio_T1D_cover/100))
Ratio_T1D_cover <- paste0(Ratio_T1D_cover,"%")
Ratio_T1D_cover_cohort <- paste0(Ratio_T1D_cover_cohort,"%")
Screening_captured <- paste0(Screening_captured,"%")
# value <- c( Ratio_T1D_cover,Ratio_T1D_cover_cohort,mean_n_visit,median_n_visit,case_detected_per_visit)
# name <- c("T1D captured (population)","T1D captured (cohort)","visits per person (mean)","visits per person (median)","case detected per autoantibody test")

value <- c( Screening_captured, Ratio_T1D_cover_cohort,Ratio_T1D_cover,mean_n_visit,case_detected_per_visit)
name <- c("Screening captured","follow up retained","final captured"," mean visits/followed child","visits/case captured")

stable <- data.frame(name,value)
stable.p3 <- ggtexttable(stable,rows = NULL, cols = NULL,theme = ttheme(base_style = "default", base_size = 8))

# x <- c(0.0,  1.5,  1.5 + 0.00001,  2.0,  2.0 + 0.00001,  4.0,  4.0 + 0.00001,  6.0 ,  6.0 + 0.00001,  8.0,  8.0 + 0.00001, 10.0, 10.0 + 0.00001)
# y <- (1 - c(0.5,0.5,0.5,0.5,0.8,0.8,0.9,0.9,0.9,0.9,0.9,0.9,0.9))/5

x <- round(day_beginv2/365.25*2)/2
x <- c(0, rep(x[-1],each = 2)) + c(0,rep(c(0,0.000001),length.out = 2*(length(x) - 1)))
y <- c(rep(ratio_kept_people2*population,each = 2),0)

df_d <- data.frame(x,y,2)
colnames(df_d)[3] <- "X1"

y <- c(1,1)*population
x <- c(0,1)*10
df_p <- data.frame(x,y,1)
colnames(df_p)[3] <- "X1"
df <- rbind(df_p,df_d)
df$X1 <- factor(df$X1)
p3 <- ggplot(df,aes(x = x,y = y, fill = X1) ) +
  geom_ribbon(aes(x = x,ymax = y),ymin = 0) +
  scale_fill_manual(name = '', values = c("1" = "#56B4E9", "2" = "#009E73"),labels = c("Reduced follow up","Close follow up")) +
  theme_classic() +
  xlab("time") +
  ylab("propotion of population") +
  theme(legend.position = c(.5,.75),legend.key.width = unit(0.1,"cm"),legend.key.height = unit(0.5,"cm"),legend.text = element_text(size = 9), legend.box = "horizontal",legend.direction = "horizontal") +
  scale_y_continuous(breaks = c(0,5,10,population),limits = c(0,12)) +
  scale_x_continuous(breaks = c(0,2,4,6,8,10)) 

p3
# ggdraw() +
#   draw_plot(p1, x = 0, y = .5, width = .5, height = .5) +
#   draw_plot(p2, x = .5, y = .5, width = .5, height = .5) +
#   draw_plot(p3, x = 0, y = 0, width = 0.5, height = 0.5) +
#   # draw_plot(p4, x = 0.5, y = 0.25, width = 0.5, height = 0.25) +
#   draw_plot(stable.p, x = 0.5, y = 0, width = 0.5, height = 0.5) +
#   draw_plot_label(label = c("A", "B", "C"), size = 15,
#                   x = c(0, 0.5, 0), y = c(1, 1, 0.5))


# ggdraw() +
#   draw_plot(p1, x = 0.02, y = 0.67, width = 0.5, height = .33) +
#   draw_plot(stable.p1, x = 0.5, y = 0.67, width = 0.5, height = .33) +
#   draw_plot(p2, x = 0.02, y = 0.34, width = 0.5, height = .33) +
#   draw_plot(stable.p2, x = 0.5, y = 0.34, width = 0.5, height = .33) +
#   draw_plot(p3, x = 0.02, y = 0, width = 0.5, height = 0.33) +
#   draw_plot(stable.p3, x = 0.5, y = 0, width = 0.5, height = .33) +
#   draw_plot_label(label = c("A", "B", "C"), size = 15,
#                   x = c(0, 0, 0), y = c(1, 0.67, 0.34))

# ggsave("C:/Users/Lauric/Dropbox/TEDDY prediction model paper/figures and tables/supplementary/figure_cohorts.jpg",width = 6.8, height = 7.65 )



p4 <- ggdraw() +
  draw_plot(p1, x = 0.5, y = 0.67, width = 0.5, height = .33) +
  draw_plot(p2, x = 0.5, y = 0.34, width = 0.5, height = .33) +
  draw_plot(p3, x = 0.5, y = 0, width = 0.5, height = 0.33) +
  draw_plot_label(label = c("a", "b", "c","d","e","f"), size = 15,
                  x = c(0, 0, 0,0.48,0.48,0.48), y = c(1, 0.67, 0.34, 1, 0.67, 0.34))
p4
ggsave(paste0(pathfigures_tables,"figure2.jpg"),width = 6.8, height = 8 )

#read_pptx() %>%
#  add_slide(layout = "Title and Content", master = "Office Theme") %>%
#  ph_with_vg(code = print(p4), type = "body",width = 6.8, height = 8) %>%
#  print(target = paste0(pathfigures_tables,"figure2.pptx"))

