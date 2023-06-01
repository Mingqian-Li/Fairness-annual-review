
##### main t-AUC
#############################################################
## ## ferratlauric@gmail.com - Novembre 2018
#############################################################
# library to load

set.seed(112)
source(paste0(codePath,"/t_AUC - GRS2/list_variable_f.R"))
source(paste0(codePath,'t_AUC - GRS2/article_figures/tAUC_ml_v_CV.R'))

method1 <- "Cox"
complexity1 <- "abn"
# 
day_beginv <- c(60,365.25,365.25 + 183,365.25*2 + seq(0,8*365.25,365)) + 45
day_endv <- c(365.25,365.25*3,5*365.25,8*365.25)

n_time_begin <- length(day_beginv)
n_time_end <- length(day_endv)

day = 365.25*4
dayend = Inf
source(paste0(codePath,"Extract_information_per_date.R"))
dataset_ml <- finaldata
TimeRoc1 <- tAUC_ml_v_CV(complexity1,dataset_ml,dataset_ml,day,day + day_endv, variable_of_interest = "t1d")
data1 <- dplyr::bind_rows(TimeRoc1,.id = "year")
data1 <- data1 %>% mutate(year = case_when(
  year == 1 ~ "year1",
  year == 2 ~ "years3",
  year == 3 ~ "years5",
  year == 4 ~ "years8"),
  year = factor(year)) 

ggplot(data1, aes(x = FP,y = TP, colour = year)) +
  geom_line(size = 2) +
  coord_equal() +
  style_roc() +
  scale_colour_discrete(labels = c("1 year","3 years","5 years","8 years"))

###################################
method2 <- "Cox"
complexity2 <- "abn_grs_fdr"
day_beginv <- c(60,365.25,365.25 + 183,365.25*2 + seq(0,8*365.25,365))
# day_beginv <- c(365.25*5 + seq(0,3*365.25,365))

day_endv <- c(365.25,365.25*3,5*365.25,8*365.25)

n_time_begin <- length(day_beginv)
n_time_end <- length(day_endv)

day = 365.25*4
dayend = Inf
source(paste0(codePath,"Extract_information_per_date.R"))
dataset_ml <- finaldata
TimeRoc2 <- tAUC_ml_v_CV(complexity2,dataset_ml,dataset_ml,day,day + day_endv, variable_of_interest = "t1d")
data2 <- dplyr::bind_rows(TimeRoc2,.id = "year")
data2 <- data2 %>% mutate(year = case_when(
  year == 1 ~ "year1",
  year == 2 ~ "years3",
  year == 3 ~ "years5",
  year == 4 ~ "years8"),
  year = factor(year)) 

ggplot(data2, aes(x = FP,y = TP, colour = year)) +
  geom_line(size = 2) +
  coord_equal() +
  style_roc() +
  scale_colour_discrete(labels = c("1 year","3 years","5 years","8 years"))


######################### model 3


#method3 <- "Cox"
#complexity3 <- "full_model_pvalue"
#day_beginv <- c(60,365.25,365.25 + 183,365.25*2 + seq(0,8*365.25,365))
# day_beginv <- c(365.25*5 + seq(0,3*365.25,365))

#day_endv <- c(365.25,365.25*3,5*365.25,8*365.25)

#n_time_begin <- length(day_beginv)
#n_time_end <- length(day_endv)

#day = 365.25*4
#dayend = Inf
#source(paste0(codePath,"Extract_information_per_date.R"))
#dataset_ml <- finaldata
#TimeRoc3 <- tAUC_ml_v_CV(complexity3,dataset_ml,dataset_ml,day,day + day_endv, variable_of_interest = "t1d")
#data3 <- dplyr::bind_rows(TimeRoc3,.id = "year")
#data3 <- data3 %>% mutate(year = case_when(
#  year == 1 ~ "year1",
#  year == 2 ~ "years3",
#  year == 3 ~ "years5",
#  year == 4 ~ "years8"),
#  year = factor(year)) 

#ggplot(data3, aes(x = FP,y = TP, colour = year)) +
#  geom_line(size = 2) +
#  coord_equal() +
#  style_roc() +
#  scale_colour_discrete(labels = c("1 year","3 years","5 years","8 years"))

######################### combined model

data1$model <- "ab"
data2$model <- "grs+fdr+ab"
#data3$model <- "grs+fdr+ab+country+sinusitis+weight"
data_all <- rbind(data1,data2) 
data_all <- data_all[data_all$year == "years3" | data_all$year == "years8",]
data_all$model <- factor(data_all$model)

# http://colorbrewer2.org/#type=qualitative&scheme=Set1&n=8
p_4 <- ggplot(data_all, aes(x = FP,y = TP, colour = year)) +
  geom_line(aes(linetype = model),size = 1.2) +
  coord_equal() +
  style_roc(ylab = "Sensitivity", xlab = "1 - Specificity") +
  scale_linetype_manual(name = "Model Components",values = c(2,1,3), labels = c("AB only","AB+GRS2+FH","AB+GRS2+FH+country+sinusitis+weight")) +
  scale_colour_manual(name = "Future Prediction Interval (Horizon)",values = c("#4daf4a","#984ea3"), labels = c("3 years","8 years")) +
  theme(legend.position = c(0.55,0.3),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.key.width = unit(1.5,"cm")) 
p_4
# ggsave("C:/Users/Lauric/Dropbox/TEDDY prediction model paper/figures and tables/figure1PanelA2.jpg")
# 
# library("tidyverse")
# library("officer")
# library("rvg")
# read_pptx() %>%
#   add_slide(layout = "Title and Content", master = "Office Theme") %>%
#   ph_with_vg(code = print(p), type = "body") %>%
#   print(target = "C:/Users/Lauric/Dropbox/TEDDY prediction model paper/figures and tables/figure1PanelA2.pptx")
# 
# compare(TimeRoc2, TimeRoc3, adjusted = FALSE, abseps = 1e-06)


# p2 <- ggplot(data_all, aes(x = FP,y = TP, colour = year)) +
#   geom_line(aes(linetype = model),size = 1.2) +
#   coord_equal() +
#   style_roc(ylab = "TRUE positive rate", xlab = "False positive rate") +
#   scale_linetype_manual(name = "Model Components",values = c(2,1,3), labels = c("AB only","AB+GRS2+FH","AB+GRS2+FH+\ncountry+sinusitis+weight")) +
#   scale_colour_manual(name = "Future Prediction Interval (Horizon)",values = c("#4daf4a","#984ea3"), labels = c("3 years","8 years")) +
#   theme(axis.text = element_text(size = 16),
#         axis.title = element_text(size = 16),
#         legend.text = element_text(size = 16),
#         legend.title = element_text(size = 16),
#         legend.key.width = unit(1.5,"cm")) +
#   geom_abline(intercept = 0, slope = 1)
# p2 



# library("tidyverse")
# library("officer")
# library("rvg")
# read_pptx() %>%
#   add_slide(layout = "Title and Content", master = "Office Theme") %>%
#   ph_with_vg(code = print(p2), type = "body") %>%
#   print(target = "C:/Users/Lauric/Dropbox/TEDDY prediction model paper/figures and tables/figure1PanelA_ada.pptx")
