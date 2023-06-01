
table_UKB <- read.delim(paste0(dataPath,"ROC_table_UKB.txt"))


sensitivity_initial <- c(table_UKB[which.min(abs(table_UKB$sensitivity-0.7)),"sensitivity"],table_UKB[which.min(abs(table_UKB$sensitivity-0.75)),"sensitivity"],table_UKB[which.min(abs(table_UKB$sensitivity-0.80)),"sensitivity"],table_UKB[which.min(abs(table_UKB$sensitivity-0.85)),"sensitivity"],table_UKB[which.min(abs(table_UKB$sensitivity-0.90)),"sensitivity"],table_UKB[which.min(abs(table_UKB$sensitivity-0.95)),"sensitivity"],table_UKB[which.min(abs(table_UKB$sensitivity-0.99)),"sensitivity"])
population_initial <- 1 - c(table_UKB[which.min(abs(table_UKB$sensitivity-0.7)),"centile"],table_UKB[which.min(abs(table_UKB$sensitivity-0.75)),"centile"],table_UKB[which.min(abs(table_UKB$sensitivity-0.80)),"centile"],table_UKB[which.min(abs(table_UKB$sensitivity-0.85)),"centile"],table_UKB[which.min(abs(table_UKB$sensitivity-0.90)),"centile"],table_UKB[which.min(abs(table_UKB$sensitivity-0.95)),"centile"],table_UKB[which.min(abs(table_UKB$sensitivity-0.99)),"centile"])

source(paste0(codePath,"t_AUC - GRS2/cohort_design/Cohort_advanced_by_proba.R"))
load(paste0(pathSaveModels,"cohort_advanced_proba_advanced.Rdata"))
source(paste0(codePath,"t_AUC - GRS2/cohort_design/Cohort_selections_by_proba.R"))
load(paste0(pathSaveModels,"cohort_selection_proba_simple.Rdata"))

sensitivity_cohort <-  c(1,sensitivity_cohort_simple,sensitivity_cohort_advanced)
n_visit <- c(18,n_visit_simple,n_visit_advanced - 1)
pthreshold <- c(1,proba_simple,proba_advanced)
data.frame(sensitivity_cohort, n_visit)

cohort <- rep(c("A",rep("B",length(sensitivity_cohort_simple)),rep("C",length(sensitivity_cohort_advanced))),length(sensitivity_initial))
sensitivity_population <- c(sensitivity_cohort %o% sensitivity_initial)
total_visit <- c(n_visit %o% population_initial)

data_cohort <- data.frame(cohort,sensitivity_population,total_visit)

ggplot(data_cohort, aes(x = sensitivity_population, y = total_visit, colour = cohort)) +
  geom_point() + 
  xlim(c(0.7,1))



cohort <- c("Simple",rep("Simple adaptative",length(sensitivity_cohort_simple)),rep("Advanced adaptative",length(sensitivity_cohort_advanced)))

# sensitivity_population_goal <- c(0.5,0.6,0.7,0.75,0.8,0.85,0.9)
sensitivity_population_goal <- c(0.75)
j <- 1
  population_initial_cumT1D <- c()
  sensitivity_n <- sensitivity_population_goal[j]
  sensitivity_for_population <- c()
  for (i in sensitivity_cohort) {
    y <- sensitivity_n/i
    sensitivity_for_population <- c(sensitivity_for_population,y)
    population_initial_cumT1D <- c(population_initial_cumT1D,1 - c(table_UKB[which.min(abs(table_UKB$sensitivity - y)),"centile"]))
  }
  total_visit_cumT1D <- c(n_visit * population_initial_cumT1D)
  total_visit_cumT1D_ratio_cases <- total_visit_cumT1D/(sensitivity_n*0.005)
  
  data_cohort <- data.frame(cohort,sensitivity_n*100,population_initial_cumT1D*100,sensitivity_cohort*100,(1-sensitivity_for_population)*100,(1-sensitivity_cohort)*100,total_visit_cumT1D,total_visit_cumT1D_ratio_cases,n_visit,pthreshold)
  
  p <- ggplot(data_cohort, aes(x = population_initial_cumT1D, y = total_visit_cumT1D, colour = cohort, group = sensitivity_cohort,group_2 = total_visit_cumT1D_ratio_cases,group_3 = n_visit, group_4 = pthreshold)) +
    geom_point()
  
  ggplotly(p)
  
  
  best <- data_cohort %>% mutate(cohort = fct_reorder(cohort,-n_visit)) %>% group_by(cohort) %>% filter(total_visit_cumT1D == min(total_visit_cumT1D)) %>% slice(1L)
  teddycost <- as.numeric(best[best$cohort == "Simple","total_visit_cumT1D"][1,1])
  best <- best %>% mutate(cost_gain = -((total_visit_cumT1D - teddycost)/teddycost*100))
  print(best)
  print(sensitivity_n)
  best <- best %>%  mutate(X.1...sensitivity_for_population....100 = 100-X.1...sensitivity_for_population....100) %>% 
                    mutate(sensitivity_cohort...100 = 100 - sensitivity_cohort...100)
  table_summary <- best %>% select(-matches("pthreshold")) %>% select(cohort,population_initial_cumT1D...100,X.1...sensitivity_for_population....100,sensitivity_cohort...100,sensitivity_n...100,total_visit_cumT1D,total_visit_cumT1D_ratio_cases,cost_gain)
  colnames(table_summary) <- c("Cohort design","Screened newborns in followed cohort","% of cases in followed cohort","% not detected in advance in followed cohort","Total % detected before onset","Follow-up test per child in the screened population","Follow-up test per T1D case detected","Efficiency gain vs simple approach")
  myft <- regulartable(
    table_summary)
  myft
  myft <- theme_vanilla(myft)
  myft
  
  
  doc <- read_docx()
  doc <- body_add_flextable(doc, value = myft)
  print(doc, target = paste0(pathfigures_tables,"tableS6_new.docx"))
