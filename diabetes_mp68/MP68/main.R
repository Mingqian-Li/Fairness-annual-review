# main load

#############################################################
## ## ferratlauric@gmail.com - June 2019
#############################################################
#if using MAC you might have to pre install XQuartz https://www.xquartz.org/
##############################
# 0 - Load librairies
##############################

# machine learning 
library(caret) 
library(My.stepwise)

# handling data
library(dplyr)
library(tidyr)
library(dplyr)
library(tidyverse)

# ROC analyses
library(pROC)
library(plotROC)
library(timeROC) 

# plot tools
library(plotly)
library(ggplot2)
library(cowplot)
library(GGally)
library(ggpubr)

# export to word office
library(readxl)
library(officer) 
library(rvg)
library(flextable)

#Cox model
library(survival)
library(survey)

# utilitary
library(reshape2)
library(haven)
library(readr)
############################## 
# 1 - Source file 
##############################
# dataPath <- "C:/Users/Lauric/Desktop/Postdoc/2018-09-20 Release/"
# codePath <- "C:/Users/Lauric/Dropbox/MP68/"
# pathSaveModels <-  "C:/Users/Lauric/Desktop/Postdoc/MP68/models/"
# pathfigures_tables <- "C:/Users/Lauric/Desktop/Postdoc/MP68/figures_tables/"

dataPath <- "D:/phd/3nd/sparse group regression/diabetes/mp68_data/"
codePath <- "D:/phd/3nd/sparse group regression/diabetes/mp68_data/MP68/"
pathSaveModels <-  "D:/phd/3nd/sparse group regression/diabetes/mp68_data/mq/"
pathfigures_tables <- "D:/phd/3nd/sparse group regression/diabetes/mp68_data/mq/"

# create new directory to save models and figures.
dir.create(pathSaveModels,recursive = TRUE)
dir.create(pathfigures_tables,recursive = TRUE)
#load data
source(paste0(codePath,"main_load_data.R"))
#load useful functions
source(paste0(codePath,'t_AUC - GRS2/list_variable_f.R'))
source(paste0(codePath,'ggplotAUC.R'))

###################################


# compute models to be use to create the figures in the article
# load functiona to compute Cox model at different landmark
source(paste0(codePath,"t_AUC - GRS2/function_T_AUC_CV.R"))
source(paste0(codePath,"t_AUC - GRS2/function_T_AUC.R"))
# AB + GRS + FH # may take 2-3 hours to compute, only need to be done once
method <- "Cox"
complexity <- "abn_grs_fdr"
source(paste0(codePath,"t_AUC - GRS2/compute_T_AUC_CV.R"))
source(paste0(codePath,"t_AUC - GRS2/compute_Cox_model.R"))


### main article
#### figure 1 time ROC AUC at landmark 2 and 4 years
source(paste0(codePath,"fig_ROC_AUC.R"))

#### figure 2 time ROC AUC at multiples landmark and horizon time
source(paste0(codePath,"t_AUC - GRS2/plot_t_AUC_CI.R"))

#### figure 3 calibration and density plot
source(paste0(codePath,"calibration_and_density_plot.R"))

#### diagram figure 4 figure was done with the help of GIMP to draw the diagram. 
#Thus the function below is only partialy figure 3 as in the article
source(paste0(codePath,"diagram_figure.R"))


### supplementary

#### figureS1 
source(paste0(codePath,"compare_GRS.R"))

#### figureS2  HR change
source(paste0(codePath,"HR_trend.R"))
#### figureS3 ROC AUC with positive AB only
source(paste0(codePath,"figure_AB_positive_AUC.R"))

#### table S1 in the article was edited by adding references,
# here we only compute the individual predictive power and the pvalue of the variables
# it might thake few hours to run
source(paste0(codePath,"individual_predictive_power.R"))

#### tableS 2
#compute GRS + FH model, it may take 2-3 hours to compute, only need to be done once
method <- "Cox"
complexity <- "grs_fdr"
source(paste0(codePath,"t_AUC - GRS2/compute_T_AUC_CV.R"))
#compute GRS model, it may take 2-3 hours to compute, only need to be done once
method <- "Cox"
complexity <- "grs"
source(paste0(codePath,"t_AUC - GRS2/compute_T_AUC_CV.R"))

### run table S2
table_name <- "tableS2"
complexity1 <-  "grs"
complexity2 <-  "grs_fdr"
source(paste0(codePath,"tableSCox_CV.R"))
#### table S3
#compute Abn model, it may take 2-3 hours to compute, only need to be done once
method <- "Cox"
complexity <- "abn"
source(paste0(codePath,"t_AUC - GRS2/compute_T_AUC_CV.R"))


### run table S4
table_name <- "tableS3"
complexity1 <-  "abn"
complexity2 <-  "abn_grs_fdr"
source(paste0(codePath,"tableSCox_CV.R"))

#### table S4 probability predicted for different scenario
source(paste0(codePath,"predict_proba_for_different_scenarios.R"))

#### tableS 5 ABC
# already computed by running figure 2

#### table S 6 descriptive statistics of the dataset

source(paste0(codePath,"descriptive_statistics.R"))


################ stepwise regression at different landmarks
source(paste0(codePath,"t_AUC - GRS2/stepwise/stepwise.R"))

