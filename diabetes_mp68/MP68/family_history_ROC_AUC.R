#compute GRS model, it may take 2-3 hours to compute, only need to be done once
method <- "Cox"
complexity <- "fdr"
source(paste0(codePath,"t_AUC - GRS2/compute_T_AUC_CV.R"))


method <- "Cox"
complexity <- "abn_fdr"
source(paste0(codePath,"t_AUC - GRS2/compute_T_AUC_CV.R"))

### run table S3
table_name <- "table_FH"
complexity1 <-  "abn_grs_fdr"
complexity2 <-  "abn_fdr"
source(paste0(codePath,"tableSCox_CV.R"))

complexity <- "abn_fdr"
source(paste0(codePath,"t_AUC - GRS2/compute_Cox_model.R"))
