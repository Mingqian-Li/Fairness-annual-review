##### templatenalyse results t_AUC
#############################################################
## ## ferratlauric@gmail.com - December 2018
#############################################################
source(paste0(codePath,"ggplotAUC.R"))

method <- "Cox"

if (complexity1 == "abn") {
  day_beginv <- c(365.25,365.25 + 183,365.25*2 + seq(0,5*365.25,365)) +45
} else {
  day_beginv <- c(60,365.25,365.25 + 183,365.25*2 + seq(0,5*365.25,365)) +45
}

day_endv <- c(365.25,365.25*3,5*365.25,8*365.25)


n_time_begin <- length(day_beginv)
n_time_end <- length(day_endv)

AUC_m <- matrix(, nrow = n_time_begin, ncol = n_time_end)
seAUC_m <- matrix(, nrow = n_time_begin, ncol = n_time_end)
nt1d_m <- matrix(, nrow = n_time_begin, ncol = n_time_end)
ntot_m <- matrix(, nrow = n_time_begin, ncol = n_time_end)
n_2_keep_m <- matrix(, nrow = n_time_begin, ncol = n_time_end)
k <- 3

AUC_m_k <- matrix(, nrow = k, ncol = n_time_end)
seAUC_m_k <- matrix(, nrow = k, ncol = n_time_end)
nt1d_m_k <- matrix(, nrow = k, ncol = n_time_end)
ntot_m_k <- matrix(, nrow = k, ncol = n_time_end)
n_2_keep_m_k <- matrix(, nrow = k, ncol = n_time_end)

n_nested <- 10
AUC_m_n_nested <- matrix(, nrow = n_nested, ncol = n_time_end)
seAUC_m_n_nested <- matrix(, nrow = n_nested, ncol = n_time_end)
nt1d_m_n_nested <- matrix(, nrow = n_nested, ncol = n_time_end)
ntot_m_n_nested <- matrix(, nrow = n_nested, ncol = n_time_end)
n_2_keep_m_n_nested <- matrix(, nrow = n_nested, ncol = n_time_end)
for (j in 1:n_time_begin) {
  for (l in 1:n_nested) {
    for (i in 1:k) {
      
      # names_tAUC <- paste0("t_AUC_",method,"_complexity_",complexity,"_day_begin_",day_beginv[j],"_day_end_",paste0(day_endv,collapse = "_"),"_kfold_",k,"_fold_",i)
      names_tAUC <- paste0("t_AUC_",method,"_complexity_",complexity1,"_day_begin_",day_beginv[j],"_day_end_",paste0(day_endv,collapse = "_"),"_kfold_",k,"_fold_",i,"_nested_",l)
      # load results
      load(file = paste0(pathSaveModels,names_tAUC,".RData"))
      AUC_m_k[i,] <- ROC.T$AUC
      seAUC_m_k[i,] <- ROC.T$inference$vect_sd_1
      nt1d_m_k[i,] <- nt1d
      ntot_m_k[i,] <- ntot
      n_2_keep_m_k[i,] <- n_people_2_keep
    }
    AUC_m_n_nested[l,] <- colMeans(AUC_m_k, na.rm = TRUE, dims = 1)
    seAUC_m_n_nested[l,] <- apply(AUC_m_k,2,var)
    nt1d_m_n_nested[l,] <- colSums(nt1d_m_k,na.rm = TRUE, dims = 1)
    ntot_m_n_nested[l,] <- colSums(ntot_m_k,na.rm = TRUE, dims = 1)
    n_2_keep_m_n_nested[l,] <- colMeans(n_2_keep_m_k,na.rm = TRUE, dims = 1)
  }
  AUC_m[j,] <- colMeans(AUC_m_n_nested, na.rm = TRUE, dims = 1)
  seAUC_m[j,] <- apply(seAUC_m_n_nested,2,function(x) sqrt(mean(x,na.rm = TRUE)))
  nt1d_m[j,] <- colMeans(nt1d_m_n_nested,na.rm = TRUE, dims = 1)
  ntot_m[j,] <- colMeans(ntot_m_n_nested,na.rm = TRUE, dims = 1)
  n_2_keep_m[j,] <- colMeans(n_2_keep_m_n_nested,na.rm = TRUE, dims = 1)
}

if (complexity1 == "abn") {  
  prediction_at <-  c("1 year","18 months","2 years","3 years","4 years","5 years","6 years","7 years")
  }else {
  prediction_at <-  c("3 months","1 year","18 months","2 years","3 years","4 years","5 years","6 years","7 years")
}

prediction_for <- c("1 year","3 years","5 years","8 years")

row.names(AUC_m) <- prediction_at 
colnames(AUC_m) <- prediction_for

confup <- create_IC_se(AUC_m,seAUC_m,confidencev = 1.96, class = "up")
conflow <- create_IC_se(AUC_m,seAUC_m,confidencev = 1.96, class = "low")


p1 <- ggplotAUC2(AUC_m,conflow, confup,prediction_at ,names_var = "horizon time")



#############################


AUC_m <- matrix(, nrow = n_time_begin, ncol = n_time_end)
seAUC_m <- matrix(, nrow = n_time_begin, ncol = n_time_end)
nt1d_m <- matrix(, nrow = n_time_begin, ncol = n_time_end)
ntot_m <- matrix(, nrow = n_time_begin, ncol = n_time_end)
n_2_keep_m <- matrix(, nrow = n_time_begin, ncol = n_time_end)
k <- 3

AUC_m_k <- matrix(, nrow = k, ncol = n_time_end)
seAUC_m_k <- matrix(, nrow = k, ncol = n_time_end)
nt1d_m_k <- matrix(, nrow = k, ncol = n_time_end)
ntot_m_k <- matrix(, nrow = k, ncol = n_time_end)
n_2_keep_m_k <- matrix(, nrow = k, ncol = n_time_end)

n_nested <- 10
AUC_m_n_nested <- matrix(, nrow = n_nested, ncol = n_time_end)
seAUC_m_n_nested <- matrix(, nrow = n_nested, ncol = n_time_end)
nt1d_m_n_nested <- matrix(, nrow = n_nested, ncol = n_time_end)
ntot_m_n_nested <- matrix(, nrow = n_nested, ncol = n_time_end)
n_2_keep_m_n_nested <- matrix(, nrow = n_nested, ncol = n_time_end)
for (j in 1:n_time_begin) {
  for (l in 1:n_nested) {
    for (i in 1:k) {
      
      # names_tAUC <- paste0("t_AUC_",method,"_complexity_",complexity,"_day_begin_",day_beginv[j],"_day_end_",paste0(day_endv,collapse = "_"),"_kfold_",k,"_fold_",i)
      names_tAUC <- paste0("t_AUC_",method,"_complexity_",complexity2,"_day_begin_",day_beginv[j],"_day_end_",paste0(day_endv,collapse = "_"),"_kfold_",k,"_fold_",i,"_nested_",l)
      # load results
      load(file = paste0(pathSaveModels,names_tAUC,".RData"))
      AUC_m_k[i,] <- ROC.T$AUC
      seAUC_m_k[i,] <- ROC.T$inference$vect_sd_1
      nt1d_m_k[i,] <- nt1d
      ntot_m_k[i,] <- ntot
      n_2_keep_m_k[i,] <- n_people_2_keep
    }
    AUC_m_n_nested[l,] <- colMeans(AUC_m_k, na.rm = TRUE, dims = 1)
    seAUC_m_n_nested[l,] <- apply(AUC_m_k,2,var)
    nt1d_m_n_nested[l,] <- colSums(nt1d_m_k,na.rm = TRUE, dims = 1)
    ntot_m_n_nested[l,] <- colSums(ntot_m_k,na.rm = TRUE, dims = 1)
    n_2_keep_m_n_nested[l,] <- colMeans(n_2_keep_m_k,na.rm = TRUE, dims = 1)
  }
  AUC_m[j,] <- colMeans(AUC_m_n_nested, na.rm = TRUE, dims = 1)
  seAUC_m[j,] <- apply(seAUC_m_n_nested,2,function(x) sqrt(mean(x,na.rm = TRUE)))
  nt1d_m[j,] <- colMeans(nt1d_m_n_nested,na.rm = TRUE, dims = 1)
  ntot_m[j,] <- colMeans(ntot_m_n_nested,na.rm = TRUE, dims = 1)
  n_2_keep_m[j,] <- colMeans(n_2_keep_m_n_nested,na.rm = TRUE, dims = 1)
}

prediction_for <- c("1 year","3 years","5 years","8 years")

row.names(AUC_m) <- prediction_at 
colnames(AUC_m) <- prediction_for

confup <- create_IC_se(AUC_m,seAUC_m,confidencev = 1.96, class = "up")
conflow <- create_IC_se(AUC_m,seAUC_m,confidencev = 1.96, class = "low")


p2 <- ggplotAUC2(AUC_m,conflow, confup,prediction_at ,names_var = "horizon time")

fig1 <- ggdraw() +
  draw_plot(p1, x = 0, y = 0, width = 0.5, height = 1) +
  draw_plot(p2, x = 0.5, y = 0, width = 0.5, height = 1) +
  draw_plot_label(label = c("a", "b"), size = 15,
                  x = c(0, 0.5), y = c(1,1))
fig1

ggsave(paste0(pathfigures_tables,table_name,".jpg"),width = 6.8*1.5,height = 6.8)

# ggplotAUC3(AUC_m,conflow, confup,ntot_m,nt1d_m,prediction_at ,names_var = "horizon time")
# ggsave(paste0("C:/Users/Lauric/Desktop/Postdoc/t_AUC_GRS2/figures/t_AUC_CV_all",method,"_",complexity,".jpg"),width = 9.6,height = 8.52)

# save(list = c("AUC_m","confup","conflow","seAUC_m"),file = paste0("C:/Users/Lauric/Desktop/Postdoc/t_AUC_GRS2/figures/t_AUC_",method,"_",complexity,"_CV",".Rdata"))


# row.names(n_2_keep_m) <- prediction_at 
# colnames(n_2_keep_m) <- prediction_for
# row.names(ntot_m) <- prediction_at 
# colnames(ntot_m) <- prediction_for
# ratio_m <- n_2_keep_m/ntot_m
# 
# ggplotAUC2(ratio_m,n_2_keep_m, ntot_m,prediction_at ,names_var = "horizon time")
# ggsave(paste0("C:/Users/Lauric/Desktop/Postdoc/t_AUC_GRS2/figures/people_in_survey_CV_all",method,"_",complexity,".jpg"),width = 9.6,height = 8.52)

