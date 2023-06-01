Confidence_Interval_for_AUC <- function(AUC,n1,n2,q = 0.95){
  #Hanley and McNeil (1982)
  z = qnorm(q)
  Q1 <- AUC/(2 - AUC)
  Q2 <- 2*AUC^(2)/(1 + AUC)
  seAUC <- sqrt((AUC * (1 - AUC) + (n1 - 1) * (Q1 - AUC^2) + (n2 - 1) * (Q2 - AUC^2))/(n1 * n2))
  Confidence_interval <- c(AUC - z*seAUC,min(AUC + z*seAUC,1))
  return(Confidence_interval)
}