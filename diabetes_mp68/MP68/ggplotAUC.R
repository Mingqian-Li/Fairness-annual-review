library(reshape2)
library(dplyr)
library(tidyr)
library(ggplot2)
ggplotAUC <- function(m){
  data_c <- melt(m)
  names(data_c) <-  c("Formula","cpep","AUC")
  data_c$AUC <- round(data_c$AUC,3)
  p <- ggplot(data = data_c,
              aes(x = cpep, y = Formula)) +
    geom_tile(aes(fill = AUC), colour = "white") +
    scale_fill_gradient(breaks = seq(0.7,1,0.05),low = "white", high = "green") +
    geom_text(aes(x = cpep, y = Formula, label = AUC)) +
    xlab("AUC prediction") + ylab("formula") +
    theme(legend.position = "none")
  return(p)
}

ggplotAUC2 <- function(m,m_min, m_max,levels_order,names_var ="cpep", colour_low = "white", colour_up = "#3399FF",name_ylab = "landmark",sizetext = 4){
  data_c <- melt(m)
  data_c_min <- melt(m_min)
  data_c_max <- melt(m_max)
  names(data_c) <-  c("Formula","cpep","AUC")
  names(data_c_min) <-  c("Formula","cpep","AUCmin")
  names(data_c_max) <-  c("Formula","cpep","AUCmax")
  data_c_min$AUCmin <- round(data_c_min$AUCmin,2)
  data_c_max$AUCmax <-  round(data_c_max$AUCmax,2)
  data_c$AUC <- round(data_c$AUC,2)
  data_c_I <- data_c %>% left_join(data_c_min) %>% left_join(data_c_max) %>%
    mutate(AUCmin = paste(" (",AUCmin,"-", sep = '')) %>%
    mutate(AUCmax = paste(AUCmax,")", sep = '')) %>%
    mutate(text = paste(formatC( round( AUC, 2 ), format='f', digits=2 ),"\n", AUCmin, AUCmax, sep = '')) %>% 
    mutate(text = if_else(is.na(AUC),"",text))
  data_c_I <-  data_c_I %>% mutate(Formula = factor(Formula,levels = levels_order))
  p <- ggplot(data = data_c_I,
              aes(x = cpep, y = Formula)) +
    geom_tile(aes(fill = AUC), colour = "white") +
    scale_fill_gradient(breaks = seq(0.7,1,0.03), low = colour_low, high = colour_up,limits = c(0.5,1), na.value = "white" ) +
    geom_text(aes(x = cpep, y = Formula, label = text),size = sizetext) +
    xlab(names_var) + ylab(name_ylab) +
    theme(legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line = element_blank())
  return(p)
}
ggplotAUC2_pvalue <- function(m,m_min, m_max,m_pvalue,levels_order,xlab_name = "", ylab_name ="" , colour_low = "white", colour_up = "#3399FF"){
  data_c <- melt(m)
  data_c_min <- melt(m_min)
  data_c_max <- melt(m_max)
  data_c_pvalue <- melt(m_pvalue)
  names(data_c) <-  c("Formula","cpep","AUC")
  names(data_c_min) <-  c("Formula","cpep","AUCmin")
  names(data_c_max) <-  c("Formula","cpep","AUCmax")
  names(data_c_pvalue) <- c("Formula","cpep","pvalue")
  data_c_min$AUCmin <- round(data_c_min$AUCmin,2)
  data_c_max$AUCmax <-  round(data_c_max$AUCmax,2)
  data_c$AUC <- round(data_c$AUC,2)
  data_c_pvalue$pvalue <- round(data_c_pvalue$pvalue,3)
  data_c_I <- data_c %>% left_join(data_c_min) %>% left_join(data_c_max) %>% left_join(data_c_pvalue) %>% 
    mutate(AUCmin = paste(" (",AUCmin,"-", sep = '')) %>%
    mutate(AUCmax = paste(AUCmax,") ", sep = '')) %>%
    mutate(text = paste(AUC, AUCmin, AUCmax,pvalue, sep = '')) %>% 
    mutate(text = if_else(is.na(AUC),"",text))
  data_c_I <-  data_c_I %>% mutate(Formula = factor(Formula,levels = levels_order))
  p <- ggplot(data = data_c_I,
              aes(x = cpep, y = Formula)) +
    geom_tile(aes(fill = AUC), colour = "white") +
    scale_fill_gradient(breaks = seq(0.7,1,0.03), low = colour_low, high = colour_up ) +
    geom_text(aes(x = cpep, y = Formula, label = text)) +
    xlab(xlab_name) + ylab(ylab_name) +
    theme(legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line = element_blank())
  return(p)
}
ggplotAUC3 <- function(m,m_min, m_max,ntot,nt1d,levels_order,names_var ="cpep", colour_low = "white", colour_up = "#0073E6"){
  data_c <- melt(m)
  data_c_min <- melt(m_min)
  data_c_max <- melt(m_max)
  data_c_ntot <- melt(ntot)
  data_c_nt1d <- melt(nt1d)
  names(data_c) <-  c("Formula","cpep","AUC")
  names(data_c_min) <-  c("Formula","cpep","AUCmin")
  names(data_c_max) <-  c("Formula","cpep","AUCmax")
  names(data_c_ntot) <-  c("Formula","cpep","nt1d")
  names(data_c_nt1d) <-  c("Formula","cpep","ntot")
  data_c_min$AUCmin <- round(data_c_min$AUCmin,2)
  data_c_max$AUCmax <-  round(data_c_max$AUCmax,2)
  data_c$AUC <- round(data_c$AUC,2)
  data_c_I <- data_c %>%
    left_join(data_c_min) %>%
    left_join(data_c_max) %>%
    left_join(data_c_min) %>%
    left_join(data_c_max) %>% 
    mutate(AUCmin = paste(" (",AUCmin,"-", sep = '')) %>%
    mutate(AUCmax = paste(AUCmax,")", sep = '')) %>%
    mutate(n = paste("\n",nt1d," / ",ntot)) %>% 
    mutate(text = paste(AUC, AUCmin, AUCmax,n, sep = '')) %>% 
    mutate(text = if_else(is.na(AUC),"",text))
  data_c_I <-  data_c_I %>% mutate(Formula = factor(Formula,levels = levels_order))
  p <- ggplot(data = data_c_I,
              aes(x = cpep, y = Formula)) +
    geom_tile(aes(fill = AUC), colour = "white") +
      scale_fill_gradient(breaks = seq(0.67,1,0.03), low = colour_low, high = colour_up ) +
    #scale_fill_gradient2(low = "blue", high = "red", mid = "white", space = "Lab",limits = c(0.5,1)) +
     geom_text(aes(x = cpep, y = Formula, label = text)) +
    xlab(names_var) + ylab("landmark") +
    theme(legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line = element_blank())
  return(p)
}
ggplot_heatmap <- function(m,levels_order,names_var ="cpep", name_ylab = "landmark"){
  data_c <- melt(m)
  names(data_c) <-  c("Formula","cpep","AUC")
  data_c$AUC <- round(data_c$AUC,2)
  data_c_I <- data_c
  data_c_I <-  data_c_I %>% mutate(Formula = factor(Formula,levels = levels_order),
                                  text = AUC)
  p <- ggplot(data = data_c_I,
              aes(x = cpep, y = Formula)) +
    geom_tile(aes(fill = AUC), colour = "white") +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, space = "Lab")  +
    geom_text(aes(x = cpep, y = Formula, label = text)) +
    xlab(names_var) + ylab(name_ylab) +
    theme(legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line = element_blank())
          # panel.background = element_blank())
  return(p)
}

create_IC <- function(table,confidencev=1.96){
  n <- dim(table)[1]
  CI_l <- rep(0,n)
  CI_u <- rep(0,n)
  for (i in 1:n) {
    if (i %% 2 == 1) {
      total <- table[i,"freq"] + table[i + 1,"freq"]
      p <- table[i,"freq"]/total
      p <- 1 - p
      CI_l[i] <- max(p - confidencev*sqrt(p*(1 - p)/total),0)
      CI_u[i] <- min(p + confidencev*sqrt(p*(1 - p)/total),1)
    }
    else{
      CI_l[i] <- NA
      CI_u[i] <- NA
    }
  }
  IC <- data.frame(CI_l,CI_u)
  return(IC)
}
create_IC_se <- function(table,table_se,confidencev=1.96, class){
  n1 <- dim(table)[1]
  n2 <- dim(table)[2]
  output <- matrix(, nrow = n1, ncol = n2)
  for (i in 1:n1) {
    for (j in 1:n2) {
      if (class == "up") {
        output[i,j] <- min(table[i,j] + confidencev * table_se[i,j],1)
      }
      else{
        output[i,j] <- max(table[i,j] - confidencev * table_se[i,j],0)
      }
    }
  }
  rownames(output) <-  rownames(table)
  colnames(output) <-  colnames(table)
    return(output)
}
