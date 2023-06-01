
t1dgc <- read.delim(paste0(dataPath,"t1dgc.txt"))
TEDDY <- finaldata[,c("t1d","GRS2")]
t1dgc <- t1dgc[,c("t1d_status","grs2_final")]
names(t1dgc) <- c("t1d","GRS2")
TEDDY$database <- "TEDDY"
t1dgc$database <-"T1DGC"

data <- rbind(TEDDY,t1dgc) %>% filter(!is.na(t1d)) %>% mutate(t1d = factor(t1d))

ggplot(data, aes(y = GRS2,x = database, fill = t1d)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
  scale_fill_discrete(name = "", labels = c("T1D free","T1D"))

ggsave(paste0(pathfigures_tables,"figureS3.jpg"),width = 6.8,height = 6.8)


