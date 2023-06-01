source(paste0(codePath,"figureS1A.R"))
source(paste0(codePath,"figureS1B.R"))

p2 <- ggdraw() +
  draw_plot(p, x = 0, y = 0, width = 0.5, height = 1) +
  draw_plot(p1, x = 0.5, y = 0, width = 0.5, height = 1) +
  draw_plot_label(label = c("a", "b"), size = 15,
                  x = c(0, 0.5), y = c(1, 1))
p2
ggsave(paste0(pathfigures_tables ,"figureS1.jpg"),width = 1.5*6.8,height = 1.5*3.4)
