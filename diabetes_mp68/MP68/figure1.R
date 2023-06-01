##### Figure 1
#############################################################
## ## ferratlauric@gmail.com - Novembre 2018
#############################################################
# library to load

source(paste0(codePath,"main_load_data.R"))
# extract model performance 
source(paste0(codePath,"t_AUC - GRS2/results_T_AUC_CV.R"))

# display performance  (time dependant ROC  AUC) for different landmark and horizon time
source(paste0(codePath,"t_AUC - GRS2/plot_t_AUC_CI.R"))
# display performance (ROC AUC for landmark 2 years old a and for different combination of variables)
source(paste0(codePath,"t_AUC - GRS2/article_figures/RoC_save_different_models_CV.R"))

fig1 <- ggdraw() +
  draw_plot(p, x = 0, y = 0, width = 0.5, height = 1) +
  draw_plot(p1, x = 0.5, y = 0, width = 0.5, height = 1) +
  draw_plot_label(label = c("a", "b"), size = 15,
                  x = c(0, 0.5), y = c(1,1))
fig1
ggsave(paste0(pathfigures_tables,"figure1.jpg"),width = 6.8*1.7, height = 6.8*1.7/2)


read_pptx() %>%
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with_vg(code = print(fig1), type = "body") %>%
  print(target = paste0(pathfigures_tables,"figure1.pptx"))

