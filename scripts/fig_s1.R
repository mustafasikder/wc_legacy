library(ggplot2)

# GBM incidence variable importance
v_imp_inc_gbm<- readRDS("X:/Spatial Stat/WASH Cholera/clean_repo/results/v_imp_inc_gbm.RDS")
#random forest hotspot variable importance
v_imp_hp<- readRDS("X:/Spatial Stat/WASH Cholera/clean_repo/results/v_imp_hp.RDS")

# GBM incidence plot 
mean_impurity_decrease_gbm <-
  ggplot(data = v_imp_inc_gbm,aes( Importance, reorder(var, Importance), fill = var)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#264653", "#2a9d8f", "#F16745", "#FFC65D", "#f4a261", "#4CC3D9", "#93648D", "#457b9d" )) +
  labs(y = NULL, x = "Importance (incidence)") + theme(
    legend.title = element_blank(),
    legend.position = 'none',
    axis.text.y = element_text(size = 8), 
    axis.title.x = element_text(size= 7, face = "plain"),
    axis.text.x = element_text(size = 6),
    #plot.background = element_rect(color = "White"),
    panel.grid.major = element_blank(),
    panel.grid.minor =  element_blank(), 
    panel.grid.minor.x = element_blank())+
  scale_x_continuous(position = "top")#, limits = c(min(v_imp_inc$mean_accuracy_decrease), max(v_imp_inc$mean_accuracy_decrease)), breaks = c(as.vector(summary(v_imp_inc$mean_accuracy_decrease)[c(1,3,6)])))

# random forest hotspot plot
mean_impurity_decrease.hp <-
  ggplot(data = v_imp_hp, aes(mean_accuracy_decrease, reorder(var, mean_accuracy_decrease), fill = var)) +
  geom_bar(stat = "identity") +
  scale_fill_manual( values = c("#264653","#2a9d8f","#F16745","#FFC65D","#f4a261","#4CC3D9","#93648D","#457b9d")) + 
  labs(y = NULL, x = "Conditional permutation \nimportance (hotspot)") + theme(
    legend.title = element_blank(),
    legend.position = 'none',
    axis.text.y = element_text(size = 8), 
    axis.title.x = element_text(size= 7, face = "plain"),
    axis.text.x = element_text(size = 6),
    #plot.background = element_rect(color = "White"),
    panel.grid.major = element_blank(),
    panel.grid.minor =  element_blank(), 
    panel.grid.minor.x = element_blank())+ 
  scale_x_continuous(position = "top", limits = c(0, max(v_imp_hp$mean_accuracy_decrease)), breaks = c(as.vector(summary(v_imp_hp$mean_accuracy_decrease)[c(1,3,6)])))

## Figure S1
fig_s1_rm_gbm<- gridExtra::grid.arrange(mean_impurity_decrease_gbm,
                                        mean_impurity_decrease.hp, 
                                 ncol= 2)

ggsave("X:/Spatial Stat/WASH Cholera/clean_repo/results/Figure_S1_rf_gbm.jpeg", plot= fig_s1_rm_gbm, width = 6.0, height = 2.5, units = "in", dpi= 500)

