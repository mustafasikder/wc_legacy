library(gridExtra)

load("X:/Spatial Stat/WASH Cholera/new_data/revised.model.results.RData", .GlobalEnv)

main1<- ggplot(data= full.incidence.data, aes(Predicted, Observed))+ 
  geom_point(size= .5, alpha= .5, color= 'grey40')+
  theme(legend.position = "none", panel.grid = element_line(linetype = 3, size = .5), panel.grid.minor = element_blank(), plot.margin = unit(c(1, 1, 1, 1), 'pt'))+
  scale_x_continuous(limits = c(-5, 2), breaks = c(-4, -2, 0, 2),labels = c("0.0001", "0.01", "0.0", "100"), position = "bottom")+
  scale_y_continuous(limits = c(-5, 2), breaks = c(-4, -2, 0, 2),labels = c("0.0001", "0.01", "0.0", "100"), position = "left")+
  geom_abline(slope = 1, size= 1, alpha= .5, color= '#a44a3f')+labs(x= "Predicted mean annual incidence", y= "Observed mean annual incidence")+ 
  #annotate("text", x= -4, y= 0, label= "atop (italic(R) ^ 2 == ??0.32, RMSE == ??0.95, MAE== ??0.76)", parse = TRUE, size = 2)+ 
  coord_equal()#+ scale_color_tableau(palette = "Tableau 10")#scale_color_viridis(discrete=TRUE) 

mean_impurity_decrease <-
  ggplot(data = v_imp_inc,aes( mean_accuracy_decrease, reorder(var, mean_accuracy_decrease), fill = var)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#264653", "#2a9d8f", "#F16745", "#FFC65D", "#f4a261", "#4CC3D9", "#93648D", "#457b9d" )) +
  labs(y = NULL, x = "Conditional permutation \nimportance") + theme(
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

  

plot3<- main1+annotation_custom(ggplotGrob(mean_impurity_decrease), xmin = -1.8 , xmax = 4, ymin = -Inf, ymax = 0)


# main2<- ggplot(data=full.hotspot.data, aes(m= vote.1, d= Observed, color= cv.number))+ 
#   geom_roc(n.cuts=0, linealpha = .5) + 
#   coord_equal()+
#   theme(legend.position = 'none', panel.grid = element_line(linetype = 3, size = .5), panel.grid.minor = element_blank())+
#   scale_color_tableau()+ 
#   # scale_color_viridis_d()+
#   labs(x= "False positive rate", y= "True positive rate")

# with a point on the final model ROC
# main2<- ggplot(data=full.hotspot.data, aes(m= vote.1, d= Observed, color= cv.number))+ 
#   geom_roc(n.cuts=0, linealpha = .5) + 
#   coord_equal()+
#   theme(legend.position = 'none', panel.grid = element_line(linetype = 3, size = .5), panel.grid.minor = element_blank())+
#   scale_color_grey(end= 0)+
#   #scale_color_tableau(direction = -1)+ 
#   # scale_color_viridis_d()+
#   labs(x= "False positive rate", y= "True positive rate")+ 
#   geom_roc(data = full.hotspot.data[cv.number=="cv.9",], 
#            aes(m= vote.1, d= Observed), size= 1.2, 
#            cutoffs.at = c(.145), # this is the discrimination threshold 
#            cutoff.labels = "FPR= 0.40,     \nTPR= 0.85      ", # from sensetivity(.145) and 1- specificity(.145)
#            pointsize = 1, labelsize = 2)

# without the point
main2<- ggplot(data = combined_df_hp)+ 
  geom_roc(aes(m= vote.1, d= Observed, color= cv.number), n.cuts=0, linealpha = .3) + 
  geom_roc(aes(m= vote.1.f, d= Observed), n.cuts=0, linealpha = 1) + 
  coord_equal()+
  theme(legend.position = 'none', 
        panel.grid = element_line(linetype = 3, size = .5), 
        panel.grid.minor = element_blank(), plot.margin = unit(c(1, 1, 1, 1), 'pt'))+
  scale_color_grey(end= 0)+ 
  labs(x= "False positive rate", y= "True positive rate")




mean_impurity_decrease.hp <-
  ggplot(data = v_imp_hp, aes(mean_accuracy_decrease, reorder(var, mean_accuracy_decrease), fill = var)) +
  geom_bar(stat = "identity") +
  scale_fill_manual( values = c("#264653","#2a9d8f","#F16745","#FFC65D","#f4a261","#4CC3D9","#93648D","#457b9d")) + 
  labs(y = NULL, x = "Conditional permutation \nimportance") + theme(
    legend.title = element_blank(),
    legend.position = 'none',
    axis.text.y = element_text(size = 8), 
    axis.title.x = element_text(size= 7, face = "plain"),
    axis.text.x = element_text(size = 6),
    #plot.background = element_rect(color = "White"),
    panel.grid.major = element_blank(),
    panel.grid.minor =  element_blank(), 
    panel.grid.minor.x = element_blank())+ 
  scale_x_continuous(position = "top", limits = c(min(v_imp_hp$mean_accuracy_decrease), max(v_imp_hp$mean_accuracy_decrease)), breaks = c(as.vector(summary(v_imp_hp$mean_accuracy_decrease)[c(1,3,6)])))


plot4<- main2+annotation_custom(ggplotGrob(mean_impurity_decrease.hp), xmin = .49 , xmax = 1.35, ymin = 0, ymax = .75)

#plot3.v1<- ggpubr::ggarrange(plot3, plot4, ncol = 1, labels = c("A", "B"),  heights = c(1, 1))

plot3.v1<- gridExtra::grid.arrange(main1,
                                   mean_impurity_decrease, 
                                   main2, 
                                   mean_impurity_decrease.hp, 
                                   layout_matrix= rbind(
                                     c(1, 1, 1, 1, 1, 1, NA, NA, NA, NA), 
                                     c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2),
                                     c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2),
                                     c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2), 
                                     c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2), 
                                     c(1, 1, 1, 1, 1, 1, NA, NA, NA, NA), 
                                     c(3, 3, 3, 3, 3, 3, NA, NA, NA, NA), 
                                     c(3, 3, 3, 3, 3, 3, 4, 4, 4, 4), 
                                     c(3, 3, 3, 3, 3, 3, 4, 4, 4, 4),
                                     c(3, 3, 3, 3, 3, 3, 4, 4, 4, 4),
                                     c(3, 3, 3, 3, 3, 3, 4, 4, 4, 4), 
                                     c(3, 3, 3, 3, 3, 3, NA, NA, NA, NA)))

ggsave("X:/Spatial Stat/WASH Cholera/clean_repo/results/Figure_3.jpeg", plot= plot3.v1, width = 6.5, height = 8, units = "in", dpi= 500)

