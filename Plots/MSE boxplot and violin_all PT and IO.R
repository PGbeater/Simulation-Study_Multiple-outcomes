library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggpubr)
source("D:/Learning epi-master/Research Project/Code/Code and Result 2025/Code/geom_flat_violin.R")
# Specific
v_data <- c("D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO5 PT5IO0.5_S.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO5 PT5IO5_S.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO5 PT5IO10_S.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO5 PT20IO0.5_S.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO5 PT20IO5_S.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO5 PT20IO10_S.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO5 PT50IO0.5_S.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO5 PT50IO5_S.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO5 PT50IO10_S.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO1.5 PT5IO0.5_S.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO1.5 PT5IO5_S.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO1.5 PT5IO10_S.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO1.5 PT20IO0.5_S.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO1.5 PT20IO5_S.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO1.5 PT20IO10_S.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO1.5 PT50IO0.5_S.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO1.5 PT50IO5_S.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO1.5 PT50IO10_S.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO5 PT5IO0.5_S.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO5 PT5IO5_S.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO5 PT5IO10_S.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO5 PT20IO0.5_S.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO5 PT20IO5_S.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO5 PT20IO10_S.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO5 PT50IO0.5_S.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO5 PT50IO5_S.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO5 PT50IO10_S.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO1.5 PT5IO0.5_S.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO1.5 PT5IO5_S.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO1.5 PT5IO10_S.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO1.5 PT20IO0.5_S.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO1.5 PT20IO5_S.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO1.5 PT20IO10_S.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO1.5 PT50IO0.5_S.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO1.5 PT50IO5_S.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO1.5 PT50IO10_S.RData")
plot_list <- vector("list",4)
for (i_bias in 1:4) {
  load(v_data[9*i_bias-8])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT5IO0.5"),60)
  df_25vs10vsgen1 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_bias-7])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT5IO5"),60)
  df_25vs10vsgen2 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_bias-6])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT5IO10"),60)
  df_25vs10vsgen3 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_bias-5])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT20IO0.5"),60)
  df_25vs10vsgen4 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_bias-4])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT20IO5"),60)
  df_25vs10vsgen5 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_bias-3])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT20IO10"),60)
  df_25vs10vsgen6 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_bias-2])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT50IO0.5"),60)
  df_25vs10vsgen7 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_bias-1])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT50IO5"),60)
  df_25vs10vsgen8 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_bias])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT50IO10"),60)
  df_25vs10vsgen9 <- data.frame(model,value,PTIO)
  
  df_25vs10vsgen <- as.data.frame(rbind(df_25vs10vsgen1,df_25vs10vsgen2,df_25vs10vsgen3,
                                        df_25vs10vsgen4,df_25vs10vsgen5,df_25vs10vsgen6,
                                        df_25vs10vsgen7,df_25vs10vsgen8,df_25vs10vsgen9))
  df_25vs10vsgen$model <- factor(df_25vs10vsgen$model,levels = c("perc25","perc10","gen"))
  df_25vs10vsgen$PTIO <- factor(df_25vs10vsgen$PTIO,levels = c("PT5IO0.5","PT5IO5","PT5IO10",
                                                               "PT20IO0.5","PT20IO5","PT20IO10",
                                                               "PT50IO0.5","PT50IO5","PT50IO10"))
  df_25vsgen <- df_25vs10vsgen %>%
    filter(model %in% c("perc25", "gen"))
  df_10vsgen <- df_25vs10vsgen %>%
    filter(model %in% c("perc10", "gen"))
  
  df_25vsgen_IO5IO10 <- df_25vsgen %>%
    filter(PTIO %in% c("PT5IO5","PT5IO10",
                       "PT20IO5","PT20IO10",
                       "PT50IO5","PT50IO10"))
  df_10vsgen_IO5IO10 <- df_10vsgen %>%
    filter(PTIO %in% c("PT5IO5","PT5IO10",
                       "PT20IO5","PT20IO10",
                       "PT50IO5","PT50IO10"))
  
  # box + viol
  p1 <- ggplot(df_25vsgen, aes(x = PTIO, y = value, fill = model)) +
    geom_flat_violin(width = 2,position = position_dodge(width = 1)) +
    geom_boxplot(width = 0.4,position = position_dodge(width = 1), alpha = 0.1,outlier.size = 2,outlier.alpha = 0.5) +
    scale_x_discrete(expand = expansion(mult = c(0.1, 0.1))) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(x = "Scenarios", y = "MSE", title = "The 25% model vs Generic model in MSE in Different Scenarios") +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = "black"), 
      plot.background = element_rect(fill = "white"),  
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(color = "black"), 
      axis.ticks.length = unit(0.2, "cm"))
  
  p2 <- ggplot(df_10vsgen, aes(x = PTIO, y = value, fill = model)) +
    geom_flat_violin(width = 2,position = position_dodge(width = 1)) +
    geom_boxplot(width = 0.4,position = position_dodge(width = 1), alpha = 0.1,outlier.size = 2,outlier.alpha = 0.5) +
    scale_x_discrete(expand = expansion(mult = c(0.1, 0.1))) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(x = "Scenarios", y = "MSE", title = "The 10% model vs Generic model in MSE in Different Scenarios") +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = "black"), 
      plot.background = element_rect(fill = "white"),  
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(color = "black"),
      axis.ticks.length = unit(0.2, "cm"))
  
  p3 <- ggplot(df_25vsgen_IO5IO10, aes(x = PTIO, y = value, fill = model)) +
    geom_flat_violin(width = 2,position = position_dodge(width = 1)) +
    geom_boxplot(width = 0.4,position = position_dodge(width = 1), alpha = 0.1,outlier.size = 2,outlier.alpha = 0.5) +
    scale_x_discrete(expand = expansion(mult = c(0.1, 0.1))) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(x = "Scenarios", y = "MSE", title = "The 25% model vs Generic model in MSE in Different Scenarios") +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = "black"), 
      plot.background = element_rect(fill = "white"),  
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(color = "black"), 
      axis.ticks.length = unit(0.2, "cm"))
  
  p4 <- ggplot(df_10vsgen_IO5IO10, aes(x = PTIO, y = value, fill = model)) +
    geom_flat_violin(width = 2,position = position_dodge(width = 1)) +
    geom_boxplot(width = 0.4,position = position_dodge(width = 1), alpha = 0.1,outlier.size = 2,outlier.alpha = 0.5) +
    scale_x_discrete(expand = expansion(mult = c(0.1, 0.1))) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(x = "Scenarios", y = "MSE", title = "The 10% model vs Generic model in MSE in Different Scenarios") +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = "black"), 
      plot.background = element_rect(fill = "white"),  
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(color = "black"),
      axis.ticks.length = unit(0.2, "cm"))
  plot_list[[i_bias]][[1]] <- p1
  plot_list[[i_bias]][[2]] <- p2
  plot_list[[i_bias]][[3]] <- p3
  plot_list[[i_bias]][[4]] <- p4
}


origin_plot_25vsgen <- plot_list[[1]][[1]]
origin_plot_10vsgen <- plot_list[[1]][[2]]
origin_plot_25vsgen_IO5IO10 <- plot_list[[1]][[3]]
origin_plot_10vsgen_IO5IO10 <- plot_list[[1]][[4]]

combine_plot_25vsgen <- ggarrange(plot_list[[3]][[1]],plot_list[[2]][[1]],plot_list[[4]][[1]],
                                  ncol = 1, nrow = 3, 
                                  labels = c("seed1217 TO5","seed0620 TO1.5","seed1217 TO1.5"),
                                  label.x = 0.78,
                                  label.y = 0.995,
                                  align = "v")
combine_plot_10vsgen <- ggarrange(plot_list[[3]][[2]],plot_list[[2]][[2]],plot_list[[4]][[2]],
                                  ncol = 1, nrow = 3, 
                                  labels = c("seed1217 TO5","seed0620 TO1.5","seed1217 TO1.5"),
                                  label.x = 0.78,
                                  label.y = 0.995,
                                  align = "v")
combine_plot_25vsgen_IO5IO10 <- ggarrange(plot_list[[3]][[3]],plot_list[[2]][[3]],plot_list[[4]][[3]],
                                          ncol = 1, nrow = 3, 
                                          labels = c("seed1217 TO5","seed0620 TO1.5","seed1217 TO1.5"),
                                          label.x = 0.78,
                                          label.y = 0.995,
                                          align = "v")
combine_plot_10vsgen_IO5IO10 <- ggarrange(plot_list[[3]][[4]],plot_list[[2]][[4]],plot_list[[4]][[4]],
                                          ncol = 1, nrow = 3, 
                                          labels = c("seed1217 TO5","seed0620 TO1.5","seed1217 TO1.5"),
                                          label.x = 0.78,
                                          label.y = 0.995,
                                          align = "v")

ggsave(filename = "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Plot box with viol_new/without unmeasured/MSE/Origin 25vsgen_S.png",plot = origin_plot_25vsgen,width = 16,height = 6,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Plot box with viol_new/without unmeasured/MSE/Origin 10vsgen_S.png",plot = origin_plot_10vsgen,width = 16,height = 6,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Plot box with viol_new/without unmeasured/MSE/Sensitivity 25vsgen_S.png",plot = combine_plot_25vsgen,width = 16,height = 18,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Plot box with viol_new/without unmeasured/MSE/Sensitivity 10vsgen_S.png",plot = combine_plot_10vsgen,width = 16,height = 18,dpi = 330)

ggsave(filename = "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Plot box with viol_new/without unmeasured/MSE/Origin_IO5IO10 25vsgen_S.png",plot = origin_plot_25vsgen_IO5IO10,width = 16,height = 6,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Plot box with viol_new/without unmeasured/MSE/Origin_IO5IO10 10vsgen_S.png",plot = origin_plot_10vsgen_IO5IO10,width = 16,height = 6,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Plot box with viol_new/without unmeasured/MSE/Sensitivity_IO5IO10 25vsgen_S.png",plot = combine_plot_25vsgen_IO5IO10,width = 16,height = 18,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Plot box with viol_new/without unmeasured/MSE/Sensitivity_IO5IO10 10vsgen_S.png",plot = combine_plot_10vsgen_IO5IO10,width = 16,height = 18,dpi = 330)

# System cluster
v_data <- c("D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO5 PT5IO0.5_SC.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO5 PT5IO5_SC.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO5 PT5IO10_SC.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO5 PT20IO0.5_SC.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO5 PT20IO5_SC.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO5 PT20IO10_SC.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO5 PT50IO0.5_SC.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO5 PT50IO5_SC.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO5 PT50IO10_SC.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO1.5 PT5IO0.5_SC.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO1.5 PT5IO5_SC.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO1.5 PT5IO10_SC.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO1.5 PT20IO0.5_SC.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO1.5 PT20IO5_SC.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO1.5 PT20IO10_SC.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO1.5 PT50IO0.5_SC.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO1.5 PT50IO5_SC.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO1.5 PT50IO10_SC.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO5 PT5IO0.5_SC.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO5 PT5IO5_SC.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO5 PT5IO10_SC.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO5 PT20IO0.5_SC.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO5 PT20IO5_SC.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO5 PT20IO10_SC.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO5 PT50IO0.5_SC.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO5 PT50IO5_SC.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO5 PT50IO10_SC.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO1.5 PT5IO0.5_SC.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO1.5 PT5IO5_SC.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO1.5 PT5IO10_SC.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO1.5 PT20IO0.5_SC.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO1.5 PT20IO5_SC.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO1.5 PT20IO10_SC.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO1.5 PT50IO0.5_SC.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO1.5 PT50IO5_SC.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO1.5 PT50IO10_SC.RData")
plot_list <- vector("list",4)
for (i_bias in 1:4) {
  load(v_data[9*i_bias-8])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT5IO0.5"),60)
  df_25vs10vsgen1 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_bias-7])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT5IO5"),60)
  df_25vs10vsgen2 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_bias-6])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT5IO10"),60)
  df_25vs10vsgen3 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_bias-5])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT20IO0.5"),60)
  df_25vs10vsgen4 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_bias-4])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT20IO5"),60)
  df_25vs10vsgen5 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_bias-3])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT20IO10"),60)
  df_25vs10vsgen6 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_bias-2])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT50IO0.5"),60)
  df_25vs10vsgen7 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_bias-1])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT50IO5"),60)
  df_25vs10vsgen8 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_bias])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT50IO10"),60)
  df_25vs10vsgen9 <- data.frame(model,value,PTIO)
  
  df_25vs10vsgen <- as.data.frame(rbind(df_25vs10vsgen1,df_25vs10vsgen2,df_25vs10vsgen3,
                                        df_25vs10vsgen4,df_25vs10vsgen5,df_25vs10vsgen6,
                                        df_25vs10vsgen7,df_25vs10vsgen8,df_25vs10vsgen9))
  df_25vs10vsgen$model <- factor(df_25vs10vsgen$model,levels = c("perc25","perc10","gen"))
  df_25vs10vsgen$PTIO <- factor(df_25vs10vsgen$PTIO,levels = c("PT5IO0.5","PT5IO5","PT5IO10",
                                                               "PT20IO0.5","PT20IO5","PT20IO10",
                                                               "PT50IO0.5","PT50IO5","PT50IO10"))
  df_25vsgen <- df_25vs10vsgen %>%
    filter(model %in% c("perc25", "gen"))
  df_10vsgen <- df_25vs10vsgen %>%
    filter(model %in% c("perc10", "gen"))
  
  df_25vsgen_IO5IO10 <- df_25vsgen %>%
    filter(PTIO %in% c("PT5IO5","PT5IO10",
                       "PT20IO5","PT20IO10",
                       "PT50IO5","PT50IO10"))
  df_10vsgen_IO5IO10 <- df_10vsgen %>%
    filter(PTIO %in% c("PT5IO5","PT5IO10",
                       "PT20IO5","PT20IO10",
                       "PT50IO5","PT50IO10"))
  
  # box + viol
  p1 <- ggplot(df_25vsgen, aes(x = PTIO, y = value, fill = model)) +
    geom_flat_violin(width = 2,position = position_dodge(width = 1)) +
    geom_boxplot(width = 0.4,position = position_dodge(width = 1), alpha = 0.1,outlier.size = 2,outlier.alpha = 0.5) +
    scale_x_discrete(expand = expansion(mult = c(0.1, 0.1))) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(x = "Scenarios", y = "MSE", title = "The 25% model vs Generic model in MSE in Different Scenarios") +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = "black"), 
      plot.background = element_rect(fill = "white"),  
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(color = "black"), 
      axis.ticks.length = unit(0.2, "cm"))
  
  p2 <- ggplot(df_10vsgen, aes(x = PTIO, y = value, fill = model)) +
    geom_flat_violin(width = 2,position = position_dodge(width = 1)) +
    geom_boxplot(width = 0.4,position = position_dodge(width = 1), alpha = 0.1,outlier.size = 2,outlier.alpha = 0.5) +
    scale_x_discrete(expand = expansion(mult = c(0.1, 0.1))) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(x = "Scenarios", y = "MSE", title = "The 10% model vs Generic model in MSE in Different Scenarios") +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = "black"), 
      plot.background = element_rect(fill = "white"),  
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(color = "black"),
      axis.ticks.length = unit(0.2, "cm"))
  
  p3 <- ggplot(df_25vsgen_IO5IO10, aes(x = PTIO, y = value, fill = model)) +
    geom_flat_violin(width = 2,position = position_dodge(width = 1)) +
    geom_boxplot(width = 0.4,position = position_dodge(width = 1), alpha = 0.1,outlier.size = 2,outlier.alpha = 0.5) +
    scale_x_discrete(expand = expansion(mult = c(0.1, 0.1))) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(x = "Scenarios", y = "MSE", title = "The 25% model vs Generic model in MSE in Different Scenarios") +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = "black"), 
      plot.background = element_rect(fill = "white"),  
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(color = "black"), 
      axis.ticks.length = unit(0.2, "cm"))
  
  p4 <- ggplot(df_10vsgen_IO5IO10, aes(x = PTIO, y = value, fill = model)) +
    geom_flat_violin(width = 2,position = position_dodge(width = 1)) +
    geom_boxplot(width = 0.4,position = position_dodge(width = 1), alpha = 0.1,outlier.size = 2,outlier.alpha = 0.5) +
    scale_x_discrete(expand = expansion(mult = c(0.1, 0.1))) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(x = "Scenarios", y = "MSE", title = "The 10% model vs Generic model in MSE in Different Scenarios") +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = "black"), 
      plot.background = element_rect(fill = "white"),  
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(color = "black"),
      axis.ticks.length = unit(0.2, "cm"))
  plot_list[[i_bias]][[1]] <- p1
  plot_list[[i_bias]][[2]] <- p2
  plot_list[[i_bias]][[3]] <- p3
  plot_list[[i_bias]][[4]] <- p4
}


origin_plot_25vsgen <- plot_list[[1]][[1]]
origin_plot_10vsgen <- plot_list[[1]][[2]]
origin_plot_25vsgen_IO5IO10 <- plot_list[[1]][[3]]
origin_plot_10vsgen_IO5IO10 <- plot_list[[1]][[4]]

combine_plot_25vsgen <- ggarrange(plot_list[[3]][[1]],plot_list[[2]][[1]],plot_list[[4]][[1]],
                                  ncol = 1, nrow = 3, 
                                  labels = c("seed1217 TO5","seed0620 TO1.5","seed1217 TO1.5"),
                                  label.x = 0.78,
                                  label.y = 0.995,
                                  align = "v")
combine_plot_10vsgen <- ggarrange(plot_list[[3]][[2]],plot_list[[2]][[2]],plot_list[[4]][[2]],
                                  ncol = 1, nrow = 3, 
                                  labels = c("seed1217 TO5","seed0620 TO1.5","seed1217 TO1.5"),
                                  label.x = 0.78,
                                  label.y = 0.995,
                                  align = "v")
combine_plot_25vsgen_IO5IO10 <- ggarrange(plot_list[[3]][[3]],plot_list[[2]][[3]],plot_list[[4]][[3]],
                                          ncol = 1, nrow = 3, 
                                          labels = c("seed1217 TO5","seed0620 TO1.5","seed1217 TO1.5"),
                                          label.x = 0.78,
                                          label.y = 0.995,
                                          align = "v")
combine_plot_10vsgen_IO5IO10 <- ggarrange(plot_list[[3]][[4]],plot_list[[2]][[4]],plot_list[[4]][[4]],
                                          ncol = 1, nrow = 3, 
                                          labels = c("seed1217 TO5","seed0620 TO1.5","seed1217 TO1.5"),
                                          label.x = 0.78,
                                          label.y = 0.995,
                                          align = "v")

ggsave(filename = "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Plot box with viol_new/without unmeasured/MSE/Origin 25vsgen_SC.png",plot = origin_plot_25vsgen,width = 16,height = 6,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Plot box with viol_new/without unmeasured/MSE/Origin 10vsgen_SC.png",plot = origin_plot_10vsgen,width = 16,height = 6,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Plot box with viol_new/without unmeasured/MSE/Sensitivity 25vsgen_SC.png",plot = combine_plot_25vsgen,width = 16,height = 18,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Plot box with viol_new/without unmeasured/MSE/Sensitivity 10vsgen_SC.png",plot = combine_plot_10vsgen,width = 16,height = 18,dpi = 330)

ggsave(filename = "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Plot box with viol_new/without unmeasured/MSE/Origin_IO5IO10 25vsgen_SC.png",plot = origin_plot_25vsgen_IO5IO10,width = 16,height = 6,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Plot box with viol_new/without unmeasured/MSE/Origin_IO5IO10 10vsgen_SC.png",plot = origin_plot_10vsgen_IO5IO10,width = 16,height = 6,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Plot box with viol_new/without unmeasured/MSE/Sensitivity_IO5IO10 25vsgen_SC.png",plot = combine_plot_25vsgen_IO5IO10,width = 16,height = 18,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Plot box with viol_new/without unmeasured/MSE/Sensitivity_IO5IO10 10vsgen_SC.png",plot = combine_plot_10vsgen_IO5IO10,width = 16,height = 18,dpi = 330)


# Center
v_data <- c("D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO5 PT5IO0.5_C.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO5 PT5IO5_C.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO5 PT5IO10_C.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO5 PT20IO0.5_C.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO5 PT20IO5_C.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO5 PT20IO10_C.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO5 PT50IO0.5_C.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO5 PT50IO5_C.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO5 PT50IO10_C.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO1.5 PT5IO0.5_C.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO1.5 PT5IO5_C.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO1.5 PT5IO10_C.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO1.5 PT20IO0.5_C.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO1.5 PT20IO5_C.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO1.5 PT20IO10_C.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO1.5 PT50IO0.5_C.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO1.5 PT50IO5_C.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO1.5 PT50IO10_C.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO5 PT5IO0.5_C.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO5 PT5IO5_C.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO5 PT5IO10_C.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO5 PT20IO0.5_C.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO5 PT20IO5_C.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO5 PT20IO10_C.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO5 PT50IO0.5_C.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO5 PT50IO5_C.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO5 PT50IO10_C.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO1.5 PT5IO0.5_C.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO1.5 PT5IO5_C.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO1.5 PT5IO10_C.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO1.5 PT20IO0.5_C.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO1.5 PT20IO5_C.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO1.5 PT20IO10_C.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO1.5 PT50IO0.5_C.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO1.5 PT50IO5_C.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO1.5 PT50IO10_C.RData")
plot_list <- vector("list",4)
for (i_bias in 1:4) {
  load(v_data[9*i_bias-8])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT5IO0.5"),60)
  df_25vs10vsgen1 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_bias-7])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT5IO5"),60)
  df_25vs10vsgen2 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_bias-6])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT5IO10"),60)
  df_25vs10vsgen3 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_bias-5])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT20IO0.5"),60)
  df_25vs10vsgen4 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_bias-4])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT20IO5"),60)
  df_25vs10vsgen5 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_bias-3])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT20IO10"),60)
  df_25vs10vsgen6 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_bias-2])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT50IO0.5"),60)
  df_25vs10vsgen7 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_bias-1])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT50IO5"),60)
  df_25vs10vsgen8 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_bias])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT50IO10"),60)
  df_25vs10vsgen9 <- data.frame(model,value,PTIO)
  
  df_25vs10vsgen <- as.data.frame(rbind(df_25vs10vsgen1,df_25vs10vsgen2,df_25vs10vsgen3,
                                        df_25vs10vsgen4,df_25vs10vsgen5,df_25vs10vsgen6,
                                        df_25vs10vsgen7,df_25vs10vsgen8,df_25vs10vsgen9))
  df_25vs10vsgen$model <- factor(df_25vs10vsgen$model,levels = c("perc25","perc10","gen"))
  df_25vs10vsgen$PTIO <- factor(df_25vs10vsgen$PTIO,levels = c("PT5IO0.5","PT5IO5","PT5IO10",
                                                               "PT20IO0.5","PT20IO5","PT20IO10",
                                                               "PT50IO0.5","PT50IO5","PT50IO10"))
  df_25vsgen <- df_25vs10vsgen %>%
    filter(model %in% c("perc25", "gen"))
  df_10vsgen <- df_25vs10vsgen %>%
    filter(model %in% c("perc10", "gen"))
  
  df_25vsgen_IO5IO10 <- df_25vsgen %>%
    filter(PTIO %in% c("PT5IO5","PT5IO10",
                       "PT20IO5","PT20IO10",
                       "PT50IO5","PT50IO10"))
  df_10vsgen_IO5IO10 <- df_10vsgen %>%
    filter(PTIO %in% c("PT5IO5","PT5IO10",
                       "PT20IO5","PT20IO10",
                       "PT50IO5","PT50IO10"))
  
  # box + viol
  p1 <- ggplot(df_25vsgen, aes(x = PTIO, y = value, fill = model)) +
    geom_flat_violin(width = 2,position = position_dodge(width = 1)) +
    geom_boxplot(width = 0.4,position = position_dodge(width = 1), alpha = 0.1,outlier.size = 2,outlier.alpha = 0.5) +
    scale_x_discrete(expand = expansion(mult = c(0.1, 0.1))) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(x = "Scenarios", y = "MSE", title = "The 25% model vs Generic model in MSE in Different Scenarios") +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = "black"), 
      plot.background = element_rect(fill = "white"),  
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(color = "black"), 
      axis.ticks.length = unit(0.2, "cm"))
  
  p2 <- ggplot(df_10vsgen, aes(x = PTIO, y = value, fill = model)) +
    geom_flat_violin(width = 2,position = position_dodge(width = 1)) +
    geom_boxplot(width = 0.4,position = position_dodge(width = 1), alpha = 0.1,outlier.size = 2,outlier.alpha = 0.5) +
    scale_x_discrete(expand = expansion(mult = c(0.1, 0.1))) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(x = "Scenarios", y = "MSE", title = "The 10% model vs Generic model in MSE in Different Scenarios") +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = "black"), 
      plot.background = element_rect(fill = "white"),  
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(color = "black"),
      axis.ticks.length = unit(0.2, "cm"))
  
  p3 <- ggplot(df_25vsgen_IO5IO10, aes(x = PTIO, y = value, fill = model)) +
    geom_flat_violin(width = 2,position = position_dodge(width = 1)) +
    geom_boxplot(width = 0.4,position = position_dodge(width = 1), alpha = 0.1,outlier.size = 2,outlier.alpha = 0.5) +
    scale_x_discrete(expand = expansion(mult = c(0.1, 0.1))) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(x = "Scenarios", y = "MSE", title = "The 25% model vs Generic model in MSE in Different Scenarios") +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = "black"), 
      plot.background = element_rect(fill = "white"),  
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(color = "black"), 
      axis.ticks.length = unit(0.2, "cm"))
  
  p4 <- ggplot(df_10vsgen_IO5IO10, aes(x = PTIO, y = value, fill = model)) +
    geom_flat_violin(width = 2,position = position_dodge(width = 1)) +
    geom_boxplot(width = 0.4,position = position_dodge(width = 1), alpha = 0.1,outlier.size = 2,outlier.alpha = 0.5) +
    scale_x_discrete(expand = expansion(mult = c(0.1, 0.1))) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(x = "Scenarios", y = "MSE", title = "The 10% model vs Generic model in MSE in Different Scenarios") +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = "black"), 
      plot.background = element_rect(fill = "white"),  
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(color = "black"),
      axis.ticks.length = unit(0.2, "cm"))
  plot_list[[i_bias]][[1]] <- p1
  plot_list[[i_bias]][[2]] <- p2
  plot_list[[i_bias]][[3]] <- p3
  plot_list[[i_bias]][[4]] <- p4
}


origin_plot_25vsgen <- plot_list[[1]][[1]]
origin_plot_10vsgen <- plot_list[[1]][[2]]
origin_plot_25vsgen_IO5IO10 <- plot_list[[1]][[3]]
origin_plot_10vsgen_IO5IO10 <- plot_list[[1]][[4]]

combine_plot_25vsgen <- ggarrange(plot_list[[3]][[1]],plot_list[[2]][[1]],plot_list[[4]][[1]],
                                  ncol = 1, nrow = 3, 
                                  labels = c("seed1217 TO5","seed0620 TO1.5","seed1217 TO1.5"),
                                  label.x = 0.78,
                                  label.y = 0.995,
                                  align = "v")
combine_plot_10vsgen <- ggarrange(plot_list[[3]][[2]],plot_list[[2]][[2]],plot_list[[4]][[2]],
                                  ncol = 1, nrow = 3, 
                                  labels = c("seed1217 TO5","seed0620 TO1.5","seed1217 TO1.5"),
                                  label.x = 0.78,
                                  label.y = 0.995,
                                  align = "v")
combine_plot_25vsgen_IO5IO10 <- ggarrange(plot_list[[3]][[3]],plot_list[[2]][[3]],plot_list[[4]][[3]],
                                          ncol = 1, nrow = 3, 
                                          labels = c("seed1217 TO5","seed0620 TO1.5","seed1217 TO1.5"),
                                          label.x = 0.78,
                                          label.y = 0.995,
                                          align = "v")
combine_plot_10vsgen_IO5IO10 <- ggarrange(plot_list[[3]][[4]],plot_list[[2]][[4]],plot_list[[4]][[4]],
                                          ncol = 1, nrow = 3, 
                                          labels = c("seed1217 TO5","seed0620 TO1.5","seed1217 TO1.5"),
                                          label.x = 0.78,
                                          label.y = 0.995,
                                          align = "v")

ggsave(filename = "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Plot box with viol_new/without unmeasured/MSE/Origin 25vsgen_C.png",plot = origin_plot_25vsgen,width = 16,height = 6,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Plot box with viol_new/without unmeasured/MSE/Origin 10vsgen_C.png",plot = origin_plot_10vsgen,width = 16,height = 6,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Plot box with viol_new/without unmeasured/MSE/Sensitivity 25vsgen_C.png",plot = combine_plot_25vsgen,width = 16,height = 18,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Plot box with viol_new/without unmeasured/MSE/Sensitivity 10vsgen_C.png",plot = combine_plot_10vsgen,width = 16,height = 18,dpi = 330)

ggsave(filename = "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Plot box with viol_new/without unmeasured/MSE/Origin_IO5IO10 25vsgen_C.png",plot = origin_plot_25vsgen_IO5IO10,width = 16,height = 6,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Plot box with viol_new/without unmeasured/MSE/Origin_IO5IO10 10vsgen_C.png",plot = origin_plot_10vsgen_IO5IO10,width = 16,height = 6,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Plot box with viol_new/without unmeasured/MSE/Sensitivity_IO5IO10 25vsgen_C.png",plot = combine_plot_25vsgen_IO5IO10,width = 16,height = 18,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Plot box with viol_new/without unmeasured/MSE/Sensitivity_IO5IO10 10vsgen_C.png",plot = combine_plot_10vsgen_IO5IO10,width = 16,height = 18,dpi = 330)


# U-shape
v_data <- c("D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO5 PT5IO0.5_US.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO5 PT5IO5_US.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO5 PT5IO10_US.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO5 PT20IO0.5_US.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO5 PT20IO5_US.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO5 PT20IO10_US.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO5 PT50IO0.5_US.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO5 PT50IO5_US.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO5 PT50IO10_US.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO1.5 PT5IO0.5_US.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO1.5 PT5IO5_US.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO1.5 PT5IO10_US.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO1.5 PT20IO0.5_US.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO1.5 PT20IO5_US.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO1.5 PT20IO10_US.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO1.5 PT50IO0.5_US.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO1.5 PT50IO5_US.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed0620 TO1.5 PT50IO10_US.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO5 PT5IO0.5_US.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO5 PT5IO5_US.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO5 PT5IO10_US.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO5 PT20IO0.5_US.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO5 PT20IO5_US.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO5 PT20IO10_US.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO5 PT50IO0.5_US.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO5 PT50IO5_US.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO5 PT50IO10_US.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO1.5 PT5IO0.5_US.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO1.5 PT5IO5_US.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO1.5 PT5IO10_US.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO1.5 PT20IO0.5_US.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO1.5 PT20IO5_US.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO1.5 PT20IO10_US.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO1.5 PT50IO0.5_US.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO1.5 PT50IO5_US.RData",
            "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO1.5 PT50IO10_US.RData")
plot_list <- vector("list",4)
for (i_bias in 1:4) {
  load(v_data[9*i_bias-8])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT5IO0.5"),60)
  df_25vs10vsgen1 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_bias-7])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT5IO5"),60)
  df_25vs10vsgen2 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_bias-6])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT5IO10"),60)
  df_25vs10vsgen3 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_bias-5])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT20IO0.5"),60)
  df_25vs10vsgen4 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_bias-4])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT20IO5"),60)
  df_25vs10vsgen5 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_bias-3])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT20IO10"),60)
  df_25vs10vsgen6 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_bias-2])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT50IO0.5"),60)
  df_25vs10vsgen7 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_bias-1])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT50IO5"),60)
  df_25vs10vsgen8 <- data.frame(model,value,PTIO)
  
  load(v_data[9*i_bias])
  perc25 <- sapply(1:20, function(x)Result_form[[x]][7,4])
  perc10 <-  sapply(1:20, function(x)Result_form[[x]][8,4])
  gen <-  sapply(1:20, function(x)Result_form[[x]][9,4])
  model <- rep(c("perc25","perc10","gen"),each = 20)
  value <- c(perc25,perc10,gen)
  PTIO <- rep(c("PT50IO10"),60)
  df_25vs10vsgen9 <- data.frame(model,value,PTIO)
  
  df_25vs10vsgen <- as.data.frame(rbind(df_25vs10vsgen1,df_25vs10vsgen2,df_25vs10vsgen3,
                                        df_25vs10vsgen4,df_25vs10vsgen5,df_25vs10vsgen6,
                                        df_25vs10vsgen7,df_25vs10vsgen8,df_25vs10vsgen9))
  df_25vs10vsgen$model <- factor(df_25vs10vsgen$model,levels = c("perc25","perc10","gen"))
  df_25vs10vsgen$PTIO <- factor(df_25vs10vsgen$PTIO,levels = c("PT5IO0.5","PT5IO5","PT5IO10",
                                                               "PT20IO0.5","PT20IO5","PT20IO10",
                                                               "PT50IO0.5","PT50IO5","PT50IO10"))
  df_25vsgen <- df_25vs10vsgen %>%
    filter(model %in% c("perc25", "gen"))
  df_10vsgen <- df_25vs10vsgen %>%
    filter(model %in% c("perc10", "gen"))
  
  df_25vsgen_IO5IO10 <- df_25vsgen %>%
    filter(PTIO %in% c("PT5IO5","PT5IO10",
                       "PT20IO5","PT20IO10",
                       "PT50IO5","PT50IO10"))
  df_10vsgen_IO5IO10 <- df_10vsgen %>%
    filter(PTIO %in% c("PT5IO5","PT5IO10",
                       "PT20IO5","PT20IO10",
                       "PT50IO5","PT50IO10"))
  
  # box + viol
  p1 <- ggplot(df_25vsgen, aes(x = PTIO, y = value, fill = model)) +
    geom_flat_violin(width = 2,position = position_dodge(width = 1)) +
    geom_boxplot(width = 0.4,position = position_dodge(width = 1), alpha = 0.1,outlier.size = 2,outlier.alpha = 0.5) +
    scale_x_discrete(expand = expansion(mult = c(0.1, 0.1))) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(x = "Scenarios", y = "MSE", title = "The 25% model vs Generic model in MSE in Different Scenarios") +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = "black"), 
      plot.background = element_rect(fill = "white"),  
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(color = "black"), 
      axis.ticks.length = unit(0.2, "cm"))
  
  p2 <- ggplot(df_10vsgen, aes(x = PTIO, y = value, fill = model)) +
    geom_flat_violin(width = 2,position = position_dodge(width = 1)) +
    geom_boxplot(width = 0.4,position = position_dodge(width = 1), alpha = 0.1,outlier.size = 2,outlier.alpha = 0.5) +
    scale_x_discrete(expand = expansion(mult = c(0.1, 0.1))) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(x = "Scenarios", y = "MSE", title = "The 10% model vs Generic model in MSE in Different Scenarios") +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = "black"), 
      plot.background = element_rect(fill = "white"),  
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(color = "black"),
      axis.ticks.length = unit(0.2, "cm"))
  
  p3 <- ggplot(df_25vsgen_IO5IO10, aes(x = PTIO, y = value, fill = model)) +
    geom_flat_violin(width = 2,position = position_dodge(width = 1)) +
    geom_boxplot(width = 0.4,position = position_dodge(width = 1), alpha = 0.1,outlier.size = 2,outlier.alpha = 0.5) +
    scale_x_discrete(expand = expansion(mult = c(0.1, 0.1))) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(x = "Scenarios", y = "MSE", title = "The 25% model vs Generic model in MSE in Different Scenarios") +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = "black"), 
      plot.background = element_rect(fill = "white"),  
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(color = "black"), 
      axis.ticks.length = unit(0.2, "cm"))
  
  p4 <- ggplot(df_10vsgen_IO5IO10, aes(x = PTIO, y = value, fill = model)) +
    geom_flat_violin(width = 2,position = position_dodge(width = 1)) +
    geom_boxplot(width = 0.4,position = position_dodge(width = 1), alpha = 0.1,outlier.size = 2,outlier.alpha = 0.5) +
    scale_x_discrete(expand = expansion(mult = c(0.1, 0.1))) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(x = "Scenarios", y = "MSE", title = "The 10% model vs Generic model in MSE in Different Scenarios") +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = "black"), 
      plot.background = element_rect(fill = "white"),  
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(color = "black"),
      axis.ticks.length = unit(0.2, "cm"))
  plot_list[[i_bias]][[1]] <- p1
  plot_list[[i_bias]][[2]] <- p2
  plot_list[[i_bias]][[3]] <- p3
  plot_list[[i_bias]][[4]] <- p4
}


origin_plot_25vsgen <- plot_list[[1]][[1]]
origin_plot_10vsgen <- plot_list[[1]][[2]]
origin_plot_25vsgen_IO5IO10 <- plot_list[[1]][[3]]
origin_plot_10vsgen_IO5IO10 <- plot_list[[1]][[4]]

combine_plot_25vsgen <- ggarrange(plot_list[[3]][[1]],plot_list[[2]][[1]],plot_list[[4]][[1]],
                                  ncol = 1, nrow = 3, 
                                  labels = c("seed1217 TO5","seed0620 TO1.5","seed1217 TO1.5"),
                                  label.x = 0.78,
                                  label.y = 0.995,
                                  align = "v")
combine_plot_10vsgen <- ggarrange(plot_list[[3]][[2]],plot_list[[2]][[2]],plot_list[[4]][[2]],
                                  ncol = 1, nrow = 3, 
                                  labels = c("seed1217 TO5","seed0620 TO1.5","seed1217 TO1.5"),
                                  label.x = 0.78,
                                  label.y = 0.995,
                                  align = "v")
combine_plot_25vsgen_IO5IO10 <- ggarrange(plot_list[[3]][[3]],plot_list[[2]][[3]],plot_list[[4]][[3]],
                                          ncol = 1, nrow = 3, 
                                          labels = c("seed1217 TO5","seed0620 TO1.5","seed1217 TO1.5"),
                                          label.x = 0.78,
                                          label.y = 0.995,
                                          align = "v")
combine_plot_10vsgen_IO5IO10 <- ggarrange(plot_list[[3]][[4]],plot_list[[2]][[4]],plot_list[[4]][[4]],
                                          ncol = 1, nrow = 3, 
                                          labels = c("seed1217 TO5","seed0620 TO1.5","seed1217 TO1.5"),
                                          label.x = 0.78,
                                          label.y = 0.995,
                                          align = "v")

ggsave(filename = "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Plot box with viol_new/without unmeasured/MSE/Origin 25vsgen_US.png",plot = origin_plot_25vsgen,width = 16,height = 6,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Plot box with viol_new/without unmeasured/MSE/Origin 10vsgen_US.png",plot = origin_plot_10vsgen,width = 16,height = 6,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Plot box with viol_new/without unmeasured/MSE/Sensitivity 25vsgen_US.png",plot = combine_plot_25vsgen,width = 16,height = 18,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Plot box with viol_new/without unmeasured/MSE/Sensitivity 10vsgen_US.png",plot = combine_plot_10vsgen,width = 16,height = 18,dpi = 330)

ggsave(filename = "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Plot box with viol_new/without unmeasured/MSE/Origin_IO5IO10 25vsgen_US.png",plot = origin_plot_25vsgen_IO5IO10,width = 16,height = 6,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Plot box with viol_new/without unmeasured/MSE/Origin_IO5IO10 10vsgen_US.png",plot = origin_plot_10vsgen_IO5IO10,width = 16,height = 6,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Plot box with viol_new/without unmeasured/MSE/Sensitivity_IO5IO10 25vsgen_US.png",plot = combine_plot_25vsgen_IO5IO10,width = 16,height = 18,dpi = 330)
ggsave(filename = "D:/Learning epi-master/Research Project/Code/Code and Result 2025/Plot box with viol_new/without unmeasured/MSE/Sensitivity_IO5IO10 10vsgen_US.png",plot = combine_plot_10vsgen_IO5IO10,width = 16,height = 18,dpi = 330)