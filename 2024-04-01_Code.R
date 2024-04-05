rm(list = ls())
setwd("/Users/libai/Documents/RAnalysisProjects/11_箱线图/2024-04-01_Jerry张瑞恒")
library(showtext)
showtext_auto(enable=TRUE)

dt <- read.csv("data/raw/Results_21Mar2022.csv")
colnames(dt)
# [1] "mc_run_id"      "grouping"       "mean_ghgs"      "mean_land"      "mean_watscar"  
# [6] "mean_eut"       "mean_ghgs_ch4"  "mean_ghgs_n2o"  "mean_bio"       "mean_watuse"   
# [11] "mean_acid"      "sd_ghgs"        "sd_land"        "sd_watscar"     "sd_eut"        
# [16] "sd_ghgs_ch4"    "sd_ghgs_n2o"    "sd_bio"         "sd_watuse"      "sd_acid"       
# [21] "n_participants" "sex"            "diet_group"     "age_group"  
head(dt)

# 选择包含mean的列
dt_mean <- dt[, c(3:10)]

#绘制热图
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(pheatmap)

#删除na
dt_mean <- na.omit(dt_mean)

#相关性热图
correlation_matrix <- cor(dt_mean)
pheatmap(correlation_matrix, color = colorRampPalette(rev(brewer.pal(9, "RdBu")))(100),
         border_color = "black", fontsize = 8, main = "Correlation")

#箱线图
library(ggplot2)
library(tidyr)
library(dplyr)

library(treemap)

treemap(dt, index=c("grouping"), vSize="mean_land")

#mean_ghgs_n2o
treemap(dt, index=c("grouping"), vSize="mean_ghgs_n2o")


treemap(dt,
        index = c("grouping", "sex"),
        vSize = "mean_ghgs_n2o", 
        vColor = "sd_ghgs_n2o",
        type = "value",
        format.legend = list(scientific = FALSE, big.mark = " ")
)


library(autoReg)
#sex列因子化
dt$sex <- as.factor(dt$sex)

fit=glm(sex~mean_ghgs+mean_land+mean_watscar+mean_eut+mean_ghgs_ch4+mean_bio+mean_watuse+mean_acid+sd_ghgs+sd_land+sd_watscar+sd_eut+sd_ghgs_ch4+sd_bio+sd_watuse+sd_acid
         + diet_group + age_group
          ,data=dt,family="binomial")
result=autoReg(fit) 
result %>% myft()
modelPlot(fit)
