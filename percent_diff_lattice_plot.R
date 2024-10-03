library(stringr)
library(tidyverse)
library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(Hmisc)
library(olsrr)
library(readxl)
library(geomtextpath)
library(lsr)
library(ggpubr)
library(gridExtra)

hatch_diff = read.csv("C:/Users/joemh/Desktop/manuscript analysis/percent_diff_data_frames/hatch_rates_percent_diff.csv")
yolk_diff = read.csv("C:/Users/joemh/Desktop/manuscript analysis/percent_diff_data_frames/yolk_rates_percent_diff.csv") 
growth_diff = read.csv("C:/Users/joemh/Desktop/manuscript analysis/percent_diff_data_frames/growth_rates_percent_diff.csv")


#make percent diff plots for each variable, then combine into lattice plot!
ggplot(hatch_diff, aes(x = as.factor(Mom_numeric), y = percent_diff)) + geom_point(aes(color = Elevation, shape = Coll_date, size = 4)) + scale_color_manual(values=c("red","blue")) + scale_shape_manual(values=c(16,17), name = "Season") + geom_hline(yintercept = 0.0) + ylab("Percent Change in Hatching Success Rate") + xlab("Brood")

ggplot(yolk_diff, aes(x = as.factor(Mom_numeric), y = percent_diff)) + geom_point(aes(color = Elevation, shape = Season, size = 4)) + scale_color_manual(values=c("red","blue")) + scale_shape_manual(values=c(16,17), name = "Season") + geom_errorbar(aes(ymax=percent_diff+YolkConsumptionRate_SD,ymin=percent_diff-YolkConsumptionRate_SD)) + geom_hline(yintercept = 0.0) + ylab("Percent Change in Yolk Consumption Rate") + xlab("Brood")

ggplot(growth_diff, aes(x = as.factor(Mom_numeric), y = percent_diff)) + geom_point(aes(color = Elevation, shape = Season, size = 4)) + scale_color_manual(values=c("red","blue")) + scale_shape_manual(values=c(16,17), name = "Season") + geom_errorbar(aes(ymax=percent_diff+GrowthRate_SD,ymin=percent_diff-GrowthRate_SD)) + geom_hline(yintercept = 0.0) + ylab("Percent Change in Growth Rate") + xlab("Brood")

a = ggplot(hatch_diff, aes(x = as.factor(Mom_numeric), y = percent_diff)) + geom_point(aes(color = Elevation, shape = Coll_date, size = 4)) + scale_color_manual(values=c("red","blue")) + scale_shape_manual(values=c(16,17), name = "Season") + geom_hline(yintercept = 0.0) + ylab("Percent Change in Hatching Success Rate") + xlab("Brood")

b = ggplot(yolk_diff, aes(x = as.factor(Mom_numeric), y = percent_diff)) + geom_point(aes(color = Elevation, shape = Season, size = 4)) + scale_color_manual(values=c("red","blue")) + scale_shape_manual(values=c(16,17), name = "Season") + geom_errorbar(aes(ymax=percent_diff+YolkConsumptionRate_SD,ymin=percent_diff-YolkConsumptionRate_SD)) + geom_hline(yintercept = 0.0) + ylab("Percent Change in Yolk Consumption Rate") + xlab("Brood")+ theme(axis.title.x = element_blank(),axis.text.x=element_blank())

c = ggplot(growth_diff, aes(x = as.factor(Mom_numeric), y = percent_diff)) + geom_point(aes(color = Elevation, shape = Season, size = 4)) + scale_color_manual(values=c("red","blue")) + scale_shape_manual(values=c(16,17), name = "Season") + geom_errorbar(aes(ymax=percent_diff+GrowthRate_SD,ymin=percent_diff-GrowthRate_SD)) + geom_hline(yintercept = 0.0) + ylab("Percent Change in Growth Rate") + xlab("Brood")+ theme(axis.title.x = element_blank(),axis.text.x=element_blank())


grid.arrange(c,b,a, ncol = 1)
