library(stringr)
library(tidyverse)
library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(Hmisc)
library(olsrr)
library(readxl)
library(gridExtra)

#read in size data at week 1
size_data = read.csv("C:/Users/joemh/Downloads/final data analysis-20240408T164937Z-001/final data analysis/early_embryo_yolkratio_values.csv")

#add mom_numeric column
size_data = arrange(size_data, mom)
size_data$Mom_numeric = NA
size_data$Mom_numeric[1:23] = 5
size_data$Mom_numeric[24:47] = 6
size_data$Mom_numeric[48:71] = 7
size_data$Mom_numeric[72:95] = 8
size_data$Mom_numeric[96:119] = 9
size_data$Mom_numeric[120:143] = 10
size_data$Mom_numeric[144:167] = 11
size_data$Mom_numeric[168:191] = 12
size_data$Mom_numeric[192:213] = 2
size_data$Mom_numeric[214:237] = 3
size_data$Mom_numeric[238:261] = 4
size_data$Mom_numeric[262:285] = 1
size_data$Mom_numeric[286:309] = 17
size_data$Mom_numeric[310:333] = 18
size_data$Mom_numeric[334:357] = 19
size_data$Mom_numeric[358:381] = 20
size_data$Mom_numeric[382:405] = 21
size_data$Mom_numeric[406:429] = 22
size_data$Mom_numeric[430:453] = 23
size_data$Mom_numeric[454:477] = 24
size_data$Mom_numeric[478:499] = 13
size_data$Mom_numeric[500:522] = 14
size_data$Mom_numeric[523:546] = 15
size_data$Mom_numeric[547:569] = 16

#add adjusted yolk percentage column to make neat axes
size_data$adjusted_yolk_percentage = size_data$yolk_embryo_ratio*100

#make boxplots of developmental stage and size at week 1 of the experiment
ggplot(size_data, aes(x = Mom_numeric, y = yolk_embryo_ratio, fill = Elevation, group = Mom_numeric)) + geom_boxplot() + scale_y_reverse() + ylab("Yolk/Embryo Area at Week 1") + xlab("Brood") + scale_fill_manual(values=c("red", "blue")) + ggtitle("Developmental Stages Across Broods at Week 1")+ theme(plot.title = element_text(face="bold"))

ggplot(size_data, aes(x = Mom_numeric, y = embryo_area_mm, fill = Elevation, group = Mom_numeric)) + geom_boxplot()  + ylab("Embryo Area (mm) at Week 1") + xlab("Brood") + scale_fill_manual(values=c("red", "blue"))+ ggtitle("Embryo Areas Across Broods at Week 1")+ theme(plot.title = element_text(face="bold"))

ggplot(size_data, aes(x = yolk_embryo_ratio, y = embryo_area_mm, fill = Elevation, group = Mom_numeric)) + geom_boxplot() + xlab("Yolk/Embryo Ratio at Week 1") + ylab("Embryo Area (mm) at Week 1") + scale_fill_manual(values=c("red", "blue")) + scale_x_reverse() 

x = ggplot(size_data, aes(x = Mom_numeric, y = yolk_embryo_ratio, fill = Elevation, group = Mom_numeric)) + geom_boxplot() + scale_y_reverse() + ylab("Yolk/Embryo Area at Week 1") + xlab("Brood") + scale_fill_manual(values=c("red", "blue")) + ggtitle("Developmental Stages Across Broods at Week 1")+ theme(plot.title = element_text(face="bold"))

y = ggplot(size_data, aes(x = Mom_numeric, y = embryo_area_mm, fill = Elevation, group = Mom_numeric)) + geom_boxplot()  + ylab("Embryo Area (mm sq) at Week 1") + xlab("Brood") + scale_fill_manual(values=c("red", "blue"))+ ggtitle("Embryo Areas Across Broods at Week 1")+ theme(plot.title = element_text(face="bold"))

z = ggplot(size_data, aes(x = yolk_embryo_ratio, y = embryo_area_mm, fill = Elevation, group = Mom_numeric)) + geom_boxplot() + xlab("Yolk/Embryo Ratio at Week 1") + ylab("Embryo Area (mm) at Week 1") + scale_fill_manual(values=c("red", "blue")) + scale_x_reverse() 

grid.arrange(x,y)

#no title version of plots
xx = ggplot(size_data, aes(x = Mom_numeric, y = adjusted_yolk_percentage, fill = Elevation, group = Mom_numeric, color = Exp)) + geom_boxplot() + scale_fill_manual(values = c("red", "lightblue"))  + scale_color_manual(values=c("darkorange", "blue"), name = "Season")+ scale_x_continuous(breaks=c(1,6,12,18,24)) + ylab("Yolk Percentage at Week 1") + theme(axis.title.x = element_blank(),axis.text.x=element_blank())

yy = ggplot(size_data, aes(x = Mom_numeric, y = embryo_area_mm, fill = Elevation, group = Mom_numeric, color = Exp)) + geom_boxplot() + scale_fill_manual(values = c("red", "lightblue")) + scale_color_manual(values=c("darkorange", "blue"), name = "Season") + scale_x_continuous(breaks=c(1,6,12,18,24))  + ylab("Embryo Area (mm^2) at Week 1") + xlab("Brood")  

grid.arrange(xx,yy)

#calculate means, mins, and maxes
all_broods_developmentalstage_mean = mean(size_data$yolk_embryo_ratio)

all_broods_size_mean = mean(size_data$embryo_area_mm)

lower_el = filter(size_data, Elevation == "Lower")
higher_el = filter(size_data, Elevation == "Higher")

lower_el_development_mean = mean(lower_el$yolk_embryo_ratio)
lower_el_min =min(lower_el$yolk_embryo_ratio)
lower_el_max =max(lower_el$yolk_embryo_ratio)

higher_el_development_mean = mean(higher_el$yolk_embryo_ratio)
higher_el_min =min(higher_el$yolk_embryo_ratio)
higher_el_max =max(higher_el$yolk_embryo_ratio)

lower_el_size_mean = mean(lower_el$embryo_area_mm)
lower_el_size_min =min(lower_el$embryo_area_mm)
lower_el_size_max =max(lower_el$embryo_area_mm)

higher_el_size_mean = mean(higher_el$embryo_area_mm)
higher_el_size_min =min(higher_el$embryo_area_mm)
higher_el_size_max =max(higher_el$embryo_area_mm)

