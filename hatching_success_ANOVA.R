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

data_final = read.csv("C:/Users/joemh/Desktop/final data analysis/all_treatments_hatching_success.csv")

data_final$Mom = str_replace_all(data_final$Mom, fixed(" "), "")

#make a scaled percent success column for making a neaat y axis on plots 
data_final$scaled_percent_success = data_final$Decimal_success*100

write.csv(data_final, "C:/Users/joemh/Downloads/final data analysis-20240408T164937Z-001/final data analysis/meta comparisons/hatching_success_data.csv")


#read in embryo area and yolk ratio values
average_sizes = read.csv("C:/Users/joemh/Desktop/final data analysis/avg_embryoarea_yolkratio_per_brood_treatment.csv")
average_sizes$ID = str_replace_all(average_sizes$ID, fixed(" "), "")

#add treatment_numeric column, r wont match control column for winter broods with "Control" string for some reason
average_sizes = arrange(average_sizes, Treatment)
average_sizes$Treatment_numeric = NA
average_sizes$Treatment_numeric[1:24] = 0
average_sizes$Treatment_numeric[25:48] = 1

#make data to test interaction between developmental stage and hatching success
test_data = left_join(data_final, average_sizes, join_by(Mom == ID, Treatment_numeric))

#make a scaled percent success and scaled yolk percentage columns for making a neat y axis on plots 
test_data$scaled_percent_success = test_data$Decimal_success*100
test_data$scaled_yolk_percentage = test_data$YolkEmbryoRatio*100

#anovas for developmental stage and embryo area and hatching success
c = aov(Decimal_success~YolkEmbryoRatio, data = test_data)

summary(c)

d = aov(Decimal_success~EmbryoAreamm, data = test_data)

summary(d)


b = aov(Decimal_success~YolkEmbryoRatio + Elevation_numeric + Coll_date_numeric, data = test_data)

summary(b)

#plot hatching success over developmental stage 
ggplot(test_data, aes(x = YolkEmbryoRatio, y = Decimal_success, color = Elevation)) + scale_color_manual(values=c("red", "blue")) + geom_point() + scale_x_reverse() + geom_smooth(method = "lm", se = F, aes(group = Elevation))+ scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + ylab("% Hatching Success") + xlab("Mean Yolk/Embryo Ratio at Week 1") + ggtitle("Developmental Stage and Hatching Success")+ theme(plot.title = element_text(face="bold"))

ggplot(test_data, aes(x = YolkEmbryoRatio, y = Decimal_success)) + geom_point() + scale_x_reverse() + geom_smooth(method = "lm")+ stat_regline_equation(label.y=1) +stat_cor(aes(label = ..rr.label..), label.y=0.95) + scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + ylab("% Hatching Success") + xlab("Mean Yolk/Embryo Ratio at Week 1") + ggtitle("Developmental Stage and Hatching Success")+ theme(plot.title = element_text(face="bold"))

#plot wihtout title
ggplot(test_data, aes(x = scaled_yolk_percentage, y = scaled_percent_success))+ ylab("% Hatching Success") + xlab("Mean Yolk Percentage at Week 1") +scale_y_continuous(breaks=c(40,60,80,100)) + geom_point(aes(color = Elevation, shape = as.factor(Treatment_numeric), fill = Coll_date)) + scale_color_manual(values=c("red","blue")) + scale_shape_manual(values=c(21,24), labels = c("Control", "Heat Shock"), name = "Treatment") + scale_fill_manual(values=c("black", "white"), name = "Season") + geom_smooth(method = "lm")+ stat_regline_equation(label.y=40) +stat_cor(aes(label = ..rr.label..), label.y=35)  

#check if slopes are significant
lm(Decimal_success ~ YolkEmbryoRatio * Elevation_numeric, data = test_data) 
L = lm(Decimal_success ~ YolkEmbryoRatio * Elevation_numeric, data = test_data) 

summary(L)

lm(Decimal_success ~ YolkEmbryoRatio , data = test_data) 
Q = lm(Decimal_success ~ YolkEmbryoRatio, data = test_data) 

summary(Q)



#plot developmental stage across broods regardless of treatment
#read in data
avg_brood_sizes = read.csv("C:/Users/joemh/Desktop/final data analysis/avg_embryoarea_yolkratio_per_brood.csv")

#add elevation column to avg_brood_size
avg_brood_sizes$Elevation = NA
avg_brood_sizes$Elevation[1:12] = "Higher"
avg_brood_sizes$Elevation[13:24] = "Lower"


ggplot(avg_brood_sizes, aes(x = Mom_numeric, y = YolkEmbryoRatio, fill = Elevation)) + geom_col() + scale_fill_manual(values=c("red", "blue"))+ xlab("Brood") + ylab("Mean Yolk/Embryo Area Ratio at Week 1") + ggtitle("Developmental Stages at Week 1") + theme(plot.title = element_text(face="bold"))

#anovas for hatching success and elevation
Y = aov(Decimal_success~Elevation_numeric + Coll_date_numeric  + Treatment_numeric + Mom_numeric, data = data_final)

summary(Y)

W = aov(Decimal_success~Elevation_numeric * Coll_date_numeric  + Treatment_numeric + Mom_numeric, data = data_final)

summary(W)


Z = aov(Decimal_success~Elevation_numeric * Coll_date_numeric  + Treatment_numeric, data = data_final)

summary(Z)

comparison = AIC(Y,W,Z)

comparison

K = aov(Decimal_success~Elevation_numeric * Coll_date_numeric  * Treatment_numeric * Mom_numeric, data = data_final)

anova_summary = summary(K)

summary(K)

write.csv(anova_summary[[1]], "C:/Users/joemh/Desktop/manuscript analysis/anova_tables/hatching_success_anova.csv")


#calculate SE for hatching success across elevations and seasons, first calculate SD then divide by sqrt of sample size
SDs<-ddply(data_final,.(Elevation, Coll_date), function(d) sd(d$Decimal_success))

SDs$SE = NA
SDs$SE[1] = SDs$V1[1]/sqrt(4)
SDs$SE[2] = SDs$V1[2]/sqrt(8)
SDs$SE[3] = SDs$V1[3]/sqrt(4)
SDs$SE[4] = SDs$V1[4]/sqrt(8)




ggplot(data_final, aes(x=Elevation,y=Decimal_success, Group = Elevation, color = Coll_date)) + geom_boxplot() + stat_summary(fun.y = mean, geom="point", shape = 18, size = 4, position = position_dodge(0.75)) + scale_color_manual(values=c("red", "blue"))  + scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + ylab("% Hatching Success") + labs(color = "Season") + ggtitle("Hatching Success Across\nElevations and Seasons")+ theme(plot.title = element_text(face="bold"))

plot = ggplot(data_final, aes(x=Elevation,y=Decimal_success, Group = Elevation, color = Coll_date)) + geom_boxplot() + stat_summary(fun.y = mean, geom="point", shape = 18, size = 4, position = position_dodge(0.75)) + scale_color_manual(values=c("red", "blue"))  + scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + ylab("% Hatching Success") + labs(color = "Season") + ggtitle("Hatching Success Across\nElevations and Seasons")+ theme(plot.title = element_text(face="bold"))



ggplot_build(plot)

#NO TITLE VERSION OF PLOT
ggplot(data_final, aes(x=Elevation,y=scaled_percent_success, Group = Elevation, color = Coll_date)) + geom_boxplot() + stat_summary(fun.y = mean, geom="point", shape = 18, size = 4, position = position_dodge(0.75)) + scale_color_manual(values=c("red", "blue")) + ylab("% Hatching Success") + labs(color = "Season") 


#calculate means and SE across elevations in isolation from season
lower_el = filter(data_final, Elevation == "Lower")
mean(lower_el$Decimal_success)
lower_el_SE = sd(lower_el$Decimal_success)/sqrt(24)

higher_el = filter(data_final, Elevation == "Higher")
mean(higher_el$Decimal_success)
higher_el_SE = sd(higher_el$Decimal_success)/sqrt(24)

#make %difference plots! use "data_final" dataframe to start.
data_final = arrange(data_final, Mom_numeric)
print(diff(data_final$Decimal_success))
data_final$RawDiff = NA
data_final$RawDiff[2:48] = diff(data_final$Decimal_success)
percent_diff = filter(data_final, Treatment_numeric == 0)
heat_shock_broods = filter(data_final, Treatment == "Heat Shock")
percent_diff$percent_diff = heat_shock_broods$RawDiff/percent_diff$Decimal_success
percent_diff$percent_diff = percent_diff$percent_diff*100
ggplot(percent_diff, aes(x = as.factor(Mom_numeric), y = percent_diff)) + geom_point(aes(color = Elevation, shape = Coll_date, size = 4)) + scale_color_manual(values=c("red","blue")) + scale_shape_manual(values=c(16,17), name = "Season") + geom_hline(yintercept = 0.0) + ylab("Percent Change in Hatching Success Rate") + xlab("Brood")


#write csv of percent change dataframe to make lattice plot of all percent changes 
write.csv(percent_diff, "C:/Users/joemh/Desktop/manuscript analysis/percent_diff_data_frames/hatch_rates_percent_diff.csv")

#calculate mean % change in hatching success rate
mean(percent_diff$percent_diff)

#create subsets of percent_diff data frame to calculate effect size 
percent_diff_lower_elevation = filter(percent_diff, Elevation_numeric == 0)
mean(percent_diff_lower_elevation$percent_diff)

percent_diff_higher_elevation = filter(percent_diff, Elevation_numeric == 1)
mean(percent_diff_higher_elevation$percent_diff)

percent_diff_winter = filter(percent_diff, Coll_date == "Winter")
mean(percent_diff_winter$percent_diff)
percent_diff_winter = arrange(percent_diff_winter, Elevation_numeric)
mean(percent_diff_winter$percent_diff[1:8])

percent_diff_summer = filter(percent_diff, Coll_date == "Summer")
mean(percent_diff_summer$percent_diff[1:4])
mean(percent_diff_summer$percent_diff[5:8])
