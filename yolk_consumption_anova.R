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
library(broom)

data_final = read.csv("C:/Users/joemh/Desktop/final data analysis/all_treatments_yolk_consumption_rates.csv")

sample_sizes = data_final %>%
  group_by(mom) %>%
  dplyr::summarize(control_count = sum(Treatment_numeric == 0),hs_count = sum(Treatment_numeric == 1))

write.csv(sample_sizes, "C:/Users/joemh/Desktop/manuscript analysis/sample_sizes/yolk_consumption_sample_sizes.csv")
  


#read in embryo area and yolk ratio values
early_data = read.csv("C:/Users/joemh/Desktop/final data analysis/early_embryo_yolkratio_values.csv")


#read in average embryo area and yolk ratio values for each brood
avg_sizes = read.csv("C:/Users/joemh/Downloads/final data analysis-20240408T164937Z-001/final data analysis/avg_embryoarea_yolkratio_per_brood_treatment.csv")

test_data = left_join(data_final, early_data, join_by(mom, embryo, Treatment), relationship = "many-to-many")

avg_test_data = left_join(AvgData, avg_sizes, join_by(ID, Treatment), relationship = "many-to-many")

#make adjusted yolk percentage columns for neat axes
avg_test_data$adjusted_yolk_percentage = avg_test_data$YolkEmbryoRatio*100
avg_test_data$adjusted_yolk_percentageSD = avg_test_data$YolkEmbryoRatio_SD*100
#anova test for developomental stage and yolk consumption rate
p = aov(Estimate~yolk_embryo_ratio, data = test_data)

summary(p)

f = aov(Estimate~embryo_area_mm, data = test_data)

summary(f)



#make plot of yolk consumption rates on y axis and developmental stage on x axis with error bars for each axis
u = ggplot(data=avg_test_data, aes(x=adjusted_yolk_percentage,y=YolkConsumptionRate)) +scale_shape_manual(values = c(1, 16))
u+geom_errorbar(aes(ymax=YolkConsumptionRate+YolkConsumptionRate_SD,ymin=YolkConsumptionRate-YolkConsumptionRate_SD))+ geom_errorbarh(aes(xmax=YolkEmbryoRatio+YolkEmbryoRatio_SD,xmin=YolkEmbryoRatio-YolkEmbryoRatio_SD)) + geom_point(aes(color = Elevation, shape = Treatment, fill = Season)) + scale_color_manual(values=c("red","blue")) + scale_shape_manual(values=c(21,24), labels = c("Control", "Heat Shock"), name = "Treatment") + scale_fill_manual(values=c("black", "white"), name = "Season")  + scale_color_manual(values =c("red", "blue")) + geom_smooth(method = "lm", se = F, aes(color = Elevation))+ ylab("Mean Yolk Consumption Rate (%yolk/day)") + xlab("Mean Yolk/Embryo Ratio at Week 1") + ggtitle("Mean Brood Yolk Consumption Rates and\nDevelopmental Stages")+ theme(plot.title = element_text(face="bold"))



#plot without title
u+geom_errorbar(aes(ymax=YolkConsumptionRate+YolkConsumptionRate_SD,ymin=YolkConsumptionRate-YolkConsumptionRate_SD))+ geom_errorbarh(aes(xmax=adjusted_yolk_percentage+adjusted_yolk_percentageSD,xmin=adjusted_yolk_percentage-adjusted_yolk_percentageSD)) + geom_point(aes(color = Elevation, shape = Treatment, fill = Season)) + scale_color_manual(values=c("red","blue")) + scale_shape_manual(values=c(21,24), labels = c("Control", "Heat Shock"), name = "Treatment") + scale_fill_manual(values=c("black", "white"), name = "Season")  + scale_color_manual(values =c("red", "blue")) + geom_smooth(method = "lm", se = F, aes(color = Elevation))+ ylab("Mean Yolk Consumption Rate (%yolk/day)") + xlab("Mean Yolk Percentage at Week 1")
lm(YolkConsumptionRate~YolkEmbryoRatio*Elevation+Treatment, data = avg_test_data)
l = lm(YolkConsumptionRate~YolkEmbryoRatio*Elevation+Treatment, data = avg_test_data)

summary(l)

#test plot with one linear regression instead of two. Talk to Jonathon about which is better. 
u+geom_errorbar(aes(ymax=YolkConsumptionRate+YolkConsumptionRate_SD,ymin=YolkConsumptionRate-YolkConsumptionRate_SD))+ geom_errorbarh(aes(xmax=adjusted_yolk_percentage+adjusted_yolk_percentageSD,xmin=adjusted_yolk_percentage-adjusted_yolk_percentageSD)) + geom_point(aes(color = Elevation, shape = Treatment, fill = Season)) + scale_color_manual(values=c("red","blue")) + scale_shape_manual(values=c(21,24), labels = c("Control", "Heat Shock"), name = "Treatment") + scale_fill_manual(values=c("black", "white"), name = "Season")  + scale_color_manual(values =c("red", "blue")) + geom_smooth(method = "lm")+ stat_regline_equation(label.y=0.026) +stat_cor(aes(label = ..rr.label..), label.y=0.025) + ylab("Mean Yolk Consumption Rate (%yolk/day)") + xlab("Mean Yolk Percentage at Week 1")





W= lm(YolkConsumptionRate~YolkEmbryoRatio*as.factor(Elevation)+as.factor(Treatment), data = avg_test_data)

summary(W)

#ANOVAs for elevation and yolk consumption rate
X = aov(Estimate~Elevation_numeric * coll_date_numeric + Treatment_numeric + Mom_numeric/embryo, data = data_final)

summary(X)

Y = aov(Estimate~Elevation_numeric + coll_date_numeric  + Treatment_numeric + Mom_numeric/embryo, data = data_final)

summary(Y)

comparison = AIC(X,Y)
comparison

Z = aov(Estimate~Elevation_numeric * coll_date_numeric  + Treatment_numeric, data = data_final)

summary(Z)

comparison = AIC(X,Y,Z)

comparison

K = aov(Estimate~Elevation_numeric * coll_date_numeric  * Treatment_numeric * Mom_numeric/embryo, data = data_final)

summary(K)

write.csv(anova_summary[[1]], "C:/Users/joemh/Desktop/manuscript analysis/anova_tables/yolk_consumption_anova.csv")







## Calculate mean and SD for data for each mom
means<-ddply(data_final,.(mom,Treatment), function(d) mean(d$Estimate))
head(means)
names(means)=c("ID","Treatment","YolkConsumptionRate")

SD<-ddply(data_final,.(mom,Treatment), function(d) sd(d$Estimate))
head(SD)
names(SD)=c("ID","Treatment","YolkConsumptionRate_SD")

AvgData=merge(means,SD)
head(AvgData)

## now let's make a plot of the data using means+ error bars
AvgData$Season=c(rep("Winter",16),rep("Summer",8),rep("Winter",14),rep("Summer",8))
AvgData$Elevation=c(rep("Higher",24),rep("Lower",22))

write.csv(AvgData, "C:/Users/joemh/Downloads/final data analysis-20240408T164937Z-001/final data analysis/meta comparisons/yolk_consumption_data.csv")



#calculate SE for each brood and treatment group, first calculate SD then divide by sqrt of sample size
SDs<-ddply(AvgData,.(Elevation, Treatment, Season), function(d) sd(d$YolkConsumptionRate))

SDs$SE = NA
SDs$SE[1] = SDs$V1[1]/sqrt(4)
SDs$SE[2] = SDs$V1[2]/sqrt(8)
SDs$SE[3] = SDs$V1[3]/sqrt(4)
SDs$SE[4] = SDs$V1[4]/sqrt(8)
SDs$SE[5] = SDs$V1[5]/sqrt(4)
SDs$SE[6] = SDs$V1[6]/sqrt(8)
SDs$SE[7] = SDs$V1[7]/sqrt(4)
SDs$SE[8] = SDs$V1[8]/sqrt(8)


#calculate season means
summer_moms = filter(AvgData, Season == "Summer")
winter_data = filter (AvgData, Season == "Winter")
summer_moms_mean = mean(summer_moms$YolkConsumptionRate)
winter_moms_mean = mean(winter_data$YolkConsumptionRate)

#calculate SE for season means
summer_moms_SE = sd(summer_moms$YolkConsumptionRate)/sqrt(16)
winter_moms_SE = sd(winter_data$YolkConsumptionRate)/sqrt(30)

#calculate heat shock and control means
HS_moms = filter(AvgData, Treatment == "Heat Shock")
Control_moms = filter (AvgData, Treatment == "Control")
HS_mean = mean(HS_moms$YolkConsumptionRate)
Control_mean = mean(Control_moms$YolkConsumptionRate)

#calculate SE for heat shock and control means
HS_SE = sd(HS_moms$YolkConsumptionRate)/sqrt(23)
Control_SE = sd(Control_moms$YolkConsumptionRate)/sqrt(23)



q = ggplot(data=AvgData, aes(x=Elevation,y=YolkConsumptionRate, color = Season)) + scale_color_manual(values=c("darkorange", "blue"))
qq = q+geom_boxplot(aes(fill = Treatment)) +  scale_fill_manual(values=c("lightblue", "red"), limits = c("Control", "Heat Shock")) + stat_summary(fun.y = mean, geom="point", mapping=aes(fill = interaction(Season, Treatment)), shape = 18, size = 4, position = position_dodge(0.75)) + ylab("Mean Yolk Consumption Rate (%yolk/day)") + ggtitle("Mean Brood Yolk Consumption Rates\nAcross Elevations and Seasons") + theme(plot.title = element_text(face="bold"))

ggplot_build(qq)

#plot without title
q+geom_boxplot(aes(fill = Treatment)) +  scale_fill_manual(values=c("lightblue", "red"), limits = c("Control", "Heat Shock")) + stat_summary(fun.y = mean, geom="point", mapping=aes(fill = interaction(Season, Treatment)), shape = 18, size = 4, position = position_dodge(0.75)) + ylab("Mean Yolk Consumption Rate (%yolk/day)") 


growth_rates = read.csv("C:/Users/joemh/Downloads/final data analysis-20240408T164937Z-001/final data analysis/all_treatments_growth_rates_mm.csv")


yolk_and_growth_rates = left_join(data_final, growth_rates, join_by(embryo, Treatment, Mom_numeric), relationship = "many-to-many")

#make a plot of yolk rates over growth rates 
ggplot(yolk_and_growth_rates, aes(x = Estimate.y, y = Estimate.x)) + geom_point(aes(color = Elevation.x, shape = as.factor(Treatment_numeric.x), fill = coll_date.x)) + scale_color_manual(values=c("red","blue"), name = "Elevation") + scale_shape_manual(values=c(21,24), labels = c("Control", "Heat Shock"), name = "Treatment") + scale_fill_manual(values=c("black", "white"), name = "Season") + geom_smooth(method = "lm")+stat_regline_equation(label.y=0.045) +stat_cor(aes(label = ..rr.label..),label.y=0.0425)+ xlab("Growth Rate (mm^2/day)") + ylab("Yolk Consumption Rate (%yolk/day)")

yolk_and_growth_regression = lm(Estimate.y~Estimate.x, data = yolk_and_growth_rates) 
summary(yolk_and_growth_regression)




#make %difference plots! use "AvgData" dataframe to start. 
print(diff(AvgData$YolkConsumptionRate))
AvgData$RawDiff = NA
AvgData$RawDiff[2:46] = diff(AvgData$YolkConsumptionRate)
percent_diff = filter(AvgData, Treatment == "Control")
percent_diff$Mom_numeric = NA
numeric_moms = c(5,6,7,8,9,10,11,12,2,3,4,1,17,19,20,21,22,23,24,13,14,15,16)
percent_diff$Mom_numeric = numeric_moms
All_SDs = ddply(data_final,.(mom), function(d) sd(d$Estimate))
All_means = ddply(data_final,.(mom), function(d) mean(d$Estimate))
All_SDs$Percent_SD = All_SDs$V1/All_means$V1
percent_diff$YolkConsumptionRate_SD = All_SDs$Percent_SD
percent_diff$YolkConsumptionRate_SD = percent_diff$YolkConsumptionRate_SD * 100
heat_shock_broods = filter(AvgData, Treatment == "Heat Shock")
percent_diff$percent_diff = heat_shock_broods$RawDiff/percent_diff$YolkConsumptionRate
percent_diff$percent_diff = percent_diff$percent_diff*100

#add in mom 18 manually as she did not produce a yolk or growth rate. First, copy a dummy row then edit values accordingly. 
print(percent_diff[1,])
percent_diff[24,] = percent_diff[1,]
percent_diff[24,1] = "Lower zone WINTER 2"
percent_diff[24,3:4] = NA
percent_diff[24,6] = "Lower"
percent_diff[24,8] = NA
percent_diff[24,9] = 18


#now plot! 
ggplot(percent_diff, aes(x = as.factor(Mom_numeric), y = percent_diff)) + geom_point(aes(color = Elevation, shape = Season, size = 4)) + scale_color_manual(values=c("red","blue")) + scale_shape_manual(values=c(16,17), name = "Season") + geom_errorbar(aes(ymax=percent_diff+YolkConsumptionRate_SD,ymin=percent_diff-YolkConsumptionRate_SD)) + geom_hline(yintercept = 0.0) + ylab("Percent Change in Yolk Consumption Rate") + xlab("Brood")

#write csv of percent change dataframe to make lattice plot of all percent changes 
write.csv(percent_diff, "C:/Users/joemh/Desktop/manuscript analysis/percent_diff_data_frames/yolk_rates_percent_diff.csv")


#calculate mean percent change in yolk rate
mean(percent_diff$percent_diff)

#create subsets of percent_diff data frame to calculate effect size 
percent_diff_lower_elevation = filter(percent_diff, Elevation == "Lower")
mean(percent_diff_lower_elevation$percent_diff[1:7])

percent_diff_higher_elevation = filter(percent_diff, Elevation == "Higher")
mean(percent_diff_higher_elevation$percent_diff[9:12])


percent_diff_winter = filter(percent_diff, Season == "Winter")
mean(percent_diff_winter$percent_diff)
percent_diff_winter = arrange(percent_diff_winter, Elevation_numeric)
mean(percent_diff_winter$percent_diff[1:8])

percent_diff_summer = filter(percent_diff, Season == "Summer")
mean(percent_diff_summer$percent_diff[1:4])
mean(percent_diff_summer$percent_diff[5:8])
