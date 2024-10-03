library(stringr)
library(tidyverse)
library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(Hmisc)
library(olsrr)
library(readxl)

all_treatments_yolk_data = read.csv("C:/Users/joemh/Desktop/final data analysis/all_treatments_yolk_data_QC_complete.csv")



#current version of slope extraction, taken from summer yolk consumption calculation script
yolk_consumption_slopes = function(mom_data){
  slopes = dlply(mom_data, .(mom, embryo, Treatment, Elevation), function(d) summary(lm(yolk_embryo_ratio~Day_of_Exp, data = d)))
  coefficients = ldply(slopes, function(d) coef(d))
  x=length(coefficients)
  coefficients = filter(coefficients, Estimate < 0)
  coefficients$Estimate = coefficients$Estimate * -1
  return(coefficients)
}

yolk_slopes = yolk_consumption_slopes(all_treatments_yolk_data)

#check for duplicate rows
test_data = yolk_slopes[1:510,1:3]
duplicated(yolk_slopes[1:510,1:3])
sum(duplicated(yolk_slopes[1:510,1:3]))

ggplot(yolk_slopes,  aes( x = Treatment, y = Estimate, factor = Treatment)) + geom_point() + facet_wrap(~mom)


write.csv(yolk_slopes, "C:/Users/joemh/Desktop/final data analysis/all_treatments_yolk_consumption_rates.csv")


#current version of slope extraction, taken from summer yolk consumption calculation script
growth_slopes = function(mom_data){
  slopes = dlply(mom_data, .(mom, embryo, Treatment, Elevation), function(d) summary(lm(embryo_area~Day_of_Exp, data = d)))
  coefficients = ldply(slopes, function(d) coef(d))
  x=length(coefficients)
  coefficients = filter(coefficients,  Estimate < 100000)
  return(coefficients)
}


growth_rates = growth_slopes(all_treatments_yolk_data)

#check for duplicate rows
test_data = growth_rates[1:511,1:3]
duplicated(growth_rates[1:511,1:3])
sum(duplicated(growth_rates[1:511,1:3]))

ggplot(growth_rates, aes( x = Treatment, y = Estimate, factor = Treatment)) + geom_point() + facet_wrap(~mom)


write.csv(yolk_slopes, "C:/Users/joemh/Desktop/final data analysis/all_treatments_growth_rates.csv")









