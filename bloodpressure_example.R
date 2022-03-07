#blood pressure example from 
#https://online.stat.psu.edu/stat462/node/118/


library(tidyverse)  
library(ggplot2)
library(GGally)
library(gridExtra)

bloodpressure.df <- read.csv(
  file = "bloodpressure.txt",
  header = TRUE,
  sep = "\t",
  dec = ".",  #indicates the decimal point in your language
  stringsAsFactors = TRUE #FALSE by default starting with R 4.0.0
)


qplot(bloodpressure.df$Age, bloodpressure.df$BP)
cor(bloodpressure.df$Age, bloodpressure.df$BP)

qplot(bloodpressure.df$Age, bloodpressure.df$BP) + geom_smooth(method="lm", se=F )

model1 <- lm(BP ~ Age, data = bloodpressure.df)

summary(model1)

coef(model1)
fitted.values(model1)  #predicted values from the model
residuals(model1)

bloodpressure.df$Predicted = fitted.values(model1) 
bloodpressure.df$Residuals = residuals(model1)

modelg1 <- ggplot(bloodpressure.df, aes(x = Age, y = BP)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")

modelg1

#plotting residuals vs predicted with regression line
qplot(bloodpressure.df$Predicted, bloodpressure.df$Residuals) + geom_smooth(method="lm", se=F )


modelBP_Weight <- lm(bloodpressure.df$BP ~ bloodpressure.df$Weight)
cor(x=bloodpressure.df$BP, bloodpressure.df$Weight)
summary(modelBP_Weight)

#using WEIGHT instead of the predicted values to generate a plot:
qplot(bloodpressure.df$Weight, bloodpressure.df$Residuals) + geom_smooth(method="lm", se=F )


#Above: There seems to be a strong linear relationship between BP and weight
#Below: there does not seem to be a strong linear relationship between BP and
#    duration



qplot(bloodpressure.df$Dur, bloodpressure.df$Residuals) + geom_smooth(method="lm", se=F )



