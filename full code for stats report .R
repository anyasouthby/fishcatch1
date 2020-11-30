library(PPforest)
library(ggplot2)
library(dplyr)
library(readr)
library(ISLR)
library(nlme)
library(drc)
library(plotly)
library(car)
library(rgl)
data("fishcatch")

#ANALYSING ALL FISH DATA 

#functions to label our graphs correctly
show_as_cm = function(length3) {
  output = paste0(length3, " cm")
  return(output)
}
show_as_grams = function(weight) {
  output = paste0(weight, " g")
  return(output)
}

#overall box plot to get a grasp on the data 
fish_type_box = fishcatch %>%
  ggplot(aes(x = Type, y = weight)) +
  geom_boxplot() +
  scale_y_continuous(labels = show_as_grams) +
  labs(title = "Boxplot of The Weight of Different Fish Types", subtitle = "Fish Catch Data", x = "Fish Type", y = "Weight")
fish_type_box

#just roach box plot 
specific_fish_type_box = fishcatch %>%
  filter(Type == "Roach") %>%
  ggplot(aes(x = Type, y = weight)) +
  geom_boxplot() +
  scale_y_continuous(labels = show_as_grams) +
  labs(title = "Boxplot of The Weight of Roach", subtitle = "Fish Catch Data", x = "Fish Type", y = "Weight")
specific_fish_type_box

#just Smelt box plot 
specific_fish_type_box2 = fishcatch %>%
  filter(Type == "Smelt") %>%
  ggplot(aes(x = Type, y = weight)) +
  geom_boxplot() +
  scale_y_continuous(labels = show_as_grams) +
  labs(title = "Boxplot of The Weight of Smelt", subtitle = "Fish Catch Data", x = "Fish Type", y = "Weight")
specific_fish_type_box2


#Removing the row of a fish with weight 0

fishcatch = fishcatch %>%
  filter(weight > 0)

#exploratory analysis

#correlation tests 
correlation1 = cor.test(fishcatch$weight, fishcatch$length1)
correlation1
correlation2 = cor.test(fishcatch$weight, fishcatch$length2)
correlation2
correlation3 = cor.test(fishcatch$weight, fishcatch$length3)
correlation3
correlation4 = cor.test(fishcatch$weight, fishcatch$height)
correlation4
correlation5 = cor.test(fishcatch$weight, fishcatch$width)
correlation5


#so Pearson's Correlation coefficient between length and weight is r = 0.924606

#linear regression models

#just a simple linear regression 

simple_linear_regression = lm(weight ~ length3, data = fishcatch)
summary(simple_linear_regression)

#simple linear regression, log transformed

simple_linear_regression_log = lm(log1p(weight) ~ log1p(length3), data = fishcatch)
summary(simple_linear_regression_log)

#determining which variables to use through AIC
 
full = lm(weight ~ ., data = fishcatch)
summary(full)
null = lm(weight ~ 1, data = fishcatch)
summary(null)

#forward selection - selects length3 and type and height, AIC = 1436.75

step(null, scope = list(lower = null, upper = full), direction = "forward")

#backward selection - selects type and length1 and length2 and height, AIC = 1436.69

step(full, direction = "backward")

#stepwise selection - selects length3 and type and height, AIC = 1436.75

step(null, scope = list(upper = full), direction = "both")

#there is very little in it
#as the slightly higher AIC involves one less variable, this may be easier to use. It also avoids using two length variables, which would have potentially introducted the problem of multicollinearity

#testing for multicollinearity 

backwardsmodel = lm(weight ~ length1 + length2 + height + Type, data = fishcatch)
car::vif(backwardsmodel)

forwardsmodel = lm(weight ~ length3 + height + Type, data = fishcatch)
car::vif(forwardsmodel)


#creating a linear model including all of the variables selected by AIC
AIC_model_full = lm(weight ~ length3 + Type + height, data = fishcatch)
summary(AIC_model_full)

#LOG MODELS 

AIC_log_model_full = lm(log1p(weight) ~ log1p(length3) + height + Type, data = fishcatch)
summary(AIC_log_model_full)

AIC_log_model_no_height = lm(log1p(weight) ~ log1p(length3) + Type, data = fishcatch)
summary(AIC_log_model_no_height)

#null hypothesis: there is no relationship between the weight of fish and their length, height and type
#from the model, we can see that the p-value is < 2.2e-16
#since p-value < 0.05, we can reject the null hypothesis
#the adjusted R-squared value, 0.9343, tells us that 93.43% of the variation in the response variable (the weight) is explained by the length, height and Type
#taking the log of our values results in an adjusted R-squared value of 0.9955, which tells us that 99.55% of the variation in the log value of the weight is explained by the log values of the length, height and type 


#plotting the residuals and normality 

#non-transformed
residuals_plot = lm(weight ~ length3 + height + Type, data = fishcatch)
par(mfrow = c(2,2))

plot(residuals_plot)

#new residuals vs fitted values plot 
log_residuals_plot = lm(log1p(weight) ~ log1p(length3) + log1p(height) + Type, data = fishcatch)
par(mfrow = c(2,2))

plot(log_residuals_plot)

log_no_height_residuals = lm(log1p(weight) ~ log1p(length3) + Type, data = fishcatch)
par(mfrow = c(2,2))
plot(log_no_height_residuals)


#testing for autocorrelation

library(lmtest)

dwtest(formula = AIC_model_full, data = fishcatch)

dwtest(formula = AIC_log_model_full, data = fishcatch)

dwtest(formula = AIC_log_model_no_height, data = fishcatch)

bptest(formula = AIC_model_full, data = fishcatch)

bptest(formula = AIC_log_model_full, data = fishcatch)

bptest(formula = AIC_log_model_no_height, data = fishcatch)

#visualization

#plotting length against width with type and size illustrated

AIC_plot = ggplot(data = fishcatch,
                  aes(x = length3,
                      y = weight,
                      color = Type,
                      size = height)) +
  geom_point(alpha = .5) +
  scale_x_continuous(labels = show_as_cm) +
  scale_y_continuous(labels = show_as_grams) +
  labs(title = "The Weight of Fish Based on Length", subtitle = "Fish Catch Data", x = "Length", y = "Weight", color = "Type of Fish", size = "Height (% of Length)")
AIC_plot

#fitting a linear regression model to the basic length - weight model 

AIC_plot2 = ggplot(data = fishcatch,
                  aes(x = length3,
                      y = weight,
                      size = height)) +
  geom_point(alpha = .5, aes(color = Type)) +
  geom_smooth(method = 'lm') +
  scale_x_continuous(labels = show_as_cm) +
  scale_y_continuous(labels = show_as_grams) +
  labs(title = "A Linear Regression Model of The Weight of Fish Based on Length", subtitle = "Fish Catch Data", x = "Length", y = "Weight",color = "Type of Fish", size = "Height (% of Length)")
AIC_plot2


#plotting the log transformed length - weight model 

AIC_plot3 = ggplot(data = fishcatch,
                  aes(x = log1p(length3),
                      y = log1p(weight),
                      color = Type,
                      size = height)) +
  geom_point(alpha = .5) +
  scale_x_continuous(labels = show_as_cm) +
  scale_y_continuous(labels = show_as_grams) +
  labs(title = "The Weight of Fish Based on Length, Height and Type, Log Transformed", subtitle = "Fish Catch Data", x = "Length", y = "Weight", color = "Type of Fish", size = "Height as a % of Length")
AIC_plot3


#fitting a linear regression model to the log-transformed data

AIC_plot4 = ggplot(data = fishcatch,
                   aes(x = log1p(length3),
                       y = log1p(weight),
                       size = height)) +
  geom_point(alpha = .5, aes(color = Type)) +
  geom_smooth(method = "lm") +
  scale_x_continuous(labels = show_as_cm) +
  scale_y_continuous(labels = show_as_grams) +
  labs(title = "A Log Transformed Simple Linear Regression Model of The Weight of Fish Based on Length", subtitle = "Fish Catch Data", x = "Length", y = "Weight", color = "Type of Fish", size = "Height (% of Length)")
AIC_plot4

#creating a confidence / prediction interval


#plotting height as the x axis 

AIC_plot5 = ggplot(data = fishcatch,
                   aes(x = height,
                       y = log1p(weight),
                       size = length3)) +
  geom_point(alpha = .5, aes(color = Type)) +
  geom_smooth(method = "lm") +
  scale_y_continuous(labels = show_as_grams) +
  labs(title = "A Log Transformed Simple Linear Regression Model of The Weight of Fish Based on Height", subtitle = "Fish Catch Data", x = "Height (% of Length)", y = "Weight", color = "Type of Fish", size = "Length (cm)")
AIC_plot5

AIC_plot6 = ggplot(data = fishcatch,
                    aes(x = log1p(height),
                        y = log1p(weight),
                        size = length3)) +
  geom_point(alpha = .5, aes(color = Type)) +
  scale_y_continuous(labels = show_as_grams) +
  labs(title = "A Simple Linear Regression Model of The Weight of Fish Based on Height", subtitle = "Fish Catch Data", x = "Height (% of Length)", y = "Weight", color = "Type of Fish", size = "Length (cm)")
AIC_plot6


pairs(fishcatch)
