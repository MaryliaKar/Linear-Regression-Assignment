data_sample_1 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_1.csv ")
library(psych) # for describe
library(tidyverse) # for tidy code

coef_table = function(model){
  require(lm.beta)
  mod_sum = summary(model)
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,4], 3))	
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"]))	
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"	
  
  
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(model), confint(model), c(0, lm.beta(model)$standardized.coefficients[c(2:length(model$coefficients))])), 2)), mod_sum_p_values)	
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")	
  mod_sum_table["(Intercept)","Std.Beta"] = "0"	
  return(mod_sum_table)
}


view(data_sample_1)
#for more detailed view of the data set:
str(data_sample_1)	
data_sample_1 %>% summary() 	
 

#I've noticed some irregularities on my data set so I use the view function and the search to find the specific object 
#searching for the mistake
view(data_sample_1)

#now I will correct those mistakes found in STAI_trait and household_income
data_sample1_cleaned <- data_sample_1 %>% 	
  mutate(	
    STAI_trait = as.numeric(as.numeric(replace(STAI_trait, STAI_trait == "3.5", 35.0))),	
    household_income = as.integer(as.integer(replace(household_income, household_income == "-4562", 4562))))
#now we can check if the data set is cleaned as expected
data_sample1_cleaned %>% summary()	
str(data_sample1_cleaned)  
describe(data_sample1_cleaned) 


#Creating the relevant regression models

model1 <- lm(pain ~ sex + age, data = data_sample1_cleaned)


model2 <- lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness
             + cortisol_serum + cortisol_saliva, data = data_sample1_cleaned)

#If I want to see what happened with the categorical variable of sex in my regression (usually I have to create a new dummy variable but r does it automatically)
summary(model1)$coeff
#I can see the coding that r used to create the "dummy" variables
contrasts(data_sample1_cleaned$sex)


#information for the regression equation
model2
summary(model2)
summary(model1)
#I use summary function and r.squared to see how much variance is explained by the two models
summary(model1)$adj.r.squared

summary(model2)$adj.r.squared

#I use AIC to compare the model fit
AIC(model1)
AIC(model2)

#But since my second model contains a subset of the first model I can use ANOVA for the residual errors and model fit
anova(model1, model2)

#to check the confidence intervals
confint(model1)
confint(model2)

#for standardized coefficients
require(lm.beta)
lm.beta(model1)
lm.beta(model2)

          #######Creating the two coefficient tables using the function in the beginning#######
#first for model 1
sm = summary(model1)	
sm	
sm_table= coef_table(model1)
sm_table

#now for model 2
sm=summary(model2)
sm
sm_table(model2)
sm

######Checking the assumptions of linear regresion for the two models######
#Model 2 #check for influential outliers
cooks.distance(model2)
plot(x = model2, which = 4) 
plot(x = model2, which = 5) 
summary(cooks.distance(model2))

#1-checking the normality of the residuals
hist( x = residuals( model2 ), 
      xlab = "Value of residual",
      main = "Normality of the residuals", 
      breaks = 20 )
library(stats)
shapiro.test(residuals(model2))
#creating a QQ plot for the normality of residuals
plot( x = model2, which = 2 )

#2-checking linearity assumption
plot(x = model2, which = 1)
library(car)
windows()
residualPlots( model = model2 )

#3-checking homogeneity of variance
ncvTest( model2 )

#4-checking for multicollinearity
vif( model2 )

#model 1
#check for influential outliers
cooks.distance(model1)
plot(x = model1, which = 4) 

#1-checking the normality of the residuals
library(stats)
shapiro.test(residuals(model1))

#2-checking linearity assumption
library(car)
windows()
residualPlots( model = model1 )

#3-checking homogeneity of variance
ncvTest( model1 )

#4-checking for multicollinearity
vif( model1 )







