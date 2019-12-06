data_sample_1 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_1.csv ")
library(psych)
library(tidyverse)

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

#For this part of the assignment we need the same cleaned data set
data_sample1_cleaned <- data_sample_1 %>% 	
  mutate(	
    STAI_trait = as.numeric(as.numeric(replace(STAI_trait, STAI_trait == "3.5", 35.0))),	
    household_income = as.integer(as.integer(replace(household_income, household_income == "-4562", 4562))))

view(data_sample1_cleaned)
#checking if we missed anything during the previous part
str(data_sample1_cleaned)
data_sample1_cleaned %>% summary()

#creating the model for performing backward regression
full.model <- lm( formula = pain ~ age + sex + STAI_trait + pain_cat + mindfulness 
                  + cortisol_serum + weight + IQ + household_income,
                   data = data_sample1_cleaned)

#####Model diagnostics for our backward regression#####
#influential outliers
cooks.distance(full.model)
plot(x = full.model, which = 4) 
#if we use 0.025 as a limit there seem to be some outliers 
#let's check the other assumptions and decide about it later

#1-checking the normality of the residuals
library(stats)
shapiro.test(residuals(full.model))
#creating a QQ plot for the normality of residuals
windows()
plot( x = full.model, which = 2 )
#I also want to ckeck kurtosis and skewness
describe(residuals(full.model))

#2-checking linearity assumption
windows()
plot(x = full.model, which = 1)
library(car)
windows()
residualPlots( model = full.model )

#3-checking homogeneity of variance
require(car)
ncvTest( full.model )

#4-multicollinearity
vif( full.model )




#now run the analysis
step( object = full.model,
       direction = "backward" )

#run the model with the retained predictors from the backward regression

backward_model= lm( pain ~ age + sex + pain_cat + mindfulness 
                  + cortisol_serum + weight, data = data_sample1_cleaned)


#get the regression equation
backward_model


#run our final model for the regression in part 1 of the assignment
theory_based_model <- lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness
             + cortisol_serum + cortisol_saliva, data = data_sample1_cleaned)

summary(theory_based_model)

#Backward and theory-based model comparison
summary(backward_model)$adj.r.squared
summary(theory_based_model)$adj.r.squared

AIC(backward_model)
AIC(theory_based_model)

anova(backward_model, theory_based_model) #just wanted to see what happens 
#anova isn't appropriate in this case because the two models aren't nested




#comparison between the initial full.model and the backward model
summary(full.model)
AIC(full.model)
AIC(backward_model)


summary(full.model)$adj.r.squared
summary(backward_model)$adj.r.squared

anova(full.model, backward_model)



####information for the coefficient table####
library(lm.beta)
sm=summary(backward_model)
sm
sm_table=coef_table(backward_model)
sm_table


data_sample_2= read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_2.csv")
library(psych)
library(tidyverse)
#checking for irregularities in the new data set
View(data_sample_2)
str(data_sample_2)	
data_sample_2 %>% summary() 	


#creating again the models for our comparison
backward_model2 <- lm(pain ~ sex + age + pain_cat + mindfulness
                      + cortisol_serum + weight, data = data_sample_2)


theory_based_model2 <- lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness
                         + cortisol_serum + cortisol_saliva, data = data_sample_2)

#calculation of the squared differences for the two models
RSS1 = sum((data_sample_2$pain - predict(backward_model2))^2)
RSS1

RSS2 = sum((data_sample_2$pain - predict(theory_based_model2))^2)
RSS2
mod_mean <- lm(pain ~ 1, data = data_sample_2)
TSS = sum((data_sample_2$pain - predict(mod_mean))^2)
TSS

R2 = 1 - (RSS1/TSS)
R2

R2= 1 - (RSS2/TSS)
R2






