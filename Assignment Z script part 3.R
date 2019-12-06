
data_3=read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_3.csv")
data_4=read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_4.csv")

library(psych) # for describe		
library(tidyverse) # for tidy code and ggplot		
library(cAIC4) # for cAIC		
library(r2glmm) # for r2beta		
library(lme4) # for lmer	
library(lmerTest) # to get singificance test in lmer	
library(MuMIn) # for r.squaredGLMM	

#for getting the St. Beta later
stdCoef.merMod <- function(object) {	
  sdy <- sd(getME(object,"y"))	
  sdx <- apply(getME(object,"X"), 2, sd)	
  sc <- fixef(object)*sdx/sdy	
  se.fixef <- coef(summary(object))[,"Std. Error"]	
  se <- se.fixef*sdx/sdy	
  return(data.frame(stdcoef=sc, stdse=se))	
}	


#look at the data for irregularities
#Data file 3
view(data_3)
str(data_3)	
data_3 %>% summary() 

##we have an extra factor for Female, we want to drop it
data3_final <- data_3 %>% 	
  mutate(sex = droplevels(replace(sex, sex == "Female", "female"))) 	

data3_final %>% summary() 
describe(data3_final)

#Data file 4
view(data_4)
str(data_4)
data_4  %>% summary()
##for data set 4 we have a negative value in household income that we 
##want to change
data_4$household_income[data_4$household_income==-23346]=23346
data_4  %>% summary() #there is another irregular negative value to change
data_4$household_income[data_4$household_income==-9250]=9250
data_4  %>% summary()
describe(data_4)


###No missing values or other irregularities, we can move on
##Build the linear mixed model on data3_final

mod_rand_int<- lmer(pain ~ sex + age + STAI_trait + pain_cat + mindfulness
                    + cortisol_serum + cortisol_saliva + (1 | hospital),data3_final)




###########Continue with the fixed effect regression model###########
mod_rand_int
#for coefficients
summary(mod_rand_int)
#for confidence intervals of the model coefficients
confint(mod_rand_int)
stdCoef.merMod(mod_rand_int)
##compute variance by the fixed effect predictors
r2beta(mod_rand_int, method = "nsj", data = data3_final)

r.squaredGLMM(mod_rand_int)

cAIC(mod_rand_int)$caic



#using the refression equation in data file 4
mod_rand_int2<- lmer(pain ~ sex + age + STAI_trait + pain_cat + mindfulness
                    + cortisol_serum + cortisol_saliva + (1 | hospital),data_4)

r2beta(mod_rand_int2, method = "nsj", data = data_4)

r.squaredGLMM(mod_rand_int2)

RSS = sum((data_4$pain - predict(mod_rand_int2))^2)
mod_mean <- lm(pain ~ 1, data = data_4)
TSS = sum((data_4$pain - predict(mod_mean))^2)
R2 = 1 - (RSS/TSS)
R2

#for this you have to run first the std coef function at the beginning for data 3
stdCoef.merMod(mod_rand_int)


#bulding new models including the most influential predictor of the previous model
#random intercept model
model_rnd_int3<-lmer(pain ~ pain_cat + (1 | hospital), data3_final)
model_rnd_slope<-lmer(pain ~ pain_cat + (pain_cat | hospital), data3_final)


sum(residuals(model_rnd_int3)^2)		
sum(residuals(model_rnd_slope)^2)		
cAIC(model_rnd_int3)$caic		
cAIC(model_rnd_slope)$caic		



#visualization of the lines of the two models
data3_slope = data3_final %>% mutate(pred_int = predict(model_rnd_int3),
                                pred_slope = predict(model_rnd_slope))

#random intercept model

windows()
data3_slope %>% ggplot() + aes(y = pain, x = pain_cat, group = hospital)+ 
             geom_point(aes(color = hospital), size = 4) +
  geom_line(color = "red", aes(y = pred_int, x = pain_cat)) +
  facet_wrap(~hospital, ncol = 2)

windows()
data3_slope %>% ggplot() + aes(y = pain, x = pain_cat,group = hospital)+
 geom_point(aes(color = hospital), size = 4) +
  geom_line(color = "red", aes(y = pred_slope, x = pain_cat)) +
  facet_wrap(~hospital, ncol = 2)



#run some basic model diagnostics in order to be sure that all the assumptions are met
#Influential outliers
library(influence.ME)
influence_observation = influence(mod_rand_int, obs = T)$alt.fixed
influence_group = influence(mod_rand_int,group = "hospital")$alt.fixed

windows()
data_plot_inflience = as_tibble(influence_group) %>% 	
  gather(colnames(influence_group), value = coefficient, key = predictor)	

data_plot_inflience %>% 	
  ggplot() +	
  aes(x = 1, y = coefficient, group = predictor) +	
  geom_violin() +	
  facet_wrap( ~ predictor, scales = "free")	

#normality of the residuals
library(lattice) # for qqmath	
windows()
qqmath(mod_rand_int, id = 0.05)
windows()
qqmath(ranef(mod_rand_int))#for ploting the random effect


#linearity and homoscedasticity of the fixed effect predictors and the outcome
windows()
plot(mod_rand_int, arg = "pearson")
#multicolinearity
windows()
pairs.panels(data3_final[, c("hospital", 
                              "sex", "age" , "STAI_trait" , "pain_cat" , "mindfulness",
                              "cortisol_serum" , "cortisol_saliva")], col = "red", lm = T)








