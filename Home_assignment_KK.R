##add a normal distribution curve
curve(dnorm(x, mean=mean(BMI), sd=sd(BMI)), add=TRUE, col="darkblue", lwd=2) ###not used
library(tidyverse)
class(home_sample_1$sex)##factor
table(home_sample_1$sex)###female is the baseline
options(digits=7) ###set decimals to default value
home_sample_1_big.bang<-home_sample_1 
summary(home_sample_1)
which.min(home_sample_1$STAI_trait) ##18

home_sample_1<-home_sample_1[-c(18),]###delete low score for STAI_trait
which.min(home_sample_1$household_income) ##48
home_sample_1<-home_sample_1[-c(48),]###delete negative income

require(ggplot2)                       ####histograms#####
hist.pain <- ggplot(home_sample_1, aes(pain)) + geom_histogram(aes(y = ..density..), 
colour = "black", fill = "white") + labs(x = "Pain Score(1-10)", y = "Density") + 
stat_function(fun = dnorm, args = list(mean = mean(home_sample_1$pain, na.rm = TRUE), 
sd = sd(home_sample_1$pain, na.rm = TRUE)), colour = "black", size = 1)
hist.pain


hist(home_sample_1$pain, main="Pain", xlab = "Pain Score (Range:1-10)", ylim= c(0,50))
hist(home_sample_1$age, main="Age", xlab= "Age", ylim= c(0,60))
hist(home_sample_1$STAI_trait, main="STAI", xlab= "STAI_trait(Range:20-80)", ylim=c(0,70))
hist(home_sample_1$pain_cat, main = "Pain Catastrophising", xlab= "Pain Catastrophising (Range:0-52)" ,ylim=c(0,60))
hist(home_sample_1$cortisol_serum, main = "Serum cortisol", xlab= "Cortisol", ylim= c(0,35))
hist(home_sample_1$cortisol_saliva, main = "Saliva cortisol", xlab= "Cortisol", ylim= c(0,35))##appears bimodal?
shapiro.test(home_sample_1$cortisol_saliva)##non-significant (p =.91)
hist(home_sample_1$mindfulness, main = "Mindful Attention Awareness Scale", xlab= "MAAS(range:1-6)", ylim= c(0,40))summary(home_sample_1)
describe(home_sample_1) ###skewness (1 to -1), Kurtosis (2 to -2)
require(lmtest)
mod1<-lm(pain~age+sex, data=home_sample_1) ###model 1
summary(mod1)
mod2.2
mod2
require (dplyr)
require(car)
mod2<-lm(pain~age+sex+STAI_trait+pain_cat+cortisol_serum+cortisol_saliva+mindfulness, data=home_sample_1)###model 2
summary(mod2)
###multicollinearity
mod1%>% vif()
mod2%>% vif()
##saliva_cortisol and serum_cortisol highly correlated 
cor.test(home_sample_1$cortisol_saliva, home_sample_1$cortisol_serum)

mod2x<-lm(pain~age+ sex+ STAI_trait+pain_cat+ cortisol_serum+mindfulness, data=home_sample_1)###STAI not sig

summary(mod2x)####STAI_trait non-significant
mod2.1<-lm(pain~age+sex+pain_cat+cortisol_serum+mindfulness, data=home_sample_1)
mod2.1 %>% vif()

hist( x = residuals( mod1 ),        ######residual histograms
      xlab = "Value of residual", main="Model 1",
      breaks = 20)
hist( x = residuals( mod2.1 ),
      xlab = "Value of residual",
      main = "Mod 2.1",
      breaks = 20)


plot(x =mod1 ,which = 4) ##cook's distance
plot(x = mod2.1, which = 4)
plot(x =mod1 ,which = 2) ##qq plots
plot(x = mod2.1, which = 2) 
shapiro.test(residuals(mod1)) ##normality (W = 0.99, p >0.05). 
shapiro.test(residuals(mod2.1)) ###(W = 0.99, p > 0.05)

####linearity
home_sample_1 %>%ggplot() +aes(x = age, y = pain) +geom_point()
home_sample_1 %>%ggplot() +aes(x = sex, y = pain) +geom_point()
home_sample_1 %>%ggplot() +aes(x = STAI_trait, y = pain) +geom_point()
home_sample_1 %>%ggplot() +aes(x = pain_cat, y = pain) +geom_point()
home_sample_1 %>%ggplot() +aes(x = cortisol_serum, y = pain) +geom_point()
home_sample_1 %>%ggplot() +aes(x = cortisol_saliva, y = pain) +geom_point()
home_sample_1 %>%ggplot() +aes(x = mindfulness, y = pain) +geom_point()

home_sample_1 %>%ggplot() +aes(x = age, y = pain) +geom_point()+ geom_smooth(method="lm")
home_sample_1 %>%ggplot() +aes(x = sex, y = pain) +geom_point()+ geom_smooth(method="lm")
home_sample_1 %>%ggplot() +aes(x = STAI_trait, y = pain) +geom_point()+ geom_smooth(method="lm")
home_sample_1 %>%ggplot() +aes(x = pain_cat, y = pain) +geom_point()+ geom_smooth(method="lm")
home_sample_1 %>%ggplot() +aes(x = cortisol_serum, y = pain) +geom_point()+ geom_smooth(method="lm")
home_sample_1 %>%ggplot() +aes(x = cortisol_saliva, y = pain) +geom_point()+ geom_smooth(method="lm")
home_sample_1 %>%ggplot() +aes(x = mindfulness, y = pain) +geom_point()+ geom_smooth(method="lm")

mod1 %>% plot(which=5) #####normality (standardised residuals vs leverage)
mod2.1 %>% plot(which=5)

# skewness and kurtosis	
describe(residuals(mod1))	####mean= 0,median=close to 0 (0.07), 1stquarter (0.99) and 3rd quarter (0.78) are similar. 
describe(residuals(mod2))	##mean=0,median=(0.08). 1st quarter (-0.68), 3rd quarter (0.87) 


yhat.1 <- fitted.values( object = mod1) ###normality of residuals
yhat.2.1 <- fitted.values( object = mod2.1 ) ###fitted vs observed values
plot( x = yhat.1,
              y = home_sample_1$pain,
              xlab = "Fitted Values",
         ylab = "Observed Values", main= "Model 1")

plot( x = yhat.2.1,
      y = home_sample_1$pain,
      xlab = "Fitted Values",
      ylab = "Observed Values", main= "Model 2.1")



mod1%>% residualPlots()###  linearity assumption 
mod2.1%>% residualPlots()

mod1%>% plot(which = 3)	
mod2.1%>% plot(which = 3)	
mod1%>% plot(which = 1)	
mod2.1%>% plot(which = 1)	

mod1%>% ncvTest() ##Chisquare = 0.0002943786, Df = 1, p = 0.98631##homoscescaticity 
mod2.1%>% ncvTest()##Chisquare = 0.0002943786, Df = 1, p = 0.98631

require(lmtest)
mod1%>% bptest()data##BP = 0.39417, df = 2, p-value = 0.8211
mod2.1 %>% bptest()##BP = 6.4203, df = 5, p-value = 0.2674

home_sample_1 %>% select(pain, age, sex, STAI_trait, pain_cat, cortisol_serum, cortisol_saliva, mindfulness) %>% 	pairs.panels(col = "turquoise", lm = T)	

require(tidyr)
require(broom)
require(lm.beta)
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

write.table(coef_table(mod1),file="coef_table_mod1.txt", sep = ",", quote = FALSE, row.names = T)###coef table mod 1 
 ## Rsquare value lower than original model
write.table(coef_table(mod2.1),file="coef_table_mod2.1.txt", sep = ",", quote = FALSE, row.names = T)
summary(mod1)
summary(mod2) 
summary(mod2.1)

require (psych)

##model comparison
anova(mod1, mod2.1)
AIC(mod1)
AIC(mod2.1)

##data checks for mod3

hist(home_sample_1$weight, main="Weight", xlab= "Weight", ylim= c(0,40))
hist(home_sample_1$IQ, main="IQ", xlab= "IQ", ylim= c(0,50))
hist(home_sample_1$household_income, main="Household Income", xlab= "Income", ylim= c(0,40))
summary(home_sample_1)
describe(home_sample_1)
which.min(home_sample_1$weight) ##15

####backward regression##########

mod3<-lm(pain~age+ sex+ STAI_trait+ pain_cat+ mindfulness+cortisol_serum+weight+IQ+household_income, data=home_sample_1)

summary(mod3) 

###checking assumptions for the new model mod3

hist( x = residuals( mod3),
      xlab = "Value of residual",
      main = "Mod 3",
      breaks = 20)
plot(x = mod3, which = 2) ##qqplot-normality
yhat.3 <- fitted.values( object = mod3 ) ##observed vs fitted (normality)
plot( x = yhat.3,
      y = home_sample_1$pain,
      xlab = "Fitted Values",
      ylab = "Observed Values", main= "Model 3")
plot(x =mod3,which = 4)##cook's
describe(residuals(mod3))
###mean   sd   median skew   kurtosis   se
##0    0.01   0.07   -0.14  -0.19    0.09


plot(x=mod3, which=1) ####linearity
mod3%>% residualPlots()
shapiro.test(residuals(mod3))##W = 0.99341, p-value = 0.6928
summary(residuals_mod3)###curvature tests (Tukey negative)
require(psych)
describe(residuals(mod3))
mod3 %>% plot(which=5) ##residuals vs leverage
plot(x =mod3,which = 3) ##homoscesdascity
mod3 %>% bptest()##homoscesdascity ##BP = 14.778, df = 9, p-value = 0.09722
ncvTest(mod3)##Chisquare = 0.008499805, Df = 1, p = 0.92654
mod3%>% vif() ##multicolinearity## all VIFs<3
summary(mod3)
write.table(coef_table(mod3),file="coef_table_mod3.txt", sep = ",", quote = FALSE, row.names = T)

full.model<- mod3###full model
step( object = full.model, # start at the full model
      direction = "backward") # allow it remove predictors but not add them +) 
backward.model<-lm(formula = pain ~ age + sex + pain_cat + cortisol_serum + mindfulness + 
                     weight, data = home_sample_1)
theory_based.model<-mod2.1###mod 2.1 is named theory-based model##

p_theory_based.model<- predict(theory_based.model, home_sample_2)####test
p_backward.model<- predict(backward.model, home_sample_2)

sum((home_sample_2$pain-p_theory_based.model)^2)###Residual sum of squares 
sum((home_sample_2$pain-p_backward.model)^2)

sqrt(sum(home_sample_2$pain-p_theory_based.model)^2)###RMSE
sqrt(sum(home_sample_2$pain-p_backward.model)^2)

AIC(full.model,theory_based.model, backward.model)##AIC

anova(theory_based.model, backward.model)
summary (full.model)
summary(backward.model)
summary(mod2.1)
write.table(coef_table(backward.model),file="coef_table_backmod.txt", sep = ",", quote = FALSE, row.names = T)
coef_table(full.model)
coef_table(theory_based.model)

##model comparison

anova(full.model, backward.model)
anova(backward.model, theory_based.model)

#########################################################################################
describe(home_sample_3)
which(is.na(home_sample_3))
which.max(home_sample_3$mindfulness)###score out of range (6.05)
home_sample_3_bigbang<-home_sample_3
summary(home_sample_3$mindfulness)
meanx<-mean(home_sample_3$mindfulness[-which.max(home_sample_3$mindfulness)])
summary(home_sample_3)
home_sample_3$mindfulness[which.max(home_sample_3$mindfulness)]= meanx
home_sample_3x<- home_sample_3 %>% mutate(sex = droplevels(replace(sex, sex == "Female", "female")),) 	

summary(home_sample_3x)

##datacheck
hist(home_sample_3x$pain, main="Pain 3x", xlab = "Pain Score (Range:1-10)", ylim= c(0,60))
hist(home_sample_3x$age, main="Age 3x", xlab= "Age", ylim= c(0,70))
hist(home_sample_3x$STAI_trait, main="STAI 3x", xlab= "STAI_trait(Range:20-80)", ylim=c(0,70))
hist(home_sample_3x$pain_cat, main = "Pain Catastrophising 3x", xlab= "Pain Catastrophising (Range:0-52)" ,ylim=c(0,80))
hist(home_sample_3x$cortisol_serum, main = "Serum cortisol 3x", xlab= "Cortisol", ylim= c(0,50))
shapiro.test(home_sample_3x$cortisol_serum)##non-sig
hist(home_sample_3x$cortisol_saliva, main = "Saliva cortisol 3x", xlab= "Cortisol", ylim= c(0,40))
hist(home_sample_3x$mindfulness, main = "Mindful Attention Awareness Scale 3x", xlab= "MAAS(range:1-6)", ylim= c(0,40))
hist(home_sample_3x$weight, main = "Weight 3x", xlab= "Weight", ylim= c(0,40))
hist(home_sample_3x$IQ, main = "IQ 3x", xlab= "IQ", ylim= c(0,60))##scores overly concentreated in the centre??
shapiro.test(home_sample_3x$IQ) ##non-sig
hist(home_sample_3x$household_income, main = "Household Income 3x", xlab= "Household Income", ylim= c(0,70))

library(psych) # for describe		
library(tidyverse) # for tidy code and ggplot		
library(cAIC4) # for cAIC		
library(r2glmm) # for r2beta		
library(lme4) # for lmer	
library(lmerTest) # to get singificance test in lmer	
library(MuMIn) # for r.squaredGLMM

stdCoef.merMod <- function(object) {	                     #####standardised coefficient
  sdy <- sd(getME(object,"y"))	
  sdx <- apply(getME(object,"X"), 2, sd)	
  sc <- fixef(object)*sdx/sdy	
  se.fixef <- coef(summary(object))[,"Std. Error"]	
  se <- se.fixef*sdx/sdy	
  return(data.frame(stdcoef=sc, stdse=se))	
}	

ModelME.1 <-lme4:: lmer(pain ~                                     
                sex+age + pain_cat+ cortisol_serum+ mindfulness+
                    (1|hospital), 
                  data = home_sample_3x, na.action = "na.omit",
                  REML = TRUE)

summary(ModelME.1)

coef_CI = confint(ModelME.1) ###confidence interval
coefficients<-stdCoef.merMod(ModelME.1)###standardised coefficients
write.table (coefficients, file = "ceofx", sep = ",", quote=F)
coefficients
# Extract out the coefficents 
model.1Plot <- tidy(ModelME.1, conf.int = TRUE)
model.1Plot
# Extract the coefficents of interest
model.1Plot_lite <- model.1Plot[ -c(1,7,8), ]
model.1Plot
# plot the coefficients 
ggplot(model.1Plot_lite, aes(x = term, y = estimate,
                         ymin = conf.low,
                         ymax = conf.high)) +
  theme_minimal() +
  geom_hline(yintercept = 0.0, color = 'black', size = 2.0) +
  geom_point() +
  geom_linerange() + coord_flip()    

plot(ModelME.1, col="black")##residual vs fitted

confint(ModelME.1) ###CI
qqnorm(resid(ModelME.1)) ###plot residuals
qqmath.(ModelME.1, id=0.05)###normality of resdiuals QQ plot
??qqmath
fixef(ModelME.1)###fixed effect
randomef<-write.table(ranef(ModelME.1), file= "ranef", quote= F, sep= ",")##random effect
require(MuMIn)
r.squaredGLMM(ModelME.1) ####R2
r2beta(ModelME.1, method = "nsj", data = home_sample_3x)3####R2 and confidence intervals

cAIC(ModelME.1)$caic		##conditional AIC
require(tibble)

influence_observation<-influence(Model.1x, obs= T)$fixed.effects ##############checking assumptions
influence_group<-influence(Model.1x, group = "hospital")$fixed.effects
influence_group1<-influence(Model.1x, group = "hospital")

require(car) ###influenceIndexplot

influenceIndexPlot(ModelME.1)######cook's distance
require(lattice)
qqmath(ModelME.1, id = 0.05, col = "black")###qqplot##normality
qqmath(ranef(ModelME.1), col= "black")##random effect
resid<-residuals(ModelME.1)
homosced_mod =lm(resid^2 ~ hospital, data = home_sample_3x)##homoscesdascity test
summary(homosced_mod)
pairs.panels(home_sample_3x[,c("sex", "age","pain_cat", "cortisol_serum", "mindfulness", "hospital")], col = "black", lm = T)
ModelME.1_test<- predict(ModelME.1, home_sample_4, allow.new.levels=T)
summary(ModelME.1_test)


# Extract out TSS
TSS <-sum((home_sample_4$pain - mean(home_sample_4$pain))^2)
RSS <- sum((home_sample_4$pain - ModelME.1_test)^2)

Variance_explained_ModelME<-1-(RSS/TSS)
Variance_explained

Model_slope<-lmer(pain ~ cortisol_serum+(cortisol_serum||hospital), ###slope
                    data = home_sample_3x, na.action = "na.omit",
                    REML = TRUE)

r.squaredGLMM(Model_slope)###R2s

r2beta(Model_slope, method = "nsj", data = home_sample_3x)##R2 with CI

cAIC(Model_slope)$caic

summary(Model_slope)



