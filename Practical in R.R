##############################################################
#R script for the intro to repeated measures practical#
##############################################################

#install the packages you will need for this analysis
install.packages("haven")
install.packages("tidyr")
install.packages("mosaic")
install.packages("ggplot2")
install.packages("lme4")
install.packages("nlme")
install.packages("lmtest")

###Load libraries
library(haven)
library(tidyr)
library(mosaic)
library(ggplot2)
library(lme4)
library(nlme)
library(lmtest)

#Helpful commands for just clearing

#clear workspace
rm(list = ls())

#set up working directory
setwd('YOUR OWN DIRECTORY')

#Read in data
Dataset <- read_dta("Dataset.dta")
View(Dataset)

#You will notice several things
#The first is that the data is in wide format so each row is an individual

#The second is there are 13 columns representing 13 variables
#subject is the person identified
#sex is whether they are male (coded as 0) or female (coded as 1)
#mat_sclass is what social class they were born into with 0 being higher and 1 being lower
#fin_b is whether their parents had financial problems when growing up with 0 as no and 1 being yes
#age_t1 - age_t4 gives the age of the individual at each time point or wave (1-4)
#dep_t1 - dep_t4 gives the depression score of the individual at each time point (1-4)

#*You will also notice there is a lot of missing data as people will have dropped out of the study

#Turn variable subject into a factor
Dataset$subject <- factor(Dataset$subject)

#The first thing we will want to do is reshape the data from wide to long so we can start exploring our trajectories

#reshape data
data <- gather(Dataset, age_cat, age, age_t1:age_t4, factor_key=TRUE)
data_dep <- gather(Dataset, dep_cat, dep, dep_t1:dep_t4, factor_key=TRUE)
data$dep_cat = data_dep$dep_cat
data$dep= data_dep$dep

data = subset(data, select = -c(dep_t1,dep_t2,dep_t3,dep_t4)) #remove redundant columns

#The dataset is now long and we can check this. Look at participant 5.
#There are now 4 rows for this person which corresponds to each wave and their age goes up by each occasion.
#We have 3 new variables now: occasion (ranging between 1-4) age and dep (which are now long)
#Notice how their depression score also changes but their time invariant characterstics are constant#*For example sex is also 1 etc...

data1 = data[order(data$subject, data$age_cat),] #order by subject and occasion
View(data1)

#I normally just do a bit of tidying so that things are a bit cleaner to look at

rm(data_dep,Dataset,) #remove now redundant dataframes

##############################################################
#YOU SHOULD ALWAYS EXPLORE THE DATA FIRST
##############################################################
  
#These are some suggestions for things you could do

#We can explore some of the data now to see how depression might change with age

#Set the factors
data$dep_cat <- factor(data$dep_cat)
data$age_cat <- factor(data$age_cat)

#mean depression scores at each age
mean(~dep | age_cat, data=data, na.rm=T)
m.dep = mean(~dep | age_cat, data=data, na.rm=T) # save as a variable
favstats(~dep | age_cat, data=data, na.rm=T)

#mean depression scores for male/females
mean(~dep | age_cat & sex, data=data, na.rm=T)
m.dep.sex = mean(~dep | age_cat & sex, data=data, na.rm=T) # save as a variable

#mean ages scores at each age category
mean(~age | age_cat, data=data, na.rm=T) 
m.age = mean(~age | age_cat, data=data, na.rm=T) # save as a variable

#mean depression scores for male/females
mean(~age | age_cat & sex, data=data, na.rm=T) 
m.age.sex = mean(~age | age_cat & sex, data=data, na.rm=T) # save as a variable

#We can see that the mean age of the first assessment is 12.8 and the mean age of the last assessment is 17.8
#Depression scores also tend to go up from 4.03 to 6.38

#Plot averages 
df.plot = data.frame(Age = c(m.age,m.age.sex),Dep=c(m.dep,m.dep.sex),Subset=c('all','all','all','all','M','M','M','M','F','F','F','F'))
ggplot(df.plot,aes(x=Age, y=Dep, color = Subset)) +
  theme_light()+
  geom_point()+
  geom_line()

#However this might not be the same for each person so we can explore some individual specfic trajectories
#Let's have a look at participant 5 who has all 4 occasions

#Plot for person 5 
ggplot(data=subset(data, subject==5),aes(x=age, y=dep)) +
  theme_light()+
  geom_point()+
  geom_line()

#Plot for person 19 
ggplot(data=subset(data, subject==19),aes(x=age, y=dep)) +
  theme_light()+
  geom_point()+
  geom_line()

#Plot for person 6167 
ggplot(data=subset(data, subject==6167),aes(x=age, y=dep)) +
  theme_light()+
  geom_point()+
  geom_line()

#What about someone wit some missing data like participant 96
ggplot(data=subset(data, subject==96),aes(x=age, y=dep)) +
  theme_light()+
  geom_point()+
  geom_line()

##############################################################
#Running some analysis with lmer
##############################################################

#We can run multilevel growth curves using the lmer command in R and the syntax is actually very simple

#Let us start with a random intercept but fixed slope model. This will assume that
#everyone can have their own starting point (intercept), but grow at the same
#rate. Unrealistic maybe, but important to see the difference. We will use the
#lmer command to run this model:

#run models
#Random intercept
mod1 = lmer(dep~age + (1|subject), REML=F , data = data)
mod1
summary(mod1)

#The output is relatively simple
#We see that we have 8,361 individuals included in the analysis
#We see that the number of observations each person contributs ranges between 1 and 4 with a mean of 2.6
#We have estimates of log-likelihood and chi2 (discussed later)

#*We then have our regression estimates and our variance estimates

#*Our regression estimates say that at age 0, the average depression score for the population is -1.43
#For every one year increase in age, depression goes up by 0.44
#We see that both these parameters show strong evidence for being diffeent from 0
#Our variance estimates don't mean much at the moment but show that they are different from 0 as well.

#The intercept variance is 7.43
#The within individual variance is 15.35

#However, having a depression score at age 0 makes very little sense. 
#A common practise in MLM is to centre yout age/time variable to something more sensical (e.g., at the beginning of the trajectory or at the mean of all the ages). 
#This is quite easily done by just subtracting age from the a given value or the mean.
#Here we will centre age to the mean of all the measurements

###Age minus 15.07
summary(data$age)
data$age_15 = data$age - 15.07

#We now have a variable called age_15 which has a mean of 0, meaning the intercept can now be "shifted" to age 15. 

mod2 = lmer(dep~age_15 + (1|subject), REML=F , data = data)
mod2
summary(mod2)

#How does this vary with the first model? Makes a lot more sense right?
#Notice how the slope coefficent is the same but _cons (which is the intercept) is now 5.22.
#So at age 15, the average depression score is 5.22 and with every year increase goes up by 0.44

#An alternative could be to create the age centered variable to be the beginning of the trajectory

###Age minus 11.33
summary(data$age)
data$age_11 = data$age - 11.33

#We now have a variable called age_11 which has a mean of around 3, meaning the intercept can now be "shifted" down so that it corresponds to the age of the first time point. 

mod3 = lmer(dep~age_11 + (1|subject), REML=F , data = data)
mod3
summary(mod3)

#Again notice that the slope coefficent hasn't changed, only the intercept as we have moved it.

#So let's plot this up and have a look at whether the plots match the estimates

#Use the predict command to create a prediced trajectory based upon estimates
#from the model. Note this is the predicted estimates for the average trajectory

#Then you can plot

data$predage = predict(mod1, data, re.form = NA)
data$predage11 = predict(mod3, data, re.form = NA)
ggplot()+
  geom_line(data = data, aes(x=age,  y = predage11), na.rm=T)+
  ylab('Depression')

#We also know from the lecture and from the formula that we can use the regression estimates to predict what a score should be at a given age
#So if we wanted to know what the depression score should be at age 16, we can calculate this

#We know the intercept score is 3.571637 so we just need to work out the slope

#First we subtract the intercept age from 16 to get the value we need to multiple by
# 11.33333 - 16 which is -4.66667
#we then multiple the slope value (0.44) by 4.66667 which is 2.0533348
#we then add those the new slope value and the intercept scores together
#3.571637 + 2.0533348 which is 5.6249718

data$predage = predict(mod1, data, re.form = NA)
data$predage11 = predict(mod3, data, re.form = NA)
ggplot()+
  geom_line(data = data, aes(x=age,  y = predage11), na.rm=T)+
  ylab('Depression') +
  geom_hline(yintercept=5.6249718, linetype="dashed", color = "red")+
  geom_vline(xintercept=16, linetype="dashed", color = "blue")

#You also need to report model fit
#Model fit is often calculated by -2*Log-Likelihood called Deviance (but you can also use AIC and BIC)

summary(mod3)

#Congratulations, you have now run your growth curve model and plotted the trajectory!
  
#Finally we will have a go at running a random intercept and random slope model.
#The syntax is actually very similar as all we are doing is adding in an additional random effect

#Random intercept + random slope
mod4 = lmer(dep~age_11 + (age_11 |subject), REML=F , data = data)
mod4
summary(mod4)

#We can see that the fixed part of the model (i.e., the regression coefficients) are quite similar
#However the random effects are now different
#The intercept variance has gone up and we now have additional random effects that quantify slope variance and the covariance between the intercept and slope
#We also see smaller within individual variance that is most likely being explained by the random slope

#We can compare models using a likelihood ratio test between the store estimates

lrtest(mod3, mod4)

#The LR tests shows that model 2 is preferred to model 1.

##############################################################
#Group specific trajectories#
##############################################################

#We will now look at plotting specific trajectories for certain groups. This 
#could be sex, early childhood financial problems or top quartile vs the lowest 
#quartile of deprivation - mostly anything really. 

#What we want to see is are there different trajectories by different groups
#We know that different trajetcories exist by sex
#remember sex is coded as men = 0 and women = 1

#You could simple add a main effect of sex into the model

#Random intercept + random slope with main effect of sex differences
mod_s1 = lmer(dep~age_11 + sex + (age_11|subject), REML=F , data = data)
mod_s1
summary(mod_s1)

#What this shows is that the the intercept score is now 2.82
#For every year increase in age depression still goes up by 0.44
#But this time we have a main effect of sex whereby if you're a woman, your depression score is 1.41 points higher

#Like above, to calculate a score at age 11 for a woman we would sex the slope to be 0 and then add the intercept and the main effect of sex
#so that would be 1.41 + 2.82 which is 4.23

data$pred = predict(mod_s1, data, re.form = NA)
data$MF[data$sex == 0] = 'Male'
data$MF[data$sex == 1] = 'Female'
ggplot()+
  geom_line(data = data, aes(x=age,  y = pred, colour=factor(MF)), na.rm=T)+
  ylab('Depression') +
  geom_hline(yintercept=4.23, linetype="dashed", color = "red")+
  geom_vline(xintercept=11.3, linetype="dashed", color = "blue")

#However, this model only tells us if women have a higher score and not if women get worse over time

#To do this, we need to create a new variable which is sex interacted with time

#Interacting sex and age
data$age_11sex = data$age_11*data$sex
data$sex <- factor(data$sex)

#Random intercept + random slope with main effect of sex differences and sex by time
mod_s2 = lmer(dep~age_11 + sex + age_11sex + (age_11|subject), REML=F , data = data)
mod_s2
summary(mod_s2)

#Compare the models

lrtest(mod_s1, mod_s2)

#Let's have a final check with the plots and just try to make sure the estimates are making sense
#We can do this by calculating what the scores for men and women should be at age 18 by subbing in the value from the model into our equations

#For men it's very simple as we just set the main effect and interaction term for sex to 0 (0.69*0 & 0.23*0)
#The intercept is 11.3333 so we subtract that from 18 which is 6.66667
#we then multiple 6.66667 by the slope coefficent (0.31) which is 2.0731937
#we then add that value to the score at the intercept (3.22) which is 5.2898047

data$pred = predict(mod_s2, data, re.form = NA)
data$MF[data$sex == 0] = 'Male'
data$MF[data$sex == 1] = 'Female'
ggplot()+
  geom_line(data = data, aes(x=age,  y = pred, colour=factor(MF)), na.rm=T)+
  ylab('Depression') +
  geom_hline(yintercept=5.289, linetype="dashed", color = "red")+
  geom_vline(xintercept=18, linetype="dashed", color = "blue")

#For women it's a bit more complicated as we use the main effect and interaction term for sex (0.69*0 & 0.23*0)
#The intercept is 11.3333 so we subtract that from 18 which is 6.66667
#we then multiple 6.66667 by the slope coefficent (0.31) + the slope for females (0.23) which is 3.6000018
#we then add that value to the score at the intercept (3.22) + the main effect of being female (0.69) which is 7.5100018

data$pred = predict(mod_s2, data, re.form = NA)
data$MF[data$sex == 0] = 'Male'
data$MF[data$sex == 1] = 'Female'
ggplot()+
  geom_line(data = data, aes(x=age,  y = pred, colour=factor(MF)), na.rm=T)+
  ylab('Depression') +
  geom_hline(yintercept=7.5100018, linetype="dashed", color = "red")+
  geom_vline(xintercept=18, linetype="dashed", color = "blue")

#Getting exact p values
coefs <- data.frame(coef(summary(mod_s2)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
