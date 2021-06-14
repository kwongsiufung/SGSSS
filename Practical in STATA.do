*******************************************************************************
***DO file for the intro to repeated measures practical***
*Alex Kwong June 2021
*******************************************************************************

*import the dataset into Stata by either dragging it in or by using the use function

use "FILEPATH\Dataset.dta", clear

use "Dataset.dta", clear

*Browse the dataset

browse

*You will notice several things
*The first is that the data is in wide format so each row is an individual

*The second is there are 13 columns representing 13 variables
*subject is the person identified
*sex is whether they are male (coded as 0) or female (coded as 1)
*mat_sclass is what social class they were born into with 0 being higher and 1 being lower
*fin_b is whether their parents had financial problems when growing up with 0 as no and 1 being yes
*age_t1 - age_t4 gives the age of the individual at each time point or wave (1-4)
*dep_t1 - dep_t4 gives the depression score of the individual at each time point (1-4)

*You will also notice there is a lot of missing data as people will have dropped out of the study

*The first thing we will want to do is reshape the data from wide to long so we can start exploring our trajectories

reshape long dep_t age_t, i(subject) j(occasion)

*The dataset is now long and we can check this. Look at participant 5.
*There are now 4 rows for this person which corresponds to each wave and their age goes up by each occasion.
*We have 3 new variables now: occasion (ranging between 1-4) age and dep (which are now long)
*Notice how their depression score also changes but their time invariant characterstics are constant
*For example sex is also 1 etc...

*I normally just do a bit of tidying so that things are a bit cleaner to look at

rename age_t age
rename dep_t dep

order subject occasion age dep

sort subject occasion

*******************************************************************************
*YOU SHOULD ALWAYS EXPLORE THE DATA FIRST
*******************************************************************************

*These are some suggestions for things you could do

*We can explore some of the data now to see how depression might change with age

bysort occasion: sum age dep

*We can see that the mean age of the first assessment is 12.8 and the mean age of the last assessment is 17.8
*Depression scores also tend to go up from 4.03 to 6.38

*We can plot this as well

sort subject occ 

egen Age = mean(age), by(occasion)
egen Depression = mean(dep), by(occasion)
twoway connected Depression Age, sort

*Overall there looks to be quite a linear relationship where depression gets worse over time

*However this might not be the same for each person so we can explore some individual specfic trajectories
*Let's have a look at participant 5 who has all 4 occasions

twoway connect dep age if subject == 5

*Can also have a look at participant 19

twoway connect dep age if subject == 19

*And now for participant 6167

twoway connect dep age if subject == 6167

*Compare this with someone who is missing some data like participant 96

twoway connect dep age if subject == 96

*You can also explore how population trajectories might vary by different populations like males and females

bysort sex occ: sum age dep

*Females
egen Females = mean(dep) if sex==1, by(occasion)
*Male
egen Males = mean(dep) if sex==0, by(occasion)

twoway connected Females Age, sort || ///
connected Males Age, sort  

*Once you have had a little explore and you have a feel for the data, you can start to think about running some analysis

*******************************************************************************
*Running some analysis using mixed
*******************************************************************************

*We can run multilevel growth curves using the mixed command in Stata and the syntax is actually very simple

*Let us start with a random intercept but fixed slope model. This will assume that
*everyone can have their own starting point (intercept), but grow at the same
*rate. Unrealistic maybe, but important to see the difference. We will use the
*mixed command to run this model:

mixed dep age || subject:, covariance(uns) cformat(%9.2f)

*The output is relatively simple
*We see that we have 8,361 individuals included in the analysis
*We see that the number of observations each person contributs ranges between 1 and 4 with a mean of 2.6
*We have estimates of log-likelihood and chi2 (discussed later)

*We then have our regression estimates and our variance estimates

*Our regression estimates say that at age 0, the average depression score for the population is -1.43
*For every one year increase in age, depression goes up by 0.44
*We see that both these parameters show strong evidence for being diffeent from 0
*Our variance estimates don't mean much at the moment but show that they are different from 0 as well.
*The intercept variance is 7.43
*The within individual variance is 15.35

*However, having a depression score at age 0 makes very little sense. 
*A common practise in MLM is to centre yout age/time variable to something more sensical (e.g., at the beginning of the trajectory or at the mean of all the ages). 
*This is quite easily done by just subtracting age from the a given value or the mean.
*Here we will centre age to the mean of all the measurements

sum age
gen age_15 = age-15.06738
sum age_15

*We now have a variable called age_15 which has a mean of 0, meaning the intercept can now be "shifted" to age 15. 

mixed dep age_15 || subject:, covariance(uns) cformat(%9.2f)

*How does this vary with the first model? Makes a lot more sense right?
*Notice how the slope coefficent is the same but _cons (which is the intercept) is now 5.22.
*So at age 15, the average depression score is 5.22 and with every year increase goes up by 0.44

*An alternative could be to create the age centered variable to be the beginning of the trajectory

sum age
gen age_11 = age-11.33333
sum age_11

*We now have a variable called age_11 which has a mean of around 3, meaning the intercept can now be "shifted" down so that it corresponds to the age of the first time point. 

mixed dep age_11 || subject:, covariance(uns) cformat(%9.2f)

*Again notice that the slope coefficent hasn't changed, only the intercept as we have moved it.

*So let's plot this up and have a look at whether the plots match the estimates

*Use the predict command to create a prediced trajectory based upon estimates
*from the model. Note this is the predicted estimates for the average trajectory

predict simple_model

*The data need to be sorted before you can plot the graph. We use the plot
*command to create a graph using the predictions from the model.

sort subject occasion

twoway (line simple_model age, sort), ///
ytitle("Depression") ///
xtitle("Age")

*We can check that our model and graphs make sense by adding in reference lines
*to the graph. We know the age of the intercept is now 11 (or 11.33333) so that is our x
*line. We know the depression score at that age should be 3.57. We don't want to examine the impact of the slope so set that be 0. 
*So if we add reference lines, they should cross on the line of best fit

twoway (line simple_model age, sort), ///
yline(3.57) xline(11.33333) ///
ytitle("Depression") ///
xtitle("Age")

*We also know from the lecture and from the formula that we can use the regression estimates to predict what a score should be at a given age
*So if we wanted to know what the depression score should be at age 16, we can calculate this

*We know the intercept score is 3.571637 so we just need to work out the slope

*First we subtract the intercept age from 16 to get the value we need to multiple by
di 11.33333 - 16
*which is -4.66667
*we then multiple the slope value (0.44) by 4.66667
di 0.44 * 4.66667
*which is 2.0533348
*we then add those the new slope value and the intercept scores together
di 3.571637 + 2.0533348
*which is 5.6249718

*Let's check this in the plot
twoway (line simple_model age, sort), ///
yline(5.6249718) xline(16) ///
ytitle("Depression") ///
xtitle("Age")

*You also need to report model fit
*Model fit is often calculated by -2*Log-Likelihood called Deviance (but you can also use AIC and BIC)
*You can either do this by hand or use the data left behind by the mixed command

di %9.0f -2*(-64651.116)
di %9.0f -2*e(ll)

*We can store the estimates for comparing later models

estimates store m1

*Congratulations, you have now run your growth curve model and plotted the trajectory!

*Finally we will have a go at running a random intercept and random slope model.
*The syntax is actually very similar as all we are doing is adding in an additional random effect

mixed dep age_11 || subject: age_11, covariance(uns) cformat(%9.2f)

*We can see that the fixed part of the model (i.e., the regression coefficients) are quite similar
*However the random effects are now different
*The intercept variance has gone up and we now have additional random effects that quantify slope variance and the covariance between the intercept and slope
*We also see smaller within individual variance that is most likely being explained by the random slope

*Calculate deviance

di %9.0f -2*e(ll)

*And store estimates

estimates store m2

*We can compare models using a likelihood ratio test between the store estimates

lrtest m1 m2

*The LR tests shows that model 2 is preferred to model 1.

*******************************************************************************
*Group specfic trajectories
*******************************************************************************

*We will now look at plotting specific trajectories for certain groups. This 
*could be sex, early childhood financial problems or top quartile vs the lowest 
*quartile of deprivation - mostly anything really. 

*What we want to see is are there different trajectories by different groups
*We know that different trajetcories exist by sex
*remember sex is coded as men = 0 and women = 1

*You could simple add a main effect of sex into the model

mixed dep age_11 sex || subject: age_11, covariance(uns) cformat(%9.2f)

*Store the estimates

estimates store m3

*What this shows is that the the intercept score is now 2.82
*For every year increase in age depression still goes up by 0.44
*But this time we have a main effect of sex whereby if you're a woman, your depression score is 1.41 points higher

*Like above, to calculate a score at age 11 for a woman we would sex the slope to be 0 and then add the intercept and the main effect of sex
di 1.41 + 2.82
*which is 4.23

*first we do our predictions

predict complex_model

*Then we can check this in the plot

*Let's check this in the plot

twoway (line complex_model age if sex==0, sort lpattern(solid)) || ///
(line complex_model age if sex==1, sort lpattern(dash))||, ///
yline(4.23) xline(11.33333) ///
ytitle("Depression") ///
xtitle("Age") ///
legend(label(1 "Male") label(2 "Female"))

*However, this model only tells us if women have a higher score and not if women get worse over time

*To do this, we need to create a new variable which is sex interacted with time

gen age_11xsex = age_11*sex

*We then include both the main effect of sex and the interaction term into
*the model.

mixed dep age_11 sex age_11xsex || subject: age_11, covariance(uns) cformat(%9.2f)

*The results tell us that the average depression score for the population intercept is 3.22 
*For every one year increase in age, depression goes up by 0.31 
*However, women have depression scores that are 0.69 points higher compared to men
*And women have greater depression slopes that go up by a further 0.23 a year

*predictions

predict final_model

*Store the estimates

estimates store m4

*And test for model fit compared to the simple model which ignored sex specfic trajetcories

lrtest m3 m4

*The results tell us that the final model is better than the model which ignores how men and women change over time

*Let's have a final check with the plots and just try to make sure the estimates are making sense
*We can do this by calculating what the scores for men and women should be at age 18 by subbing in the value from the model into our equations

*For men it's very simple as we just set the main effect and interaction term for sex to 0 (0.69*0 & 0.23*0)
*The intercept is 11.3333 so we subtract that from 18
di 11.33333 - 18
*which is 6.66667
*we then multiple 6.66667 by the slope coefficent (0.31)
di 6.66667 * 0.31
*which is 2.0731937
*we then add that value to the score at the intercept (3.22)
di 3.2 + 2.0731937
*which is 5.2898047

twoway (line final_model age if sex==0, sort lpattern(solid)) || ///
(line final_model age if sex==1, sort lpattern(dash))||, ///
yline(5.2898047) xline(18) ///
ytitle("Depression") ///
xtitle("Age") ///
legend(label(1 "Male") label(2 "Female"))

*For women it's a bit more complicated as we use the main effect and interaction term for sex (0.69*0 & 0.23*0)
*The intercept is 11.3333 so we subtract that from 18
di 11.33333 - 18
*which is 6.66667
*we then multiple 6.66667 by the slope coefficent (0.31) + the slope for females (0.23)
di 6.66667 * (0.31 + 0.23)
*which is 3.6000018
*we then add that value to the score at the intercept (3.22) + the main effect of being female (0.69)
di 3.6000018 + (3.22 + 0.69)
*which is 7.5100018

twoway (line final_model age if sex==0, sort lpattern(solid)) || ///
(line final_model age if sex==1, sort lpattern(dash))||, ///
yline(7.5100018) xline(18) ///
ytitle("Depression") ///
xtitle("Age") ///
legend(label(1 "Male") label(2 "Female"))

