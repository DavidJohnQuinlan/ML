###########################################################################
#                                                                         #
#                     Investigating the Bike Dataset                      #
#                                                                         #
###########################################################################

# this dataset contains the hourly counts of total rental bikes sharing 
# system in 2011 and 2012. There are 13 columns in the dataset, of which 
# the first is the rental count and the remaining 12 are the following
# 12 explanatory variables:
#- Date
#- Season 
#- Year
#- Month
#- Hour
#- Holiday
#- Weekday
#- Weather
#- Temp
#- Atemp
#- Hum
#- Windspeed

# the question of interest is to find the elements which influence bike 
# rentals and what their effect is

# set the current directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# import the data
hours <- read.csv("./data/bike_dataset.csv")

# examine the dataset
head(hours)
tail(hours)
dim(hours)
str(hours)
summary(hours)
# possible outliers in cnt and windspeed

# remove the index variable date
hours1 <- hours[,-2]

# examine which other variables should be removed from the dataset
# pairs(hours1)

# find the correlation between the continuous variables
cor(hours1[,c(1,9,10,11,12)])
pairs(hours1[,c(1,9,10,11,12)])
# there is a very large correlation between the temp and the feel temp 
# so it would probably make sense to remove the feels like atemp

# remove atemp
hours1 <- hours1[,c(-10)]

# ensure that the variables are factor variables
hours1$yr <- as.factor(hours1$yr)
hours1$season <- as.factor(hours1$season)
hours1$mnth <- as.factor(hours1$mnth)
hours1$hr <- as.factor(hours1$hr)
hours1$holiday <- as.factor(hours1$holiday)
hours1$weekday <- as.factor(hours1$weekday)
hours1$weathersit <- as.factor(hours1$weathersit)


#  ------------------------------------------------------------------------
# Initial Exploratory Analysis --------------------------------------------
#  ------------------------------------------------------------------------

#  ------------------------------------------------------------------------
# Create some plots of the data -------------------------------------------
#  ------------------------------------------------------------------------

#  ------------------------------------------------------------------------
# Create some boxplots ----------------------------------------------------
#  ------------------------------------------------------------------------

# create some simple boxplots
par(mfrow = c(1,4), mar = c(2,3,2,0.25))
boxplot(hours1$cnt, main = "Count") # lots of potential outliers
boxplot(hours1$temp, main = "Temp") # no outliers
boxplot(hours1$hum, main = "Humidity") # one potential outlier in humidity
boxplot(hours1$windspeed, main = "Windspeed") # a few potential outliers
par(mfrow = c(1,1))

# create a nicer boxplot using the libraries reshape and ggplot
# load the necessary libraries
library(reshape)
library(ggplot2)
library(ggpubr, warn.conflicts = F)

# create a theme for the boxplots
boxplot_theme <- theme(axis.text.x=element_text(size=15, face = "bold"),
                       axis.title.x=element_blank(), axis.title.y = element_blank(),
                       plot.title = element_text(hjust = 0.5,size = 18, face = "bold"), 
                       legend.position="none",
                       axis.title=element_text(size=25,face="bold"),
                       text = element_text(size=20))


# create boxplots of the different variables and then put them together
Cnt_boxplot <- ggplot(hours1, aes(x="Count", y=cnt)) + geom_boxplot(fill = "#F8766D") + boxplot_theme
Temp_boxplot <- ggplot(hours1, aes(x="Temperature", y=temp)) + geom_boxplot(fill = "#9EB9D4") + boxplot_theme
Hum_boxplot <- ggplot(hours1, aes(x="Humidity", y=hum)) + geom_boxplot(fill = "Light Green") + boxplot_theme
WS_boxplot <- ggplot(hours1, aes(x="Windspeed", y=windspeed)) + geom_boxplot(fill = "Yellow") + boxplot_theme

# arrange the different boxplots into one page
ggarrange(Cnt_boxplot, Temp_boxplot, Hum_boxplot, WS_boxplot,
          ncol = 4, nrow = 1)

#  ------------------------------------------------------------------------
# Create some histograms --------------------------------------------------
#  ------------------------------------------------------------------------

# plot the histogram of each variable
par(mfrow = c(1,4))
hist((hours1$cnt), cex.main=2.25, 
     main = expression(paste("Count, ", mu, "=189.46, ", sigma, "=181.39")),
     xlab = "Count", col = "#F8766D", cex.lab = 1.5, cex.axis = 1.5)
hist(hours1$temp, cex.main=2.25, 
     main = expression(paste("Temp, ", mu, "=0.50, ", sigma, "=0.19")),
     xlab = "Temp", col = "#9EB9D4", cex.lab = 1.5, cex.axis = 1.5)
hist(hours1$windspeed, cex.main=2.25, 
     main = expression(paste("Windspeed, ", mu, "=0.19, ", sigma, "=0.12")),
     xlab = "Windspeed", col = "Yellow", cex.lab = 1.5, cex.axis = 1.5)
hist(hours1$hum, cex.main=2.25, 
     main = expression(paste("Humidity, ", mu, "=0.63, ", sigma, "=0.19")),
     xlab = "Humidity", col = "Light Green", cex.lab = 1.5, cex.axis = 1.5)
par(mfrow = c(1,1))


# Find the summary statistics for cnt, temp, hum and windspeed ------------

# find the means of both variables
mean(hours1$cnt)
mean(hours1$temp)
mean(hours1$hum)
mean(hours1$windspeed)

# find the standard deviation of both variables
sd(hours1$cnt)
sd(hours1$temp)
sd(hours1$hum)
sd(hours1$windspeed)


#  ------------------------------------------------------------------------
# Examine the different categorical variables against the hourly counts ---
#  ------------------------------------------------------------------------

# create a theme for the split boxplots
box_split_theme <- theme(plot.title = element_text(hjust = 0.5),
      text = element_text(size=15),
      axis.title=element_text(size=14, face="bold"))

# create a boxplot of the hourly counts split by season
season_box <- ggplot(data = hours1, aes(as.factor(season), cnt)) + geom_boxplot() +
  labs(x = "Season") + ggtitle("Boxplots of the hourly counts of bike rentals split by season") +
  box_split_theme
season_box

# create a boxplot of the hourly counts split by Year
yr_box <- ggplot(data = hours1, aes(as.factor(yr), cnt)) + geom_boxplot() +
  labs(x = "Year") + ggtitle("Boxplots of the hourly counts of bike rentals split by year") +
  box_split_theme
yr_box
# there is a slight difference between the median counts of those who 
# rented in 2011 and 2012.

# create a boxplot of the hourly counts split by month
Month_box <- ggplot(data = hours1, aes(as.factor(mnth), cnt)) + geom_boxplot() +
  labs(x = "Month") + ggtitle("Boxplots of the hourly counts of bike rentals split by month") +
  box_split_theme
Month_box
# there is a clear trend and increase in the counts as the months change

# create a boxplot of the hourly counts split by hour
hr_box <- ggplot(data = hours1, aes(as.factor(hr), cnt)) + geom_boxplot() +
  labs(x = "Hour") + ggtitle("Boxplots of the hourly counts of bike rentals split by hour") +
  box_split_theme
hr_box
# there is a definite change in the counts during the different times 
# of the day

# create a boxplot of the hourly counts split by holiday
holiday_box <- ggplot(data = hours1, aes(as.factor(holiday), cnt)) + geom_boxplot() +
  labs(x = "Holiday") + ggtitle("Boxplots of the hourly counts of bike rentals split by holiday") +
  box_split_theme
holiday_box
# there is not a huge difference between the counts between holidays and 
# non holidays

# create a boxplot of the hourly counts split by weekday
Weekday_box <- ggplot(data = hours1, aes(as.factor(weekday), cnt)) + geom_boxplot() +
  labs(x = "Weekday") + ggtitle("Boxplots of the hourly counts of bike rentals split by weekday") +
  box_split_theme
Weekday_box
# there is not a huge difference between the counts during different days

# create a boxplot of the hourly counts split by weather conditions
weather_box <- ggplot(data = hours1, aes(as.factor(weathersit), cnt)) + geom_boxplot() +
  labs(x = "Weather Conditions") + ggtitle("Boxplots of the hourly counts of bike rentals split by weather conditions") +
  box_split_theme
weather_box
# there is a clear difference in the counts depending on the type of weather


#  ------------------------------------------------------------------------
# Fit a linear model the dataset ------------------------------------------
#  ------------------------------------------------------------------------

# fit a simple linear model between the response variable count and the 
# explanatory variables
hours_lm <- lm(cnt ~ ., data = hours1)
summary(hours_lm)

# examine the residuals plots
par(mfrow = c(2,2))
plot(hours_lm)
par(mfrow = c(1,1))

# there are some major issues with the residual plots therefore, I believe
# that it might be necessary to apply some sort of transformation to the 
# data to ensure that a constant variance remains


#  ------------------------------------------------------------------------
# Stepwise Regression -----------------------------------------------------
#  ------------------------------------------------------------------------

# remove some of the variable from the model using a stepwise regression
# procedure

# load the MASS library
library(MASS)

# run the stepwise procedure
step <- stepAIC(hours_lm, direction="backward")
step$anova 
# this did not remove any variables

# load the RcmdrMisc library
library(RcmdrMisc)

# try a BIC stepwise regression procedure
stepwise(hours_lm, direction = "forward", criterion = "BIC")
# applying forward stepwise regression using BIC as information criterion


#  ------------------------------------------------------------------------
# Season vs Month ---------------------------------------------------------
#  ------------------------------------------------------------------------

# since there are no values which are suggested to be removed from the 
# model I will examine the difference between season and month
hours_lm1 <- lm(cnt ~ season + yr + hr + holiday + weekday + weathersit + 
                 temp + hum + windspeed, data = hours1)
summary(hours_lm1)

# now, replace season with month
hours_lm2 <- lm(cnt ~ mnth + yr + hr + holiday + weekday + weathersit + 
                  temp + hum +windspeed, data = hours1)
summary(hours_lm2)
# including month is actually slight worse when season so I will leave 
# season in and remove month

# continue with this model and see if there are other variables which 
# could be removed from the model
stepwise(hours_lm2, direction = "forward", "BIC")
# none were removed from the model


#  ------------------------------------------------------------------------
# Examine the hours variable ----------------------------------------------
#  ------------------------------------------------------------------------

# examine the hours as a continuous variable
# now, replace season with month
hours_lm3 <- lm(cnt ~ mnth + yr + as.numeric(hr) + holiday + weekday + weathersit + 
                  temp + hum +windspeed, data = hours1)
summary(hours_lm3)

# see if I can split the hours variable into separate groups 
# I will split the data based on the working day
# I will split the hr variable based on those rental counts which are
# of a similar level
hours1$work <- cut(as.numeric(hours1$hr), breaks=c(0,7,19,24), labels=c(0,1,2))

# convert all 2's to 0's
hours1$work[hours1$work == 2] <- 0

# examine the newly created variable
table(hours1$work)

# relevel the variable
hours1$work <- as.character(hours1$work)
hours1$work <- as.factor(hours1$work)

# refit the model excluding hr and including work
hours1_lm_work <- lm(cnt ~ season + yr + holiday + weekday + weathersit + 
                       temp + hum + windspeed + work, data = hours1)
summary(hours1_lm_work)
# there is a very large difference when examining the difference between
# hour and the newly created work variable

# I will try one more to split the hr into 5 categories of similar levels
hours1$work <- cut(as.numeric(hours1$hr), breaks=c(0,7,9,17,20,24), labels=c(0,1,2,3,4))
table(hours1$work)

# refit a linear model with this newly created variable
hours1_lm_work_new <- lm(cnt ~ season + yr + holiday + weekday + weathersit + 
                           temp + hum + windspeed + work, data = hours1)
summary(hours1_lm_work_new)
# this is a good transformation as there is still a large amount of 
# variation explained by the explanatory variables

#  ------------------------------------------------------------------------
# Convert weekday to a binary variable ------------------------------------
#  ------------------------------------------------------------------------

# split the weekday variable into a binary variable
hours1$weekend[hours$weekday <= 5 & hours$weekday >= 1] = 0
hours1$weekend[hours$weekday == 0 | hours$weekday == 6] = 1

# create a factor variable for weekend
hours1$weekend <- as.factor(hours1$weekend)

# fit a linear model including this new variable
hours1_wk <- lm(cnt ~ season + yr + holiday + weekend + weathersit + 
                  temp + hum + windspeed + work, data = hours1)
summary(hours1_wk)
# there has been a very minimal reduction in the R2 value when using 
# weekend therefore I will include it rather than the weekdays

# examine the residual diagnostic plots
par(mfrow = c(2,2))
plot(hours1_wk)
par(mfrow = c(1,1))
# the linear models assumptions do not hold
# try find a transformation that might help easy the model assumptions issues


#  ------------------------------------------------------------------------
# Boxplots of the new variables  ------------------------------------------
#  ------------------------------------------------------------------------

# create a boxplot of the hourly counts split by weekend variable
weekend_box <- ggplot(data = hours1, aes(as.factor(weekend), cnt)) + geom_boxplot() +
  labs(x = "Weekend") + ggtitle("Boxplots of the hourly counts of bike rentals split by the new hours variable") +
  box_split_theme
weekend_box

# create a boxplot of the hourly counts split by the new hr variable
work_box <- ggplot(data = hours1, aes(as.factor(work), cnt)) + geom_boxplot() +
  labs(x = "Hours New") + ggtitle("Boxplots of the hourly counts of bike rentals split by the new hours variable") +
  box_split_theme
work_box


#  ------------------------------------------------------------------------
# Transform the data ------------------------------------------------------
#  ------------------------------------------------------------------------

# load the MASS library
library(MASS)

# use the boxcox function to find the optimal transformation to the respones
boxcox(hours1_wk)
# the boxcox function suggests that a log transformation may be a good 
# transformation to allow for there to be a constant variance relationship
# between the response and the explanatory variables


#  ------------------------------------------------------------------------
# Fit a log transformation to the response --------------------------------
#  ------------------------------------------------------------------------

# fit a log transformation to the response variable and then fit a LM
hours1_log <- lm(log(cnt) ~ season + yr + holiday + weekend + weathersit + 
                  temp + hum + windspeed + work, data = hours1)
summary(hours1_log)

# examine the residual diagnostic plots
par(mfrow = c(2,2))
plot(hours1_log)
par(mfrow = c(1,1))
# the model assumptions still do not look great!


#  ------------------------------------------------------------------------
# Fit a sqrt to the response ----------------------------------------------
#  ------------------------------------------------------------------------

# fit a sqrt transformation to the response variable and then fit a LM
hours1_sqrt <- lm(sqrt(cnt) ~ season + yr + holiday + weekend + weathersit + 
                    temp + hum + windspeed + work, data = hours1)
summary(hours1_sqrt)

# examine the residual diagnostic plots
par(mfrow = c(2,2))
plot(hours1_sqrt)
par(mfrow = c(1,1))
# the model assumptions look better then previously!


#  ------------------------------------------------------------------------
# Remove outliers from the model ------------------------------------------
#  ------------------------------------------------------------------------

# identify if the outliers are influential

# load the car library
library(car)

# identify those points which are significant outliers
influencePlot(hours1_sqrt)

# remove the points 8855 and 9124 and 586 from the plot
hours2 <- hours1[c(-8855,-9124,-586),]
# these three observations are when the storms occur and probably should
# remain in the model

# examine these points
hours[c(8855, 9124, 586),]
# all of these points are part of the three observations that form the 
# stormy weather group.


#  ------------------------------------------------------------------------
# Fit a model excluding these values -------------------------------------
#  ------------------------------------------------------------------------

# fit a sqrt transformation to the response variable and then fit a LM
hours2_sqrt <- lm(sqrt(cnt) ~ season + yr + holiday + weekend + weathersit + 
                    temp + hum + windspeed + work, data = hours2)
summary(hours2_sqrt)
# examining the output it is clear that the three observations remove the 
# stormy weather variable from the model

# examine the residual diagnostic plots
par(mfrow = c(2,2))
plot(hours2_sqrt)
par(mfrow = c(1,1))


#  ------------------------------------------------------------------------
# Fit a Poisson model to the data -----------------------------------------
#  ------------------------------------------------------------------------

# a linear model is probably not the best choice of model anyway
# a more appropriate model would be a Poisson model which models counts
hours_pois <- glm(cnt ~ ., data = hours1, family=poisson(link=log))
summary(hours_pois)

# examine the residuals of the model
par(mfrow = c(2,2))
plot(hours_pois)
par(mfrow = c(1,1))
# these residuals look better however the QQ plot is not so good

# fit the newer model including the newly created variables
hours_pois1 <- glm(cnt ~ season + yr + holiday + weekend + weathersit + 
                     temp + hum + windspeed + work, data = hours1, 
                   family=poisson(link=log))
summary(hours_pois1)

# examine the residuals of the model
par(mfrow = c(2,2))
plot(hours_pois1)
par(mfrow = c(1,1))
# these residuals look better however the QQ plot is not so good
# examine the data for possible outliers


#  ------------------------------------------------------------------------
# Examine the model for overdispersion ------------------------------------
#  ------------------------------------------------------------------------

# examine the models for overdispersion

# load the AER library
library(AER)

# run an overdispersion test on the Poisson model fitted above
dispersiontest(hours_pois, trafo = 1, alternative = "greater")
# the very small p-value indicates how we reject the null hypothesis and conclude
# that there is evidence to suggest that there is a presence of overdispersion 
# within the Poisson model. 

# run an overdispersion test on the POoisson model fitted above
dispersiontest(hours_pois1, trafo = 1, alternative = "greater")
# the very small p-value indicates how we reject the null hypothesis and conclude
# that there is evidence to suggest that there is a presence of overdispersion 
# within the Poisson model. 

# both models are highly likely to not suitably fit the data as 
# both exhibit a presence of overdispersion


#  ------------------------------------------------------------------------
# Fit a quasi-Poisson model to the data -----------------------------------
#  ------------------------------------------------------------------------

# given that above model is over-dispersed, one way of dealing with overdispersion
# is to use a quasi-Poisson model

# fit a quasi-Poisson model to the dataset
hours_quasi <- glm(cnt ~ season + yr + holiday + weekend + weathersit + 
                     temp + hum + windspeed + work, family = quasipoisson, data = hours1)
summary(hours_quasi)
# this time not all the parameters are significant

# examine the diagnostics
par(mfrow = c(2,2))
plot(hours_quasi)
par(mfrow = c(1,1))

# Pearson residuals fluctuate around zero following approx a normal distn
plot(fitted(hours_quasi), residuals(hours_quasi, "pearson"))
title("Pearson Residual plot from Poisson Regression")
abline(h = 0)
abline(h = 2, lty = 2)
abline(h = -2, lty = 2)
# this residual plot has many deviance residuals which are greater then 2
# since the residuals have been standardized in some way i.e. see definition 
# of the Pearson residuals, values lying outside +-2 or 3 are potential 
# outliers 
# from the above model is appears that there are many possible outliers

# another type of residual is the deviance residual, which should also
# approximately have a standard normal distribution. 
plot(fitted(hours_quasi), residuals(hours_quasi, type = "deviance"))
title("Deviance Residual plot from Poisson Regression")
abline(h = 0)
abline(h = 2, lty = 2)
abline(h = -2, lty = 2)

# plot the standardized residuals
plot(fitted(hours_quasi), rstandard(hours_quasi, type = "pearson"))
title("Pearson Residual plot from Poisson Regression")
abline(h = 0)
abline(h = 2, lty = 2)
abline(h = -2, lty = 2)

# plot the standardized residauls
plot(fitted(hours_quasi), rstandard(hours_quasi, type = "deviance"))
title("Pearson Residual plot from Poisson Regression")
abline(h = 0)
abline(h = 2, lty = 2)
abline(h = -2, lty = 2)
# these do not look too bad, however, there are some values which are
# apparent outliers

# the quasi-Poisson model is not terrible
# the interpretation of the terms is not bad either

# fit another model to remove the windspeed variable
# fit a quasi-Poisson model to the dataset
hours_quasi1 <- glm(cnt ~ season + yr + holiday + weekend + weathersit + 
                     temp + hum + work, family = quasipoisson, data = hours1)
summary(hours_quasi1)
# this time not all the parameters are significant

# examine the diagnostics
par(mfrow = c(2,2))
plot(hours_quasi1)
par(mfrow = c(1,1))

# examine the standardised residuals again
# plot the standardized residauls
plot(fitted(hours_quasi1), rstandard(hours_quasi1, type = "pearson"))
title("Pearson Residual plot from Poisson Regression")
abline(h = 0)
abline(h = 2, lty = 2)
abline(h = -2, lty = 2)

# plot the standardized residauls
plot(fitted(hours_quasi1), rstandard(hours_quasi1, type = "deviance"))
title("Pearson Residual plot from Poisson Regression")
abline(h = 0)
abline(h = 2, lty = 2)
abline(h = -2, lty = 2)
# these do not look too bad, however, there are some values which are
# apparent outliers

# examine if its possible to remove any terms
# season
hours_season <- update(hours_quasi1, . ~ . - season)
anova(hours_season, hours_quasi1, test = "F")

# work
hours_work <- update(hours_quasi1, . ~ . - work)
anova(hours_work, hours_quasi1, test = "F")

# yr
hours_yr <- update(hours_quasi1, . ~ . - yr)
anova(hours_yr, hours_quasi1, test = "F")

# holiday
hours_holiday <- update(hours_quasi1, . ~ . - holiday)
anova(hours_holiday, hours_quasi1, test = "F")

# weathersit
hours_weather <- update(hours_quasi1, . ~ . - weathersit)
anova(hours_weather, hours_quasi1, test = "F")

# temp
hours_temp <- update(hours_quasi1, . ~ . - temp)
anova(hours_temp, hours_quasi1, test = "F")

# hum
hours_hum <- update(hours_quasi1, . ~ . - hum)
anova(hours_hum, hours_quasi1, test = "F")
# all terms are significant and their removal does not change the model

# create a table for latex using stargazer
# load the stargazer library
library(stargazer)
library(xtable)

# create the table
xtable(hours_quasi1)

# create a table of the linear models summary statistics
xtable(hours1_sqrt)


# -------------------------------------------------------------------------
# Fit a negative binomial model -------------------------------------------
# -------------------------------------------------------------------------

# load the MASS library
library(MASS)

# just to provide another model other then the quasi poisson model
hours_nb <- glm.nb(cnt ~ season + yr + holiday + weekend + weathersit + 
                     temp + hum + windspeed + work, data = hours1)
summary(hours_nb)

# examine the diagnostics
par(mfrow = c(2,2))
plot(hours_nb)
par(mfrow = c(1,1))

# Pearson residuals fluctuate around zero following approx a normal distn
plot(fitted(hours_nb), rstandard(hours_nb, type = "pearson"))
title("Pearson Residual plot from Poisson Regression")
abline(h = 0)
abline(h = 2, lty = 2)
abline(h = -2, lty = 2)
# this residual plot has many residuals which are greater then 2
# since the residuals have been standardized in some way i.e. see definition 
# of the Pearson residuals, values lying outside +-2 or 3 are potential 
# outliers from the above model is appears that there are many possible 
# outliers

# another type of residual is the deviance residual, which should also
# approximately have a standard normal distribution 
plot(fitted(hours_nb), rstandard(hours_nb, type = "deviance"))
title("Deviance Residual plot from Poisson Regression")
abline(h = 0)
abline(h = 2, lty = 2)
abline(h = -2, lty = 2)
# these look much better then the quasi-Poisson residuals plots

# examine the significance of the deviance
pchisq(deviance(hours_nb), df.residual(hours_nb), lower.tail = F)
# small p-value indicates poor model fit

# examine if smoke is significant
hours_nb1 <- update(hours_nb, . ~ . - work)
anova(hours_nb, hours_nb1)
# all terms are significant and their removal does not change the model

# for a negative binomial model
# - It is assumed that the response variable is over-dispersed and does not 
# have an excessive number of zeros

# a one unit increase in xx, would cause the difference in the logs of 
# expected counts would be expected to decrease by xx


#  ------------------------------------------------------------------------
# Fit a GAM to the dataset  -----------------------------------------------
#  ------------------------------------------------------------------------

# load the MGCV library
library(mgcv)

# fit a model to all the supplied variables
model_orig <- gam(cnt ~ season + yr + mnth + hr + holiday + weekday + 
                    weathersit + s(temp) + s(hum) + s(windspeed), data = hours1,
                  family = "poisson")
summary(model_orig)
# deviance explained 81.4%

# fit a model to the adjusted variables
model <- gam(cnt ~ season + yr + s(temp) +
               s(hum) + s(windspeed, k = 10) + work, data = hours1, family = "poisson")
summary(model)
# deviance explained 74.6%

# examine the plots of the model
par(mfrow = c(1,3))
plot(model, residuals = T)
par(mfrow = c(1,1))


#  ------------------------------------------------------------------------
# Diagnostic plots --------------------------------------------------------
#  ------------------------------------------------------------------------

# use the gam.check function to examine the diagnostics of the model
par(mfrow = c(2,2))
gam.check(model_orig)
par(mfrow = c(1,1))
# the residuals do not appear to be approx normal

# examine the standardized residuals
qqline(model_orig$residuals)

# checking the model assumptions using diagnostics plots
# plot the residuals vs fitted values plot
plot(fitted(model_orig), residuals(model_orig),xlab = "Predicted",
     ylab = "Residuals", main = "Residuals vs. Fitted Values" )
# the values appear to be scattered evenly above and below 0.


#  ------------------------------------------------------------------------
# Change the value of k ---------------------------------------------------
#  ------------------------------------------------------------------------

# change the value of k
hours_gam <- gam(cnt ~ season + yr + s(temp) +
                   s(hum) + s(windspeed, k = 10) + work, data = hours1, family = "poisson")
summary(hours_gam)

# create an empty vector for the R values
R_vals <- rep(0,28)

# create a for loop which increases the number of k
for (i in 3:20){
  R_vals[i-2] <- summary(gam(cnt ~ season + yr + s(temp) +
                               s(hum) + s(windspeed, k = i) + work, data = hours1, family = "poisson"))$r.sq
}

# plot the Rvals against their corresponding k value.
plot(c(3:20), R_vals[1:18], type = "l")

