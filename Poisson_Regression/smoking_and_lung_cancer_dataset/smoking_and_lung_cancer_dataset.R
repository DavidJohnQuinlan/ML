###########################################################################
#                                                                         #
#         Investigation of the Smoking and Lung Cancer Dataset            #
#                                                                         #
###########################################################################

#  ------------------------------------------------------------------------
# Smoking and Lungs -------------------------------------------------------
#  ------------------------------------------------------------------------

#  ------------------------------------------------------------------------
# Some details of the dataset ---------------------------------------------
#  ------------------------------------------------------------------------

# The dataset comprises of 4 columns:
#- Age: In five year groups (9 different groups)
#- Smoking Status: (people who do not smoke, smoke cigars or pipes only)
#- Population: in hundreds of thousands
#- Deaths: number of lung cancer deaths per year

# the aim of this analysis is to estimate if there is an effect of smoking 
# on the number of deaths. 


#  ------------------------------------------------------------------------
# Initial exploratory analysis --------------------------------------------
#  ------------------------------------------------------------------------

# set the current directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read in the lungs dataset
smoking <- read.csv("./data/smoking_data.csv")

# examine the lungs dataset
head(smoking)
tail(smoking)
str(smoking)
summary(smoking)


#  ------------------------------------------------------------------------
# Fix an of the errors present within the dataset -------------------------
#  ------------------------------------------------------------------------

# from examining the summary it appears that there is an issue with the
# one of the categories of age, I should change this from 45-59 to 45-49.

# examine the different levels contained within a variable
levels(smoking$age)

# set the smoking age to be a character variable
smoking$age <- as.character(smoking$age)
# this adds a space for some reason to the character variable

# remove the extra space
smoking$age <- trimws(smoking$age)

# change the specific rowname that is incorrect
for (i in 1:nrow(smoking)){
  if (smoking$age[i] == "45-59"){
    smoking$age[i] = "45-49"
  }
}

# this code will also work
# smoking$age[smoking$age == "45-59"] <- "45-49"

# this will also work
# levels(smoking$age)[levels(smoking$age) =="45-59"] <- "45-49"
# the issue was that the variable was not a character variable

# examine the smoking dataset to ensure that this was done correctly
smoking$age

# convert these into character variable
smoking$age <- as.factor(smoking$age)


#  ------------------------------------------------------------------------
# Adding a death rate variable --------------------------------------------
#  ------------------------------------------------------------------------

# given that the number of deaths and the population are both supplied the 
# rate of deaths can also be calculated. 

# it might actually make more sense to model the rate of deaths against the 
# smoking status and the age of a population.

# create a new death_rate variable
smoking$death_rates <- smoking$dead/smoking$pop


#  ------------------------------------------------------------------------
# Create some plots of the data -------------------------------------------
#  ------------------------------------------------------------------------

#  ------------------------------------------------------------------------
# Create some boxplots ----------------------------------------------------
#  ------------------------------------------------------------------------

# create some simple boxplots
par(mfrow = c(1,3), mar = c(2,3,2,0.25))
boxplot(smoking$pop, main = "Population")
boxplot(smoking$dead, main = "Dead")
boxplot(smoking$death_rates, main = "Death Rates")
par(mfrow = c(1,1))
# there are some possible outliers present within the population and dead
# variables

# create a nicer boxplot using the libraries reshape and ggplot
# load the necessary libraries
library(reshape)
library(ggplot2)
library(ggpubr, warn.conflicts = F)

# create a theme for the boxplots
smoking_theme <- theme(axis.text.x=element_text(size=15, face = "bold"),
                       axis.title.x=element_blank(), axis.title.y = element_blank(),
                       plot.title = element_text(hjust = 0.5, size = 18, face = "bold"), legend.position="none",
                       axis.title=element_text(size=25, face="bold"),
                       text = element_text(size=20))

# create boxplots of the different variables and then put them together
Pop_boxplot <- ggplot(smoking, aes(x="Population", y=pop)) + geom_boxplot(fill = "#F8766D") + smoking_theme
Dead_boxplot <- ggplot(smoking, aes(x="Deaths", y=dead)) + geom_boxplot(fill = "#9EB9D4") + smoking_theme

# arrange the different boxplots into one page
ggarrange(Pop_boxplot, Dead_boxplot, ncol = 2, nrow = 1)

# find the median values for each age group
aggregate(smoking$death_rates, list(smoking$age), median)


#  ------------------------------------------------------------------------
# Create some histograms --------------------------------------------------
#  ------------------------------------------------------------------------

# plot the histogram of each variable
par(mfrow =c(1,2))
hist((smoking$pop), cex.main=2.5, 
     main = expression(paste("Population, ", mu, "=1558.94, ", sigma, "=1562.23")),
     xlab = "Population", col = "#F8766D", cex.lab = 1.5, cex.axis = 1.5)
hist(smoking$dead, cex.main=2.5, 
     main = expression(paste("Deaths, ", mu, "=253.61, ", sigma, "=262.60")),
     xlab = "Deaths", col = "#9EB9D4", cex.lab = 1.5, cex.axis = 1.5)
par(mfrow = c(1,1))


# Find the summary statistics for Dead and Population ---------------------

# find the means of both variables
mean(smoking$pop)
mean(smoking$dead)

# find the standard devation of both variables
sd(smoking$pop)
sd(smoking$dead)

# find the min of the population variable
min(smoking$pop)

# find the min and max of the number of deaths
min(smoking$dead)
max(smoking$dead)


#  ------------------------------------------------------------------------
# Create a correlation matrix ---------------------------------------------
#  ------------------------------------------------------------------------

# create a correlation matrix
pairs(smoking[, c(3,4,5)], cex.labels = 2.5)

# calculate the correlation between each variable
cor(smoking[, c(3,4,5)])


#  ------------------------------------------------------------------------
# Boxplots of the number of deaths against explanatory variables ----------
#  ------------------------------------------------------------------------

# there also appears to be some whitespaces present within the smoking status 
# variable

# change the level names
levels(smoking$smoke) <- c("NoSmoking", "CigarPipeOnly", "CigarretteOnly", "CigarrettePlus")

# create a boxplot of the number of deaths against the different age levels
Age_boxplot <- ggplot(smoking, aes(x=age, y=death_rates, fill=age)) + geom_boxplot() + smoking_theme
Smoking_status_boxplot <- ggplot(smoking, aes(x=smoke, y=death_rates, fill=smoke)) + geom_boxplot() + smoking_theme

# arrange the different boxplots into one page
ggarrange(Age_boxplot, Smoking_status_boxplot, ncol = 2, nrow = 1)

# find the median values of the different age group death rates
aggregate(smoking$death_rates, list(smoking$age), median)


#  ------------------------------------------------------------------------
# Fit a model to the dataset ----------------------------------------------
#  ------------------------------------------------------------------------

#  ------------------------------------------------------------------------
# Fit a Poisson model -----------------------------------------------------
#  ------------------------------------------------------------------------

# interest lies in estimating if there is an effect of smoking on the number
# of deaths. Fit a regression model

# since we are modeling the counts of the number of deaths, a Poisson regression 
# model might be a good place to start

# fit a Pisson regression model between the response deaths and the explanatory vars
smoking_pois <- glm(dead ~ ., family = poisson, data = smoking)
summary(smoking_pois)

# plot the models diagnostics
par(mfrow = c(2,2))
plot(smoking_pois)
par(mfrow = c(1,1))

# all of the parameters seem to be well fitted with very small p-values
# check to see if the Poisson model expectations hold
# the Poisson model assumptions are that the mean and variance of the model
# are equal.

# examine the deviance of the model
pchisq(deviance(smoking_pois), df.residual(smoking_pois), lower.tail = F)
# the low value indicates how there is evidence of lack of fit of the model

# load the AER library
library(AER)

# Run an overdispersion test on the Poisson model fitted above
dispersiontest(smoking_pois, trafo = 1)
# the very small p-value indicates how we reject the null hypothesis and conclude
# that there is evidence to suggest that there is a presence of overdispersion 
# within the Poisson model. 

# a deeper insight into the model can also be obtained by looking at the
# residuals. one such type of residuals are the Pearsons residuals.
# Pearson residuals fluctuate around zero following approx. a normal distribution
plot(fitted(smoking_pois), residuals(smoking_pois, "pearson"))
title("Pearson Residual plot from Poisson Regression")
abline(h = 0)
abline(h = 2, lty = 2)
abline(h = -2, lty = 2)
# this residual plot has many deviance residuals which are greater then 2
# since the residuals have been standardized in some way i.e. see definition 
# of the Pearson residuals, values lying outside +-2 or +-3 are potential 
# outliers. from the above model is appears that there are many possible outliers

# another type of residual is the deviance residual, which should also
# approximately have a standard normal distribution. 
plot(fitted(smoking_pois), residuals(smoking_pois, type = "deviance"))
title("Deviance Residual plot from Poisson Regression")
abline(h = 0)
abline(h = 2, lty = 2)
abline(h = -2, lty = 2)
# both of the above plots do not look good. 


# -------------------------------------------------------------------------
# Fit a quasi-Poisson model -----------------------------------------------
# -------------------------------------------------------------------------

# given that above model is overdispearsed, one way of dealing with 
# overdispersion is to use a quasi-Poisson model as this model has an extra
# parameter which accounts for the overdispearsion.

# fit a quasi-Poisson model to the dataset
smoking_quasi <- glm(dead ~ ., family = quasipoisson, data = smoking)
summary(smoking_quasi)
# this time not all the parameters are significant

# examine the diagnostics
par(mfrow = c(2,2))
plot(smoking_quasi)
par(mfrow = c(1,1))


# -------------------------------------------------------------------------
# Fit a negative binomial model -------------------------------------------
# -------------------------------------------------------------------------

# load the MASS library
library(MASS)

# just to provide another model other then the quasi-Poisson model
smoking_nb <- glm.nb(dead ~ ., data = smoking)
summary(smoking_nb)
# the two models mostly agree

# examine the diagnostics
par(mfrow = c(2,2))
plot(smoking_nb)
par(mfrow = c(1,1))

# Pearsons residuals
plot(fitted(smoking_nb), residuals(smoking_nb, "pearson"))
title("Pearson Residual plot from Poisson Regression")
abline(h = 0)
abline(h = 2, lty = 2)
abline(h = -2, lty = 2)

# deviance residuals
plot(fitted(smoking_nb), residuals(smoking_nb, type = "deviance"))
title("Deviance Residual plot from Poisson Regression")
abline(h = 0)
abline(h = 2, lty = 2)
abline(h = -2, lty = 2)
# these look much better then the quasi-Poisson residuals plots since they 
# now appear to be more approximately normally distributed around 0

# -------------------------------------------------------------------------
# Fit a linear model ------------------------------------------------------
# -------------------------------------------------------------------------

# fit a linear model to the dataset
smoking_lm <- lm(dead ~ ., data = smoking)
summary(smoking_lm)

# examine the diagnostic plots
par(mfrow = c(2,2))
plot(smoking_lm)
par(mfrow = c(1,1))

# there is a clear pattern present within the dataset, almost like a quadratic 
# curve, it is also present within the scale location plot indicating a 
# non-constant variance

# use a box-cox test to examine how best to change the response variable
boxcox(smoking_lm)
# the box-cox model is close to 0 which indicates a log transformation to the 
# response variable deaths

# fit a new model of the log of deaths against the explanatory variables
smoking_lm_log <- lm(log(dead) ~ ., data = smoking)
summary(smoking_lm_log)

# Examine the residual plots
par(mfrow = c(2,2))
plot(smoking_lm_log)
par(mfrow = c(1,1))
# there is still an issue given that there still appears to be a pattern 
# present in the data, I believe that the best method is to further examine 
# the quasi-Poisson and negative binomial models


# -------------------------------------------------------------------------
# Fitting a Poisson regression model with an offset -----------------------
# -------------------------------------------------------------------------

# fit a Poisson offset model
smoking_pois_offset <- glm(dead ~ age + smoke, offset = (log(pop)), data = smoking, family = poisson)
summary(smoking_pois_offset)

# examine the diagnostic plots of the model
par(mfrow = c(2,2))
plot(smoking_pois_offset)
par(mfrow = c(1,1))

# test the significance of the dispersion parameter
dispersiontest(smoking_pois_offset, trafo = 1)
# we fail to reject the null hypothesis and conclude that there is insufficient 
# evidence that there is insufficient evidence to suggest that the Poisson 
# regression model is under/overdispearsed

# calculate the deviance goodness of fit test
pchisq(smoking_pois_offset$deviance, df=smoking_pois_offset$df.residual, lower.tail=FALSE)
# fail to reject the null hypothesis and can therefore conclude that there is 
# insufficient evidence to suggest a lack of fit of the model to the data. 

# fit the null model
smoking_pois_offset_null <- glm(dead ~ 1, offset = (log(pop)), data = smoking, family = poisson)

# use the likelihood ratio test to compare the full and null models
lrtest(smoking_pois_offset, smoking_pois_offset_null)
# we reject the null hypothesis that the reduced model is a better fit to the 
# dataset than the full model

# use the likelihood ratio test to see if the reduced models would be better
# fit the age model
smoking_pois_offset_age <- glm(dead ~ age, offset = (log(pop)), data = smoking, family = poisson)

# fit the smoking status model
smoking_pois_offset_smoking_status <- glm(dead ~ smoke, offset = (log(pop)), data = smoking, family = poisson)

# compare smoking status against the full model
lrtest(smoking_pois_offset, smoking_pois_offset_smoking_status)

# compare age against the full model
lrtest(smoking_pois_offset, smoking_pois_offset_age)
# both of these obtained a very small p-value indicating how the full model is 
# a better fit to the dataset as we reject the null hypothesis that the reduced
# model is a better fit to the dataset

# I will also use stepwise regression to see if one of the variables should be 
# removed
library(MASS)
stepAIC(smoking_pois_offset, direction = "both")

# examine the Pearson residuals
# Pearson residuals fluctuate around zero following approx a normal distribution
plot(fitted(smoking_pois_offset), residuals(smoking_pois_offset, "pearson"))
title("Pearson Residual plot from Poisson Regression")
abline(h = 0)
abline(h = 2, lty = 2)
abline(h = -2, lty = 2)

# another type of residual is the deviance residual, which should also
# approximately have a standard normal distribution. 
plot(fitted(smoking_pois_offset), residuals(smoking_pois_offset, type = "deviance"))
title("Deviance Residual plot from Poisson Regression")
abline(h = 0)
abline(h = 2, lty = 2)
abline(h = -2, lty = 2)


# -------------------------------------------------------------------------
# Fitting a quasi-Poisson regression model with an offset -----------------
# -------------------------------------------------------------------------

# fit a quasi-Poisson regression model to the dataset with an offset on population
smoking_quasi_offset <- glm(dead ~ age + smoke, offset = log(pop), data = smoking, family = quasipoisson)
summary(smoking_quasi_offset)

# examine the diagnostics
par(mfrow = c(2,2))
plot(smoking_quasi_offset)
par(mfrow = c(1,1))

# calculate the deviance goodness of fit
pchisq(deviance(smoking_quasi_offset), df.residual(smoking_quasi_offset), lower.tail = F)

# examine the Pearson residuals
plot(fitted(smoking_quasi_offset), residuals(smoking_quasi_offset, "pearson"))
title("Pearson Residual plot from Poisson Regression")
abline(h = 0)
abline(h = 2, lty = 2)
abline(h = -2, lty = 2)

# deviance residuals
plot(fitted(smoking_quasi_offset), residuals(smoking_quasi_offset, type = "deviance"))
title("Deviance Residual plot from Poisson Regression")
abline(h = 0)
abline(h = 2, lty = 2)
abline(h = -2, lty = 2)
