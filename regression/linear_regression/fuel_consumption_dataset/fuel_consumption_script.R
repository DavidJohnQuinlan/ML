###########################################################################
#                                                                         #
#           Investigation of the fuel consumption dataset                 #
#                                                                         #
###########################################################################


#  ------------------------------------------------------------------------
# Investigation Information -----------------------------------------------
#  ------------------------------------------------------------------------

# The data used here were originally collected in a study of fuel consumption
# in the USA. Of primary interest is the relationship between the tax rate
# and fuel consumption, although various other explanatory variables were 
# also recorded for each of the 48 contiguous states of the USA in the year
# 1974. 

# The variables are:
#- Tax rate cents/gallon:
#- Income: mean income, in thousands of dollars
#- Roads: total length of roads, thousands of miles
#- Drivers: % of adult population with a drivers license
#- Fuel: fuel consumption, gallons/person

# Tasks:
# (1) Using informal and formal methods to build a regression model for feul
#     consumption and estimate the relationship between fuel consumption and 
#     tax rate.
# (2) Check the assumptions of the formal analysis you carry out.
# (3) Interpret your results, which is the most appropriate model and what is 
#     is the relationship between fuel consumption and tax rate? 
# (4) Check the fit of the model to see if it is adequate. If not, how could it 
#     be improved?


#  ------------------------------------------------------------------------
# Import the dataset ------------------------------------------------------
#  ------------------------------------------------------------------------

# set the current directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

fuel <- read.csv("./data/fuel_consumption.csv")

# convert to a dataframe:
fuel <- data.frame(fuel)


#  ------------------------------------------------------------------------
# Initial exploratory analysis --------------------------------------------
#  ------------------------------------------------------------------------

str(fuel)
summary(fuel)
head(fuel)
tail(fuel)
dim(fuel)


#  ------------------------------------------------------------------------
# Fit some plots to the dataset -------------------------------------------
#  ------------------------------------------------------------------------

#  ------------------------------------------------------------------------
# Boxplots ----------------------------------------------------------------
#  ------------------------------------------------------------------------

# plot boxplots of the dataset using ggplot
# load the necessary libraries
library(reshape)
library(ggplot2)
library(ggpubr, warn.conflicts = F)

# create a theme for the boxplots
boxplot_theme <- theme(axis.text.x=element_text(size=15, face = "bold"),
                       axis.title.x=element_blank(), 
                       axis.title.y = element_blank(),
                       plot.title = element_text(hjust = 0.5, size = 18, face = "bold"), 
                       legend.position="none",
                       axis.title=element_text(size=25, face="bold"),
                       text = element_text(size=20))


# create boxplots of the different continuous variables
Income_boxplot <- ggplot(fuel, aes(x="Income", y=Income)) + geom_boxplot(fill = "#F8766D") + boxplot_theme
Roads_boxplot <- ggplot(fuel, aes(x="Roads", y=Roads)) + geom_boxplot(fill = "#9EB9D4") + boxplot_theme
Drivers_boxplot <- ggplot(fuel, aes(x="Drivers", y=Drivers)) + geom_boxplot(fill = "Yellow") + boxplot_theme
Fuel_boxplot <- ggplot(fuel, aes(x="Fuel", y=Fuel)) + geom_boxplot(fill = "Light Green") + boxplot_theme
Tax_boxplot <- ggplot(fuel, aes(x="TaxRate", y=as.numeric(TaxRate))) + geom_boxplot(fill = "Purple") + boxplot_theme

# arrange the different boxplots into one page
ggarrange(Income_boxplot, Roads_boxplot, Drivers_boxplot, Fuel_boxplot, Tax_boxplot, ncol = 5, nrow = 1)
# there does not appear to be any outliers present in the income variable 
# with some possible outliers present in the other variables


#  ------------------------------------------------------------------------
# Histograms --------------------------------------------------------------
#  ------------------------------------------------------------------------

# plot the histogram of each variable
par(mfrow =c(1,5))
hist(fuel$Income, cex.main=2, 
     main = expression(paste("Income, ", mu, "=4.34, ", sigma, "=0.57")),
     xlab = "Income", col = "#F8766D",cex.lab = 1.5, cex.axis = 1.5)
hist(fuel$Roads, cex.main=2, 
     main = expression(paste("Roads, ", mu, "=5.56, ", sigma, "=3.49")),
     xlab = "Roads", col = "#9EB9D4",cex.lab = 1.5, cex.axis = 1.5)
hist(fuel$Drivers, cex.main=2, 
     main = expression(paste("Drivers, ", mu, "=57.03, ", sigma, "=5.55")),
     xlab = "Drivers", col = "Yellow",cex.lab = 1.5, cex.axis = 1.5)
hist(fuel$Fuel, cex.main=2, 
     main = expression(paste("Fuel, ", mu, "=576.77, ", sigma, "=111.89")),
     xlab = "Fuel", col = "Light Green",cex.lab = 1.5, cex.axis = 1.5)
hist((as.numeric(fuel$TaxRate)), cex.main=2, 
     main = expression(paste("Tax Rate, ", mu, "=7.67, ", sigma, "=0.95")),
     xlab = "Tax Rate", col = "Purple", cex.lab = 1.5, cex.axis = 1.5)
par(mfrow = c(1,1))
# all appear to be normally distributed


# Find the summary statistics for Income, Roads, Drivers and Fuel -----------------

# find the means of both variables
mean(fuel$Income)
mean(fuel$Roads)
mean(fuel$Drivers)
mean(fuel$Fuel)
mean(fuel$TaxRate)

# find the standard devation of both variables
sd(fuel$Income)
sd(fuel$Roads)
sd(fuel$Drivers)
sd(fuel$Fuel)
sd(fuel$TaxRate)


#  ------------------------------------------------------------------------
# Examine the relationship between Tax Rate and the other con var ---------
# -------------------------------------------------------------------------

# create a boxplot fuel consumption split by TaxRate
Fuel_box <- ggplot(data = fuel, aes(as.factor(TaxRate),Fuel)) + geom_boxplot() +
  labs(x = "Tax Rate") + ggtitle("Boxplots of fuel consumption split by tax rate") +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size=15),
        axis.title=element_text(size=14,face="bold"))

# output the fuel boxplot
Fuel_box
# the fuel consumption does appear to differ greatly for the different tax rates


#  ------------------------------------------------------------------------
# Scatterplots  -----------------------------------------------------------
#  ------------------------------------------------------------------------

# plot a scatterplot to the dataset
pairs(fuel)

# calculate the correlation matrix between each pair of variables
cor(fuel)
# high correlation between fuel and drivers


#  ------------------------------------------------------------------------
# Fit a linear regression model between fuel consumption ------------------
#  ------------------------------------------------------------------------

# fit a linear regression between fuel consumption and tax rate
fuel_lm <- lm(Fuel ~ TaxRate, data = fuel)
summary(fuel_lm)

# examine the diagnostic plots
par(mfrow = c(2,2))
plot(fuel_lm)
par(mfrow = c(1,1))
# the residuals appear to be approx normal 
# although there does seem to be some structure still present within the 
# residuals
# very low R2 value


#  ------------------------------------------------------------------------
# Fit a regression to the whole dataset -----------------------------------
#  ------------------------------------------------------------------------

# fit a linear regression model between fuel consumption and the rest of the 
# variables
fuel_lm_full <- lm(Fuel ~ ., data = fuel)
summary(fuel_lm_full)

# examine the diagnostic plots
par(mfrow = c(2,2))
plot(fuel_lm_full)
par(mfrow = c(1,1))

# remove the driver variable
fuel_lm_no_roads <- lm(Fuel ~ Income + TaxRate + Drivers, data = fuel)
summary(fuel_lm_no_roads)

# examine the diagnostic plots
par(mfrow = c(2,2))
plot(fuel_lm_no_roads)
par(mfrow = c(1,1))


#  ------------------------------------------------------------------------
# Examine potiential outliers ---------------------------------------------
#  ------------------------------------------------------------------------

# examine the dataset for possible outliers
# identify the observations with large cooks distance
cutoff <- 4/nrow(fuel) 
plot(fuel_lm_no_roads, which=4, cook.levels=cutoff)
abline(cutoff, 0, lty = 2)

# load the car library
library(car)

# I could also use the MASS libraries influential observations plot
influencePlot(fuel_lm_no_roads, main = "Infulence Plot")
# the influence plots identify 3 possible influential outliers

# examine the potential outliers
fuel[c(40, 19),]


# remove the 40th and 19th value from the dataset
fuel1 <- fuel[c(-40,-19),]


#  ------------------------------------------------------------------------
# Refit the model  --------------------------------------------------------
#  ------------------------------------------------------------------------

# remove the driver variable
fuel_lm_no_out <- lm(Fuel ~ Income + TaxRate + Drivers, data = fuel1)
summary(fuel_lm_no_out)

# examine the diagnostic plots
par(mfrow = c(2,2))
plot(fuel_lm_no_out)
par(mfrow = c(1,1))

# output a table of the parameter values using stargazer
# load the stargazer library
library(stargazer)

# create the stargazer table
stargazer(fuel_lm_no_out)
