############################################################################
#                                                                          #
#                         KBB Automobile Dataset                           #
#                                                                          #
############################################################################

# set the current directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#  ------------------------------------------------------------------------
# Import the car dataset --------------------------------------------------
#  ------------------------------------------------------------------------

car <- read.csv("./data/car_data.csv")
head(car)


#  ------------------------------------------------------------------------
# Examine the car dataset -------------------------------------------------
#  ------------------------------------------------------------------------

head(car)
tail(car)
dim(car)
str(car)
summary(car)


#  ------------------------------------------------------------------------
# Prepair and clean the data ----------------------------------------------
#  ------------------------------------------------------------------------

# convert all categorical variables into factor variables
car$Make <- as.factor(car$Make)
car$Model <- as.factor(car$Model)
car$Trim <- as.factor(car$Trim)
car$Type <- as.factor(car$Type)
car$Cylinder <- as.factor(car$Cylinder) 
car$Doors <- as.factor(car$Doors)
car$Cruise <- as.factor(car$Cruise)
car$Sound <- as.factor(car$Sound)
car$Leather <- as.factor(car$Leather)


#  ------------------------------------------------------------------------
# Examine the variables in more detail ------------------------------------
#  ------------------------------------------------------------------------

# create a pairs plot of all the variables
pairs(car)
# from this graph we can see that we should investigate door-type and 
# liter-cylinder

# create a table of the number of doors against the type of the car
table(car$Doors, car$Type)
# we can see that these basically describe the same thing since no column 
# contains more than one value, therefore, one of these variables can be removed 

# create a table of the car trim against the car type
table(car$Trim, car$Type)
# again we can see how this information is also represented within the car 
# type variable

# create a table of the car model and the car make
table(car$Model, car$Make)
# again we can see how the model information is already represented within the 
# make variable

# visually demonstrate the above tables
plot(car$Type, car$Doors, col=c("red", "blue"))
legend("topright", fill=c("red", "blue"), col=c("red", "blue"), 
       legend=c("2 doors", "4 doors"), cex=1)
# doors information is included in the type car

# create a plot of Liter split by cylinder
plot(car$Cylinder, car$Liter, xlab="Cylinder", ylab="Liter")
# clearly liter is a more detailed variable than cylinder which
# describe the engine

# Create table plots to show why we remove Cylinder and Doors -------------

# create a plot of the type against the number of doors
plot(car$Type, car$Doors, col=c("Light Blue", "firebrick"), 
     xlab = "Type", ylab = "Doors", 
     main = "Plot of car Types against the Number of car Doors",
     cex.axis = 1.5, cex.lab = 2)
legend("topright", fill=c("Light Blue", "Firebrick"), 
       legend=c("2 doors", "4 doors"), cex=1)

# load the ggplot library
library(ggplot2)

# create a plot of the number of cylinders against the liter size of the car engine
ggplot(data = car, aes(as.factor(Cylinder), Liter)) + geom_boxplot() +
  labs(x = "Cylinder") + 
  ggtitle("Boxplots of Liter split by the number of Cylinders") +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size=15),
        axis.title=element_text(size=14, face="bold"))

# create a table of the liter against the cylinder
table(car$Liter, car$Cylinder)
# we can see how the cylinder information is already included in the liter 
# variable

# remove model, trim, doors and cylinder variables from the dataset
car_new <- car[,c(-4,-5,-7,-9)]


#  ------------------------------------------------------------------------
# Create some plots of the data -------------------------------------------
#  ------------------------------------------------------------------------

#  ------------------------------------------------------------------------
# Create some boxplots ----------------------------------------------------
#  ------------------------------------------------------------------------

# create some simple boxplots
par(mfrow = c(1, 3), mar = c(2,3,2,0.25))
boxplot(car$Price, main = "Price")
boxplot(car$Mileage, main = "Milage")
boxplot(car$Liter, main = "Liter")
par(mfrow = c(1,1))
# there are some possible outliers present within the Price and Mileage 
# variables

# create a nicer boxplot using the libraries reshape and ggplot
# load the necessary libraries
library(reshape)
library(ggplot2)
library(ggpubr)

# create a theme for the boxplots
car_theme <- theme(axis.text.x=element_text(size=15, face = "bold"), 
                   axis.title.x=element_blank(), axis.title.y = element_blank(),
                   plot.title = element_text(hjust = 0.5,size = 18, face = "bold"), 
                   legend.position="none", 
                   axis.title=element_text(size=25, face="bold"), 
                   text = element_text(size=20))

# create boxplots of the different variables and then put them together
Price_boxplot <- ggplot(car, aes(x="Price", y=Price)) + geom_boxplot(fill = "#F8766D") + car_theme
Mileage_boxplot <- ggplot(car, aes(x="Mileage", y=Mileage)) + geom_boxplot(fill = "#9EB9D4") + car_theme
Liter_boxplot <- ggplot(car, aes(x="Liter", y=Liter)) + geom_boxplot(fill = "yellow") + car_theme

# arrange the different boxplots into one page
ggarrange(Price_boxplot, Mileage_boxplot, Liter_boxplot, 
          ncol = 3, nrow = 1)


#  ------------------------------------------------------------------------
# Create some histograms --------------------------------------------------
#  ------------------------------------------------------------------------

# plot the histogram of each variable
par(mfrow =c(1,3))
hist((car$Price), cex.main=2.5, 
     main = expression(paste("Price, ", mu, "=21343.14, ", sigma, "=9884.85")),
     xlab = "Price", col = "#F8766D", cex.lab = 1.5, cex.axis = 1.5)

hist(car$Mileage, cex.main=2.5, 
     main = expression(paste("Milage, ", mu, "=19831.93, ", sigma, "=8196.32")),
     xlab = "Milage", col = "#9EB9D4", cex.lab = 1.5, cex.axis = 1.5)

hist(car$Liter, cex.main=2.5, 
     main = expression(paste("Liter, ", mu, "=3.04, ", sigma, "=1.11")),
     xlab = "Liter", col = "Yellow", cex.lab = 1.5, cex.axis = 1.5)
par(mfrow = c(1,1))

# Find the summary statistics for Price, Liter and Mileage --------------------

# find the means of both variables
mean(car$Price)
mean(car$Mileage)
mean(car$Liter)

# find the standard deviation of both variables
sd(car$Price)
sd(car$Mileage)
sd(car$Liter)

# find the median value of the price variable
median(car$Price)

#  ------------------------------------------------------------------------
# Plot a histogram of the log transformed price variable -----------------
#  ------------------------------------------------------------------------

hist(log(car$Price), cex.main=2.5,
     main = expression(paste("Log of Price, ", mu, "=9.88, ", sigma, "=0.41")),
     xlab = "Price", col = "#F8766D")
par(mfrow = c(1,1))

# with summary statistics for the log data given as
mean(log(car$Price))
sd(log(car$Price))


#  ------------------------------------------------------------------------
# Create the price histograms ---------------------------------------------
#  ------------------------------------------------------------------------

# plot the histogram of the price and log of price together
par(mfrow =c(1,2))
hist((car$Price), cex.main=2.5, 
     main = expression(paste("Price, ", mu, "=21343.14, ", sigma, "=9884.85"))
     ,xlab = "Price", col = "#F8766D", cex.lab = 1.5, cex.axis = 1.5)
hist(log(car$Price), cex.main=2.5,
     main = expression(paste("Log of Price, ", mu, "=9.88, ", sigma, "=0.41")),
     xlab = "log(Price)", col = "#9EB9D4", cex.lab = 1.5, cex.axis = 1.5)
par(mfrow = c(1,1))


#  ------------------------------------------------------------------------
# Create some frequency plots of the different varaibles ------------------
#  ------------------------------------------------------------------------

# load the necessary plotting libraries
library(sjPlot)
library(sjmisc)

# create a frequency bar chart for each of the variables
plot_frq(car$Make, title="Frequency of the different car Makes", sort.frq = "des")
plot_frq(car$Model, title="Frequency of the different car Models", sort.frq = "des")
plot_frq(car$Trim, title="Frequency of the different car Trim", sort.frq = "des")
plot_frq(car$Type, title="Frequency of the different car Types", sort.frq = "des")
plot_frq(car$Cylinder, title="Frequency of the different car Cylinders", sort.frq = "des")
plot_frq(car$Doors, title="Frequency of the different car Doors", sort.frq = "des")
plot_frq(car$Cruise, title="Frequency of the Cruise", sort.frq = "des")
plot_frq(car$Sound, title="Frequency of the Sounds", sort.frq = "des")
plot_frq(car$Leather, title="Frequency of Leather Seats ", sort.frq = "des")


#  ------------------------------------------------------------------------
# Examine the different categorical variables agasint the price -----------
#  ------------------------------------------------------------------------

# create a boxplot of the different prices split by car make
make_box <- ggplot(data = car, aes(as.factor(Make),Price)) + geom_boxplot() +
  labs(x = "Make") + ggtitle("Boxplots of Price split by car Make") +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size=15),
        axis.title=element_text(size=14,face="bold"))
# this shows how the Cadillac seem to have a larger price then the other cars

# create a boxplot of the different prices split by car type:
type_box <- ggplot(data = car, aes(as.factor(Type),Price)) + geom_boxplot() +
labs(x = "Type") + ggtitle("Boxplots of Price split by car Type") +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size=15),
        axis.title=element_text(size=14,face="bold"))
# convertibles seem to cost more then the other types of cars

# create a boxplot of the different prices split by if the car has cruise or not
cruise_box <- ggplot(data = car, aes(as.factor(Cruise),Price)) + geom_boxplot() +
  labs(x = "Cruise") + ggtitle("Boxplots of Price split by car Cruise") +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size=15),
        axis.title=element_text(size=14, face="bold"))
# more expensive Cars seem to generally have cruise

# create a boxplot of the different prices split by if the car has
sound_box <- ggplot(data = car, aes(as.factor(Sound), Price)) + geom_boxplot() +
  labs(x = "Sound") + ggtitle("Boxplots of Price split by Sound") +
  theme(plot.title = element_text(hjust = 0.5), axis.title=element_text(size=14, face="bold"))

# create a boxplot of the different prices split by if the car has leather seats or not
leather_box <- ggplot(data = car, aes(as.factor(Leather),Price)) + geom_boxplot() +
  labs(x = "Leather") + ggtitle("Boxplots of Price split by Leather") +
  theme(plot.title = element_text(hjust = 0.5), axis.title=element_text(size=14, face="bold"))
# more cars seem to have leather seats however the mean price for both is about the
# same

# merge some of the plots together
ggarrange(make_box, type_box, ncol = 2, nrow = 1)


#  ------------------------------------------------------------------------
# Fit a model to the data -------------------------------------------------
#  ------------------------------------------------------------------------

# fit a model to the car dataset 
car_model= lm(Price ~ ., car_new)
summary(car_model)

# examine the residuals of the model
par(mfrow = c(2,2))
plot(car_model)
par(mfrow = c(1,1))

# examine the box-cox for the model
# load the MASS library
library(MASS)

# use the box-cox function
boxcox(car_model)
# this would indicate that the response variable should be log transformed


#  ------------------------------------------------------------------------
# Fit a model to the log response -----------------------------------------
#  ------------------------------------------------------------------------

# fit the model
log_car_model <- lm(log(Price) ~ ., data = car_new)
summary(log_car_model)

# examine the residuals of the model
par(mfrow = c(2,2))
plot(log_car_model)
par(mfrow = c(1,1))

#  ------------------------------------------------------------------------
# Perform a step-wise regression using AIC --------------------------------
#  ------------------------------------------------------------------------

# perform a step-wise regression to reduce the model
# create a null model
car_null_model <- lm(log(Price) ~ 1, data = car_new)
summary(car_null_model)

# examine the data using both directional step-wise regression
car_both_dir <-step(car_null_model, 
                    scope=list(lower=formula(car_null_model), 
                               upper=formula(log_car_model)), 
                    direction="both")

# the final model is defined as
formula(car_both_dir)

# examine this model in more detail
summary(car_both_dir)


#  ------------------------------------------------------------------------
# Use step-wise regression using BIC --------------------------------------
#  ------------------------------------------------------------------------

# load the RcmdrMisc library
library(RcmdrMisc)

# run the step-wise regression using BIC
car_BIC_mod <- stepwise(log_car_model, criterion = "BIC", "backward/forward")

# examine the summary of BIC model
summary(car_BIC_mod)

# print the formula of the BIC model
formula(car_BIC_mod)

# examine the residuals of this model
par(mfrow = c(2,2))
plot(car_BIC_mod)
par(mfrow = c(1,1))


#  ------------------------------------------------------------------------
# Interpretation of the coefficients --------------------------------------
#  ------------------------------------------------------------------------

# store the coefficient values
a = car_BIC_mod$coefficients
a = as.numeric(a)

# interpretation of the Intercept
cat("The intercept coefficient mean that if we take a car with 0 mileage, 
    liter capacity 0, who makes the car is Buick and the type is Convertible 
    then the price would be: ", exp(a[1]))

# interpretation for mileage
cat("When we increase by one-unit the variable Mileage and all the other 
     variables are held constant then the average price would decrease of 
     0.000823%.")

# interpretation of the coefficient for variable make
cat("When all the other variables are held constant and the car is Cadillac - 
     chevrolet - pontiac - SAAB - Saturn than the average price would increase 
     of ", (round((exp(a[3])-1),4)*100), "% - 
     decrease of ", (round((1-exp(a[4])),4)*100), "% - 
     decrease of ", (round((1-exp(a[5])),4)*100), "% - 
     increase of ", (round((exp(a[6])-1),4)*100), "% - decrease
     of ", (round((1-exp(a[7])),4)*100), "% respectively" )

# interpretation of the coefficient for variable type
cat("When all the other variables are held constant and the model of the car is
     Coupe - Hatchback - Sedan  - Wagon than the average price would decrease by ", 
    (round((1-exp(a[8])),4)*100), "%,", (round((1-exp(a[9])),4)*100), "%,",
    (round((1-exp(a[10])),4)*100), "%,", (round((1-exp(a[11])),4)*100), "% 
    respectively")

# interpretation of the coefficient for variable liter
cat("When we increase by one-unit the variable Liter and all the other variables
     are held constant then the average price would increase of ", 
     (round((exp(a[12])-1),4)*100),"%.")

