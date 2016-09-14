# ---- Description ----
# This is part IV of the code along
# script for the September 14th R 
# Workshop at University of Maine,
# presented by Dr. Trevor Avery and
# Danielle Quinn.
# This code along is specifically for
# the modelling portion of the workshop.
# Enjoy!

# ---- Exercise 3.1 ----
# Set up the next several sections

# ---- Set Working Directory ----
setwd("C:/Users/danie/Desktop/Maine")

# ---- Load Packages ----
library(ggplot2)
library(MASS)

# ---- Source Custom Functions ----
# This allows you to run other pieces of
# code as separate pieces; here we are
# running code that sets up some functions
# that have been customized for this workshop
source("HighStatLib.R") # Highland Statistics Ltd.
source("MoreFunctions.R")

# ---- Import Data ----
fish<-read.delim("Baileyetal2008_modified.txt")

# Check data structure
str(fish)
summary(fish)
head(fish)
dim(fish)
names(fish)

# ---- Data Manipulation ----
# Before we begin the analysis, we want to
# make a few minor changes to the data

# Convert depth to km
fish$depth_km<-fish$depth_m/1000

# ---- Research Question ----
# What is the abundance-depth relationship?

# ---- Basic Visualization ----
# Visualize what exactly you're trying to model
baseline<-ggplot(fish)+
  geom_point(aes(x=depth_km, y=abundance))+
  theme_bw(15)+xlab("Depth (km)")+ylab("Abundance")
baseline

# ---- Linear Model: Set Up ----
# Run a simple linear model
m0<-lm(abundance~depth_km, data=fish)
m0 # Model output
summary(m0) # Summarize model output

# ---- Linear Model: Diagnostics ----
## Diagnostics 1: Residuals vs Fitted Values ##
# Create data frame of residuals and fitted values
diag.m0<-data.frame(residuals=resid(m0), fitted=fitted(m0))

# Visualize fitted values vs residuals
ggplot(diag.m0)+
  geom_point(aes(x=fitted, y=residuals), size=3)+
  geom_hline(yintercept=0, linetype="dashed", col="blue")+
  theme_bw(16)+ylab("Residuals")+xlab("Fitted Values)")

# We see a pattern (heterogeneity)! No good.
# We could stop there and say that the model isn't appropriate,
# but let's continue with the diagnostic process

## Diagnostics 2: Dispersion ##
# Should be close to 1
dispersion(m0) # Nope!

## Diagnostics 3: Additional Diagnostic Plots ##
# Use {base} plot to quickly generate (poor quality)
# model diagnostic plots
plot(m0) # Click in console and hit Enter to see each plot
# 1. Residuals vs Fitted Plot (same as above)
  # Want to see randomness
# 2. Q-Q Plot
  # Want to see points fall on the line with minimal "tails"
# 3. Scale-Location Plot
  # Want to see randomness
# 4. Residuals vs Leverage
  # Want to see randomness

# Despite the fact that we know this is a terrible model,
# let's proceed with plotting the fitted line

# ---- Linear Model: Fitted Line ----
# Step 1: Create a data frame with independent variable (depth_km) 
output.m0<-data.frame(depth_km=c(1,2,3,4,5))
# Step 2: Populate a column of predictions based on the
# model m0 and the values given by our output.m0 data frame
output.m0$predicted<-predict(m0, output.m0)

# Visualize the fitted model line
baseline+
  geom_line(aes(x=depth_km, y=predicted), data=output.m0, size=1)

# We see that at high depths we get a negative density predicted!
# Add a zero line to make that clear
baseline+
  geom_line(aes(x=depth_km, y=predicted), data=output.m0, size=1)+
  geom_hline(yintercept=0, size=1, linetype='dashed', col="blue")

# Let's use the predicted standard error
# around the model to randomly generate some
# potential estimates of abundance
depths<-c(1:5)
intercept<-coef(m0)[1] # intercept given by m0
slope<-coef(m0)[2] # slope given by m0
sigma<-summary(m0)$sigma # standard error given by m0
depth_km<-c()
estimates<-c()
for(i in depths)
{
  depth_km.in<-rep(i, 100)
  depth_km<-c(depth_km, depth_km.in)
  mu<-intercept+slope*i
  estimates.in<-rnorm(100, mean=mu, sd=sigma)
  estimates<-c(estimates, estimates.in)
}
randompoints<-data.frame(depth_km, estimates)

# Visualize our randomly generated potential estimates of abundance
baseline+
  geom_line(aes(x=depth_km, y=predicted), data=output.m0, size=1)+
  geom_point(aes(x=depth_km, y=estimates), col='grey50', alpha=0.5, data=randompoints)+
  geom_hline(yintercept=0, size=1, linetype='dashed', col="blue")

# ---- Linear Model: Conclusions ----
# So, based on the:
# (a) heterogeneity of the residuals,
# (b) overdispersion,
# (c) other diagnositc plots,
# (d) fitted line, and 
# (e) potential predictions,
# we can see that this model sucks.

# ---- Poisson GLM: Set Up ----
# One of the big issues we're having is that our dependent variable
# (abundance) can't go below zero; we can't have a negative number of fish

# Run a generalized linear model (GLM) with a Poisson distribution
m1<-glm(abundance~depth_km, data=fish, family="poisson")
m1 # Model output
summary(m1) # Summarize model output

# ---- Poisson GLM: Diagnostics ----
## Diagnostics 1: Residuals vs Fitted Values ##
# Create data frame of residuals and fitted values
diag.m1<-data.frame(residuals=resid(m1), fitted=fitted(m1))

# Visualize fitted values vs residuals
ggplot(diag.m1)+
  geom_point(aes(x=fitted, y=residuals), size=3)+
  geom_hline(yintercept=0, linetype="dashed", col="blue")+
  theme_bw(16)+ylab("Residuals")+xlab("Fitted Values)")

## Diagnostics 2: Dispersion ##
dispersion(m1, modeltype="poisson") # Better, but still bad

# ---- Poisson GLM: Fitted Line ----
# Step 1: Create a data frame with independent variable (depth_km) 
output.m1<-data.frame(depth_km=seq(from=min(fish$depth_km),
                                   to=max(fish$depth_km),
                                   length=20))
# Step 2: Populate a column of predictions based on the
# model m1 and the values given by our output.m1 data frame
output.m1$predicted<-predict(m1, output.m1, type='response')

# Visualize the fitted model line
baseline+
  geom_line(aes(x=depth_km, y=predicted), data=output.m1, size=1)

# Better! Using a Poisson distribution ensures that predicted values don't fall below 0

# To better understand what overdispersion means, let's generate
# potential estimates using the m1 model
depths<-seq(from=min(fish$depth_km),
            to=max(fish$depth_km),
            length=20)
intercept<-coef(m1)[1] # intercept given by m1
slope<-coef(m1)[2] # slope given by m1
depth_km<-c()
estimates<-c()
for(i in depths)
{
  depth_km.in<-rep(i, 100)
  depth_km<-c(depth_km, depth_km.in)
  mu<-exp(intercept+slope*i) # link function
  estimates.in<-rpois(100, lambda=mu)
  estimates<-c(estimates, estimates.in)
}
randompoints<-data.frame(depth_km, estimates)

# Visualize our randomly generated potential estimates of abundance
baseline+
  geom_line(aes(x=depth_km, y=predicted), data=output.m1, size=1)+
  geom_point(aes(x=depth_km, y=estimates), col='grey50', alpha=0.5, data=randompoints)

# We see that the potential estimates based on this model do not
# adequately represent our real data; our real data are overdispersed

# ---- Poisson GLM: Conclusions ----
# But, we know that the model still isn't appropriate, based on the
# (a) heterogeneity of the residuals, and
# (b) overdispersion.

# ---- Adding a Factor ----
# It's possible that the issues we're seeing
# in our models are a result of a factor that is
# influencing the data but is not being included
# in the model.

# How might period influence the data?
pr.fac(m1, as.factor(fish$period), modeltype="poisson")

# We want to see residuals associated with each period
# are normally distributed about 0.
# This plot shows us that this isn't true for
# residuals associated with period 2.
# This suggets that we should include period as
# a factor in subsequent models.

# ---- Poisson GLM + Factor: Set Up ----
# First, let's create a variable that treats period
# as a factor rather than a number
fish$fperiod<-as.factor(fish$period)

# Run a generalized linear model (GLM) with a Poisson
# distribution and two interacting factors; depth and period
m2<-glm(abundance~depth_km*fperiod, data=fish, family="poisson")
m2 # Model output
summary(m2) # Summarize model output

# ---- Poisson GLM + Factor: Diagnostics ----
# Let's just check our dispersion
dispersion(m2, modeltype="poisson")
# Still too high, let's move on

# ---- Adding an Offset ----
# The area of each site differs - maybe this
# is the source of our problems! We can
# add it to offset or weigh the abundance values

# ---- Poisson GLM + Factor + Offset: Set Up ----
# First, let's create a variable of the log(area)
fish$larea<-log(fish$area)

# Run a generalized linear model (GLM) with a Poisson
# distribution, two interacting factors; depth and period,
# and the log(area) as an offset
m3<-glm(abundance~depth_km*fperiod+offset(larea), data=fish, family="poisson")
m3 # Model output
summary(m3) # Summarize model output

# ---- Poisson GLM + Factor + Offset: Diagnostics ----
# Dispersion
dispersion(m3, modeltype="poisson")
# Still too high. It's time to move away from the poisson
# distribution and onto a new model type.

# ---- NB GLM + Factor + Offset: Set Up ----
# Negative binomial GLMs add an adidtional parameter
# (theta) that accounts for the variance being greater
# than the mean (overdispersion).

# Run a generalized linear model (GLM) with a negative binomial
# distribution, two interacting factors; depth and period,
# and the log(area) as an offset
m4<-glm.nb(abundance~depth_km*fperiod+offset(larea), data=fish) # Notice the different function
m4 # Model output
summary(m4) # Summarize model output

# ---- NB GLM + Factor + Offset: Diagnostics ----
## Diagnostics 2. Dispersion ##
dispersion(m4) # Hooray, close to 1!

## Diagnostics 1. Residuals vs Fitted Values ##
# Create data frame of residuals and fitted values
diag.m4<-data.frame(residuals=resid(m4), fitted=fitted(m4))

# Visualize fitted values vs residuals
ggplot(diag.m4)+
  geom_point(aes(x=fitted, y=residuals), size=3)+
  geom_hline(yintercept=0, linetype="dashed", col="blue")+
  theme_bw(16)+ylab("Residuals")+xlab("Fitted Values)")

# Hooray, no pattern!

## Diagnostics 3: Additional Diagnostic Plots ##
plot(m4)

# Hooray, all looks decent!

# ---- NB GLM + Factor + Offset: Model Selection ----
# Now that we have our baseline model figured out, let's
# tweak it to make it as great as possible

# To look at which, if any, terms should be
# dropped from the model:
drop1(m4, test="Chi")

# This suggests that the interaction term between 
# depth_km and period isn't necessary to the model
# (p > 0.05) and can be dropped

# Create new model with interaction term dropped
# * indicates interaction, + indicates no interaction
m5<-glm.nb(abundance~depth_km+fperiod+offset(larea), data=fish)

# Check again for terms that should be dropped
drop1(m5, test="Chi")
# All are significant and should be kept

# Compare models using AIC
# A lower AIC indicates a better fit
AIC(m4,m5)
# These models are close enough that AIC doesn't
# really tell us much. But our previous diagnostics
# and drop1() gives us the information we need.

# ---- NB GLM + Factor + Offset: Fitted Lines ----
# We have our final model, m5

# Step 1: Create a data frame with independent variables
# (depth_km, fperiod, and larea)
# This is going to be more complicated than before because
# we need to set up a data frame that has every combination of
# each of the two variables

# depth_km: 20 values spanning the rane of depths sampled
# fperiod: 1 and 2, as factors
# larea: average larea

# The expand.grid() function will do it for you!
output.m5<-expand.grid(depth_km=seq(from=min(fish$depth_km),
                         to=max(fish$depth_km),
                         length=20),
            fperiod=as.factor(c(1,2)),
            larea=mean(fish$larea))

# Step 2: Populate a column of predictions based on the
# model m5 and the values given by our output.m5 data frame
output.m5$predicted<-predict(m5, output.m5, type='response')

# Step 3: Visualize the fitted model line
baseline+
  geom_line(aes(x=depth_km, y=predicted, col=fperiod), data=output.m5, size=1)

# Finally, add confidence limits around the predicted model lines
output.m5$fit<-predict(m5, output.m5, type="link", se=TRUE)$fit
output.m5$se<-predict(m5, output.m5, type="link", se=TRUE)$se

baseline+
  geom_line(aes(x=depth_km, y=predicted, col=fperiod), data=output.m5, size=1)+
  geom_line(aes(x=depth_km, y=exp(fit+se), col=fperiod), data=output.m5, size=1, linetype="dashed")+
  geom_line(aes(x=depth_km, y=exp(fit-se), col=fperiod), data=output.m5, size=1, linetype="dashed")
