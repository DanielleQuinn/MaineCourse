# ---- Description ----
# This is the BLANK code along script for
# the September 14th R Workshop at 
# University of Maine, presented by
# Dr. Trevor Avery and Danielle Quinn
# Enjoy!

# ---- R Basics: Calculator ----
# R follows order of operations
7*5+90/(10+2)

# ---- R Basics: Functions ----
# Functions are used to "do things" in R
 # This finds the square root of 81

# Functions usually only accept specific types of arguments

# Functions can be nested

# ---- R Basics: Packages ----
# Packages are stored in your local library
# You only need to install a package once

# You need to load installed packages each time you use R
library(ggplot2)
library(lubridate)
library(dplyr)

# ---- R Basics: Help Pages ----
 # Search loaded packages for sum()
 # Search installed packages for sum() and "sum"

# ---- Exercise 1.2 ----
# (A) Use R as a calculator

# (B) Practice some simple functions
# log()
# abs()

# (C) Install and load these packages
# ggplot2
# lubridate
# dplyr
# reshape2

# (D) Bring up the help page for mean()
?mean()

# ---- R Basics: Objects ----
# An object is a stored value or set of values

# Scalar Objects
# One dimensional, length of one
 # Built in scalar

 # Create our own scalar object

# Vector Objects
# One dimensional, length > one
 # built in vector

 # Create our own vector object
mycols<-c("red","blue","yellow","green")

# ---- R Basics: Data Classes ----
# Variables can have various classes
# Vectors can only contain one class ("atomic")
mycols
class(mycols) # What class is mycols?
as.factor(mycols) # View it as a factor

mycounts<-c(1,6,13,2,"about 5",2)
mean(mycounts) # Average mycounts
class(mycounts) # What class is mycounts?

# ---- R Basics: NAs ----
ncounts<-as.numeric(mycounts) # Convert mycounts to numeric and name it ncounts

mean(ncounts) # Average ncounts

mean(ncounts, na.rm=TRUE)  # Most functions have an argument specifically used to ignore NAs
 # Default na.rm=FALSE

# ---- Exercise 1.3 ----
# (A) Create a scalar called myname that contains your first name
myname<-"Danielle"

# (B) Create a vector called mybirthday that contains three numeric elements;
# your day, month, and year of birth
mybirthday<-c(17,01,1988)

# (C) Create a vector called test that contains both numeric and character
# elements. What does class() tell you about test?
test<-c(1,15,"a",5)
test

# (D) Convert test to a factor and call it ftest. Now convert ftest
# to a numeric object called ntest. What do you notice about ntest?
ftest<-as.factor(test)
ftest
ntest<-as.numeric(ftest)
ntest
ntest<-as.numeric(as.character(ftest))

# ---- R Basics: Indexing ----
# Positional attributes designated by []
# Used to query objects

# View long vector
1:100

# View shorter vector (mycols)

# View element in position 3

# View elements in positions 2 to 4

# View elements in positions 1, 3, and 4

# ---- R Basics: Conditional Indexing ----
# Are elements are equal to "blue"?

# == asks "does it equal?"
# = sets the value

# Which elements are equal to "blue"?

# "Hard coding"

# "Soft Coding"

# R understands that you want TRUE values only

# View elements equal to either "blue" or "green"

# View elements in mycols that are equal to either
# "blue" or "green"

# View elements in mycols that are not equal to "red"

# Create a vector called one_ten containing all
# integers from 1 to 10

# View elements greater than or equal to 8

# View elements less than 5 and greater than 2

# View elements less than 3 or greater than 9

# ---- Exercise 1.4 ----
# (A) Create an object called numvals consisting
# of a vector of numeric elements generated
# from a function
# seq()
# rnorm()
numvals<-seq(from=1, to=300, by=5)
numvals

# (B) Take a good look at your object
length(numvals)
# max()
# sort()
# unique()
# min()
# quantile()

# (C) Find all values of numvals greater than the average.
mean(numvals) # Find the average
# Hard Coding
numvals[numvals>148.5] # BAD
# Soft Coding
numvals[numvals>mean(numvals)] #GOOD

# (D) Find all values of numvals that are in the top 10%.
# Store these values in an object called topvals
# Hint: ?quantile()
quantile(numvals, probs=0.9)
topvals<-numvals[numvals>quantile(numvals, probs=0.9)]
topvals

# ---- Exercise Solutions ----
# Exercise 1.2
# (B)
log10(5)
abs(-4)

# (C)
install.packages(dplyr) # Install dplyr
install.packages(ggplot2) # Install ggplot2
install.packages(lubridate) # Install lubridate
install.packages(reshape2) # Install reshape2

library(dplyr) # Load dplyr
library(ggplot2) # Load ggplot2
library(lubridate) # Load lubridate
library(reshape2) # Load reshape2

# (D)
?mean()

# Exercise 1.3
# (A)
myname<-"Danielle"

# (B)
mybirthday<-c(17,1,1988)

# (C)
test<-c(1,2,"hello",3,"world")
class(test)

# (D)
ftest<-as.factor(test)
ntest<-as.numeric(ftest)
# ntest is a vector of numbers corresponding to factor levels, not actual values

# Exercise 1.4
# (A)
numvals<-seq(from=1, to=200, by=5)
# or
numvals<-rnorm(100, mean=5, sd=3)

# (B)
length(numvals) # How many elements in the vector?
min(numvals) # Minimum value
sort(numvals) # Sort values
unique(numvals) # Show only unique values
max(numvals) # Maximum value
quantile(numvals) # Sample quantiles of various probabilities

# (C)
numvals[numvals>mean(numvals)] # "Give me numvals where numvals is greater than the average of numvals"

# (D)
topvals<-numvals[numvals>quantile(numvals, 0.9)]
# "Give me numvals where numvals is greater than the 0.9 quantile of numvals"